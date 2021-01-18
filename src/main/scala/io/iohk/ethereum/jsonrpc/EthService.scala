package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.{ ByteString, Timeout }
import cats.syntax.either._
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status.Progress
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.{ BlockHeader, _ }
import io.iohk.ethereum.jsonrpc.AkkaTaskOps._
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.{ InMemoryWorldStateProxy, Ledger, StxLedger }
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPList
import monix.eval.Task

import java.util.Date
import java.util.concurrent.atomic.AtomicReference
import scala.collection.concurrent.{ TrieMap, Map => ConcurrentMap }
import scala.language.existentials
import scala.reflect.ClassTag

// scalastyle:off number.of.methods number.of.types file.size.limit
object EthService {

  case class ProtocolVersionRequest()
  case class ProtocolVersionResponse(value: String)

  case class SyncingRequest()
  case class SyncingStatus(
      startingBlock: BigInt,
      currentBlock: BigInt,
      highestBlock: BigInt,
      knownStates: BigInt,
      pulledStates: BigInt
  )
  case class SyncingResponse(syncStatus: Option[SyncingStatus])

  sealed trait BlockParam

  object BlockParam {
    case class WithNumber(n: BigInt) extends BlockParam
    case object Latest extends BlockParam
    case object Pending extends BlockParam
    case object Earliest extends BlockParam
  }

  case class CallTx(
      from: Option[ByteString],
      to: Option[ByteString],
      gas: Option[BigInt],
      gasPrice: BigInt,
      value: BigInt,
      data: ByteString
  )

  case class IeleCallTx(
      from: Option[ByteString],
      to: Option[ByteString],
      gas: Option[BigInt],
      gasPrice: BigInt,
      value: BigInt,
      function: Option[String] = None,
      arguments: Option[Seq[ByteString]] = None,
      contractCode: Option[ByteString]
  )

  case class CallRequest(tx: CallTx, block: BlockParam)
  case class CallResponse(returnData: ByteString)
  case class IeleCallRequest(tx: IeleCallTx, block: BlockParam)
  case class IeleCallResponse(returnData: Seq[ByteString])
  case class EstimateGasResponse(gas: BigInt)

  case class ResolvedBlock(block: Block, pendingState: Option[InMemoryWorldStateProxy])

  def resolveBlock(
      blockchain: Blockchain,
      ledger: Ledger,
      blockParam: BlockParam
  ): Either[JsonRpcError, ResolvedBlock] = {
    def getBlock(number: BigInt): Either[JsonRpcError, Block] = {
      blockchain
        .getBlockByNumber(number)
        .map(Right.apply)
        .getOrElse(Left(JsonRpcError.InvalidParams(s"Block $number not found")))
    }

    blockParam match {
      case BlockParam.WithNumber(blockNumber) => getBlock(blockNumber).map(ResolvedBlock(_, pendingState = None))
      case BlockParam.Earliest => getBlock(0).map(ResolvedBlock(_, pendingState = None))
      case BlockParam.Latest => getBlock(blockchain.getBestBlockNumber()).map(ResolvedBlock(_, pendingState = None))
      case BlockParam.Pending =>
        ledger.consensus.blockGenerator.getPendingBlockAndState
          .map(pb => ResolvedBlock(pb.pendingBlock.block, pendingState = Some(pb.worldState)))
          .map(Right.apply)
          .getOrElse(resolveBlock(blockchain, ledger, BlockParam.Latest)) //Default behavior in other clients
    }
  }
}

class EthService(
    blockchain: Blockchain,
    ledger: Ledger,
    stxLedger: StxLedger,
    keyStore: KeyStore,
    syncingController: ActorRef,
    protocolVersion: Int,
    askTimeout: Timeout
) {

  import EthService._

  val hashRate: ConcurrentMap[ByteString, (BigInt, Date)] = new TrieMap[ByteString, (BigInt, Date)]()
  val lastActive = new AtomicReference[Option[Date]](None)

  def protocolVersion(req: ProtocolVersionRequest): ServiceResponse[ProtocolVersionResponse] =
    Task.now(Right(ProtocolVersionResponse(f"0x$protocolVersion%x")))

  /**
    * Implements the eth_syncing method that returns syncing information if the node is syncing.
    *
    * @return The syncing status if the node is syncing or None if not
    */
  def syncing(req: SyncingRequest): ServiceResponse[SyncingResponse] =
    syncingController
      .askFor(SyncProtocol.GetStatus)(timeout = askTimeout, implicitly[ClassTag[SyncProtocol.Status]])
      .map {
        case Status.Syncing(startingBlockNumber, blocksProgress, maybeStateNodesProgress) =>
          val stateNodesProgress = maybeStateNodesProgress.getOrElse(Progress.empty)
          SyncingResponse(
            Some(
              SyncingStatus(
                startingBlock = startingBlockNumber,
                currentBlock = blocksProgress.current,
                highestBlock = blocksProgress.target,
                knownStates = stateNodesProgress.target,
                pulledStates = stateNodesProgress.current
              )
            )
          )
        case Status.NotSyncing => SyncingResponse(None)
        case Status.SyncDone => SyncingResponse(None)
      }
      .map(_.asRight)

  def call(req: CallRequest): ServiceResponse[CallResponse] = {
    Task {
      doCall(req)(stxLedger.simulateTransaction).map(r => CallResponse(r.vmReturnData))
    }
  }

  def ieleCall(req: IeleCallRequest): ServiceResponse[IeleCallResponse] = {
    import req.tx

    val args = tx.arguments.getOrElse(Nil)
    val dataEither = (tx.function, tx.contractCode) match {
      case (Some(function), None) => Right(rlp.encode(RLPList(function, args)))
      case (None, Some(contractCode)) => Right(rlp.encode(RLPList(contractCode, args)))
      case _ => Left(JsonRpcError.InvalidParams("Iele transaction should contain either functionName or contractCode"))
    }

    dataEither match {
      case Right(data) =>
        call(CallRequest(CallTx(tx.from, tx.to, tx.gas, tx.gasPrice, tx.value, ByteString(data)), req.block))
          .map(_.map { callResponse =>
            IeleCallResponse(
              rlp.decode[Seq[ByteString]](callResponse.returnData.toArray[Byte])(seqEncDec[ByteString]())
            )
          })
      case Left(error) => Task.now(Left(error))
    }
  }

  def estimateGas(req: CallRequest): ServiceResponse[EstimateGasResponse] = {
    Task {
      doCall(req)(stxLedger.binarySearchGasEstimation).map(gasUsed => EstimateGasResponse(gasUsed))
    }
  }

  private def doCall[A](req: CallRequest)(
      f: (SignedTransactionWithSender, BlockHeader, Option[InMemoryWorldStateProxy]) => A
  ): Either[JsonRpcError, A] = for {
    stx <- prepareTransaction(req)
    block <- resolveBlock(blockchain, ledger, req.block)
  } yield f(stx, block.block.header, block.pendingState)

  private def getGasLimit(req: CallRequest): Either[JsonRpcError, BigInt] =
    if (req.tx.gas.isDefined) Right[JsonRpcError, BigInt](req.tx.gas.get)
    else resolveBlock(blockchain, ledger, BlockParam.Latest).map(r => r.block.header.gasLimit)

  private def prepareTransaction(req: CallRequest): Either[JsonRpcError, SignedTransactionWithSender] = {
    getGasLimit(req).map { gasLimit =>
      val fromAddress = req.tx.from
        .map(Address.apply) // `from` param, if specified
        .getOrElse(
          keyStore
            .listAccounts()
            .getOrElse(Nil)
            .headOption // first account, if exists and `from` param not specified
            .getOrElse(Address(0))
        ) // 0x0 default

      val toAddress = req.tx.to.map(Address.apply)

      val tx = Transaction(0, req.tx.gasPrice, gasLimit, toAddress, req.tx.value, req.tx.data)
      val fakeSignature = ECDSASignature(0, 0, 0.toByte)
      SignedTransactionWithSender(tx, fakeSignature, fromAddress)
    }
  }

}
