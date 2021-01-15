package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.{ByteString, Timeout}
import cats.syntax.either._
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status.Progress
import io.iohk.ethereum.consensus.blocks.BlockGenerator
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import io.iohk.ethereum.domain.{BlockHeader, SignedTransaction, _}
import io.iohk.ethereum.jsonrpc.AkkaTaskOps._
import io.iohk.ethereum.jsonrpc.FilterManager.{FilterChanges, FilterLogs, LogFilterLogs}
import io.iohk.ethereum.jsonrpc.{FilterManager => FM}
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, Ledger, StxLedger}
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.{PendingTransaction, PendingTransactionsResponse}
import io.iohk.ethereum.utils._
import monix.eval.Task

import java.util.Date
import java.util.concurrent.atomic.AtomicReference
import scala.collection.concurrent.{TrieMap, Map => ConcurrentMap}
import scala.concurrent.duration.FiniteDuration
import scala.language.existentials
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import io.iohk.ethereum.transactions.TransactionPicker

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

  // case class NewFilterRequest(filter: Filter)
  // case class Filter(
  //     fromBlock: Option[BlockParam],
  //     toBlock: Option[BlockParam],
  //     address: Option[Address],
  //     topics: Seq[Seq[ByteString]]
  // )

  // case class NewBlockFilterRequest()
  // case class NewPendingTransactionFilterRequest()

  // case class NewFilterResponse(filterId: BigInt)

  // case class UninstallFilterRequest(filterId: BigInt)
  // case class UninstallFilterResponse(success: Boolean)

  // case class GetFilterChangesRequest(filterId: BigInt)
  // case class GetFilterChangesResponse(filterChanges: FilterChanges)

  // case class GetFilterLogsRequest(filterId: BigInt)
  // case class GetFilterLogsResponse(filterLogs: FilterLogs)

  // case class GetLogsRequest(filter: Filter)
  // case class GetLogsResponse(filterLogs: LogFilterLogs)
  // case class GetUncleCountByBlockNumberRequest(block: BlockParam)
  // case class GetUncleCountByBlockNumberResponse(result: BigInt)

  // case class GetUncleCountByBlockHashRequest(blockHash: ByteString)
  // case class GetUncleCountByBlockHashResponse(result: BigInt)

  // case class GetCoinbaseRequest()
  // case class GetCoinbaseResponse(address: Address)

  // case class GetBlockTransactionCountByNumberRequest(block: BlockParam)
  // case class GetBlockTransactionCountByNumberResponse(result: BigInt)

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

  def resolveBlock(
      blockParam: BlockParam,
      blockchain: Blockchain,
      blockGenerator: BlockGenerator
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
        blockGenerator.getPendingBlockAndState
          .map(pb => ResolvedBlock(pb.pendingBlock.block, pendingState = Some(pb.worldState)))
          .map(Right.apply)
          .getOrElse(resolveBlock(BlockParam.Latest, blockchain, blockGenerator)) //Default behavior in other clients
    }
  }
}

class EthService(
    blockchain: Blockchain,
    ledger: Ledger,
    stxLedger: StxLedger,
    keyStore: KeyStore,
    val pendingTransactionsManager: ActorRef,
    syncingController: ActorRef,
    filterManager: ActorRef,
    filterConfig: FilterConfig,
    blockchainConfig: BlockchainConfig,
    protocolVersion: Int,
    val getTransactionFromPoolTimeout: FiniteDuration,
    askTimeout: Timeout
) extends TransactionPicker {

  import EthService._

  val hashRate: ConcurrentMap[ByteString, (BigInt, Date)] = new TrieMap[ByteString, (BigInt, Date)]()
  val lastActive = new AtomicReference[Option[Date]](None)

  private[this] def consensus = ledger.consensus
  private[this] def blockGenerator: BlockGenerator = consensus.blockGenerator
  private[this] def fullConsensusConfig = consensus.config

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

  // def newFilter(req: NewFilterRequest): ServiceResponse[NewFilterResponse] = {
  //   implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)

  //   import req.filter._
  //   filterManager
  //     .askFor[FM.NewFilterResponse](FM.NewLogFilter(fromBlock, toBlock, address, topics))
  //     .map { resp =>
  //       Right(NewFilterResponse(resp.id))
  //     }
  // }

  // def newBlockFilter(req: NewBlockFilterRequest): ServiceResponse[NewFilterResponse] = {
  //   implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)
  //   filterManager
  //     .askFor[FM.NewFilterResponse](FM.NewBlockFilter)
  //     .map { resp =>
  //       Right(NewFilterResponse(resp.id))
  //     }
  // }

  // def newPendingTransactionFilter(req: NewPendingTransactionFilterRequest): ServiceResponse[NewFilterResponse] = {
  //   implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)
  //   filterManager
  //     .askFor[FM.NewFilterResponse](FM.NewPendingTransactionFilter)
  //     .map { resp =>
  //       Right(NewFilterResponse(resp.id))
  //     }
  // }

  // def uninstallFilter(req: UninstallFilterRequest): ServiceResponse[UninstallFilterResponse] = {
  //   implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)

  //   filterManager
  //     .askFor[FM.UninstallFilterResponse.type](FM.UninstallFilter(req.filterId))
  //     .map(_ => Right(UninstallFilterResponse(success = true)))
  // }

  // def getFilterChanges(req: GetFilterChangesRequest): ServiceResponse[GetFilterChangesResponse] = {
  //   implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)

  //   filterManager
  //     .askFor[FM.FilterChanges](FM.GetFilterChanges(req.filterId))
  //     .map { filterChanges =>
  //       Right(GetFilterChangesResponse(filterChanges))
  //     }
  // }

  // def getFilterLogs(req: GetFilterLogsRequest): ServiceResponse[GetFilterLogsResponse] = {
  //   implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)
  //   filterManager
  //     .askFor[FM.FilterLogs](FM.GetFilterLogs(req.filterId))
  //     .map { filterLogs =>
  //       Right(GetFilterLogsResponse(filterLogs))
  //     }
  // }

  // def getLogs(req: GetLogsRequest): ServiceResponse[GetLogsResponse] = {
  //   implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)
  //   import req.filter._

  //   filterManager
  //     .askFor[FM.LogFilterLogs](FM.GetLogs(fromBlock, toBlock, address, topics))
  //     .map { filterLogs =>
  //       Right(GetLogsResponse(filterLogs))
  //     }
  // def getUncleCountByBlockNumber(
  //     req: GetUncleCountByBlockNumberRequest
  // ): ServiceResponse[GetUncleCountByBlockNumberResponse] = {
  //   Task {
  //     resolveBlock(blockchain, ledger, req.block).map { case ResolvedBlock(block, _) =>
  //       GetUncleCountByBlockNumberResponse(block.body.uncleNodesList.size)
  //     }
  //   }
  // }

  // def getUncleCountByBlockHash(
  //     req: GetUncleCountByBlockHashRequest
  // ): ServiceResponse[GetUncleCountByBlockHashResponse] = {
  //   Task {
  //     blockchain.getBlockBodyByHash(req.blockHash) match {
  //       case Some(blockBody) =>
  //         Right(GetUncleCountByBlockHashResponse(blockBody.uncleNodesList.size))
  //       case None =>
  //         Left(
  //           JsonRpcError.InvalidParams(s"Block with hash ${Hex.toHexString(req.blockHash.toArray[Byte])} not found")
  //         )
  //     }
  //   }
  // }

  // def getBlockTransactionCountByNumber(
  //     req: GetBlockTransactionCountByNumberRequest
  // ): ServiceResponse[GetBlockTransactionCountByNumberResponse] = {
  //   Task {
  //     resolveBlock(blockchain, ledger, req.block).map { case ResolvedBlock(block, _) =>
  //       GetBlockTransactionCountByNumberResponse(block.body.transactionList.size)
  //     }
  //   }
  // }

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
