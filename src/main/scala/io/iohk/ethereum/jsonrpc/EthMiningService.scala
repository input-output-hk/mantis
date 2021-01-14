package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.blocks.PendingBlockAndState
import io.iohk.ethereum.consensus.ethash.EthashUtils
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{Address, BlockHeader, Blockchain}
import io.iohk.ethereum.jsonrpc.AkkaTaskOps._
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.utils.Logger
import monix.eval.Task

import java.time.Duration
import java.util.Date
import java.util.concurrent.atomic.AtomicReference
import scala.collection.concurrent.{TrieMap, Map => ConcurrentMap}
import scala.concurrent.duration.FiniteDuration
import scala.language.existentials

object EthMiningService {

  case class GetMiningRequest()
  case class GetMiningResponse(isMining: Boolean)

  case class GetWorkRequest()
  case class GetWorkResponse(powHeaderHash: ByteString, dagSeed: ByteString, target: ByteString)

  case class SubmitWorkRequest(nonce: ByteString, powHeaderHash: ByteString, mixHash: ByteString)
  case class SubmitWorkResponse(success: Boolean)

  case class GetCoinbaseRequest()
  case class GetCoinbaseResponse(address: Address)

  case class SubmitHashRateRequest(hashRate: BigInt, id: ByteString)
  case class SubmitHashRateResponse(success: Boolean)

  case class GetHashRateRequest()
  case class GetHashRateResponse(hashRate: BigInt)
}

class EthMiningService(
    blockchain: Blockchain,
    ledger: Ledger,
    jsonRpcConfig: JsonRpcConfig,
    ommersPool: ActorRef,
    syncingController: ActorRef,
    pendingTransactionsManager: ActorRef,
    getTransactionFromPoolTimeout: FiniteDuration
) extends Logger {

  import EthMiningService._

  private[this] def consensus = ledger.consensus
  private[this] def fullConsensusConfig = consensus.config
  private[this] def consensusConfig: ConsensusConfig = fullConsensusConfig.generic

  val hashRate: ConcurrentMap[ByteString, (BigInt, Date)] = new TrieMap[ByteString, (BigInt, Date)]()
  val lastActive = new AtomicReference[Option[Date]](None)

  def getMining(req: GetMiningRequest): ServiceResponse[GetMiningResponse] =
    ifEthash(req) { _ =>
      val isMining = lastActive
        .updateAndGet((e: Option[Date]) => {
          e.filter { time =>
            Duration.between(time.toInstant, (new Date).toInstant).toMillis < jsonRpcConfig.minerActiveTimeout.toMillis
          }
        })
        .isDefined

      GetMiningResponse(isMining)
    }

  def getWork(req: GetWorkRequest): ServiceResponse[GetWorkResponse] =
    consensus.ifEthash(ethash => {
      reportActive()
      val bestBlock = blockchain.getBestBlock()
      val response: ServiceResponse[GetWorkResponse] =
        Task.parZip2(getOmmersFromPool(bestBlock.hash), getTransactionsFromPool).map { case (ommers, pendingTxs) =>
          val blockGenerator = ethash.blockGenerator
          val PendingBlockAndState(pb, _) = blockGenerator.generateBlock(
            bestBlock,
            pendingTxs.pendingTransactions.map(_.stx.tx),
            consensusConfig.coinbase,
            ommers.headers,
            None
          )
          Right(
            GetWorkResponse(
              powHeaderHash = ByteString(kec256(BlockHeader.getEncodedWithoutNonce(pb.block.header))),
              dagSeed = EthashUtils.seed(pb.block.header.number.toLong),
              target = ByteString((BigInt(2).pow(256) / pb.block.header.difficulty).toByteArray)
            )
          )
        }
      response
    })(Task.now(Left(JsonRpcError.ConsensusIsNotEthash)))

  def submitWork(req: SubmitWorkRequest): ServiceResponse[SubmitWorkResponse] =
    consensus.ifEthash[ServiceResponse[SubmitWorkResponse]](ethash => {
      reportActive()
      Task {
        ethash.blockGenerator.getPrepared(req.powHeaderHash) match {
          case Some(pendingBlock) if blockchain.getBestBlockNumber() <= pendingBlock.block.header.number =>
            import pendingBlock._
            syncingController ! SyncProtocol.MinedBlock(
              block.copy(header = block.header.copy(nonce = req.nonce, mixHash = req.mixHash))
            )
            Right(SubmitWorkResponse(true))
          case _ =>
            Right(SubmitWorkResponse(false))
        }
      }
    })(Task.now(Left(JsonRpcError.ConsensusIsNotEthash)))

  def getCoinbase(req: GetCoinbaseRequest): ServiceResponse[GetCoinbaseResponse] =
    Task.now(Right(GetCoinbaseResponse(consensusConfig.coinbase)))

  def submitHashRate(req: SubmitHashRateRequest): ServiceResponse[SubmitHashRateResponse] =
    ifEthash(req) { req =>
      reportActive()
      val now = new Date
      removeObsoleteHashrates(now)
      hashRate.put(req.id, (req.hashRate -> now))
      SubmitHashRateResponse(true)
    }

  def getHashRate(req: GetHashRateRequest): ServiceResponse[GetHashRateResponse] =
    ifEthash(req) { _ =>
      removeObsoleteHashrates(new Date)
      //sum all reported hashRates
      GetHashRateResponse(hashRate.map { case (_, (hr, _)) => hr }.sum)
    }

  // NOTE This is called from places that guarantee we are running Ethash consensus.
  private def removeObsoleteHashrates(now: Date): Unit = {
    hashRate.filterInPlace { case (_, (_, reported)) =>
      Duration.between(reported.toInstant, now.toInstant).toMillis < jsonRpcConfig.minerActiveTimeout.toMillis
    }
  }

  private def reportActive(): Option[Date] = {
    val now = new Date()
    lastActive.updateAndGet(_ => Some(now))
  }

  private def getOmmersFromPool(parentBlockHash: ByteString): Task[OmmersPool.Ommers] =
    consensus.ifEthash(ethash => {
      val miningConfig = ethash.config.specific
      implicit val timeout: Timeout = Timeout(miningConfig.ommerPoolQueryTimeout)

      ommersPool
        .askFor[OmmersPool.Ommers](OmmersPool.GetOmmers(parentBlockHash))
        .onErrorHandle { ex =>
          log.error("failed to get ommer, mining block with empty ommers list", ex)
          OmmersPool.Ommers(Nil)
        }
    })(Task.now(OmmersPool.Ommers(Nil))) // NOTE If not Ethash consensus, ommers do not make sense, so => Nil

  // TODO This seems to be re-implemented in TransactionPicker, probably move to a better place? Also generalize the error message.
  private[jsonrpc] val getTransactionsFromPool: Task[PendingTransactionsResponse] = {
    implicit val timeout: Timeout = Timeout(getTransactionFromPoolTimeout)

    pendingTransactionsManager
      .askFor[PendingTransactionsResponse](PendingTransactionsManager.GetPendingTransactions)
      .onErrorRecoverWith { case ex: Throwable =>
        log.error("Failed to get pending transactions, passing empty transactions list", ex)
        Task.now(PendingTransactionsResponse(Nil))
      }
  }

  private[jsonrpc] def ifEthash[Req, Res](req: Req)(f: Req => Res): ServiceResponse[Res] = {
    consensus.ifEthash[ServiceResponse[Res]](_ => Task.now(Right(f(req))))(
      Task.now(Left(JsonRpcError.ConsensusIsNotEthash))
    )
  }
}
