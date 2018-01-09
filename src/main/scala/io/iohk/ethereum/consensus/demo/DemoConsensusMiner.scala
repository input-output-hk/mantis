package io.iohk.ethereum.consensus
package demo

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.sync.RegularSync
import io.iohk.ethereum.consensus.demo.Demo.ProofOfWork
import io.iohk.ethereum.domain.{Block, Blockchain}
import io.iohk.ethereum.jsonrpc.EthService
import io.iohk.ethereum.mining.{BlockGenerator, PendingBlock}
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class DemoConsensusMiner(
  blockchain: Blockchain,
  blockGenerator: BlockGenerator,
  ommersPool: ActorRef,
  pendingTransactionsManager: ActorRef,
  syncController: ActorRef,
  demoConsensusConfig: DemoConsensusConfig,
  ethService: EthService,
  consensus: Consensus
) extends Actor with ActorLogging {

  import DemoConsensusMiner._
  import akka.pattern.ask

  var currentEpoch: Option[Long] = None
  var currentEpochDagSize: Option[Long] = None
  var currentEpochDag: Option[Array[Array[Int]]] = None

  private[this] val IAmTheLeader = demoConsensusConfig.IAmTheLeader

  override def receive: Receive = stopped

  def stopped: Receive = {
    case StartMining =>
      context become started
      self ! ProcessMining
    case ProcessMining => // nothing
  }

  def started: Receive = {
    case StopMining => context become stopped
    case ProcessMining => processMining()
    case UpdateAndSyncTheBlock(block) => updateAndSyncTheBlock(block)
  }

  private def syncTheBlock(block: Block): Unit = {
    syncController ! RegularSync.MinedBlock(block)
    self ! ProcessMining
  }

  private def updateAndSyncTheBlock(block: Block): Unit = {
    val header = block.header
    val blockTime = header.unixTimestamp
    val now = blockGenerator.blockTimestampProvider.getEpochSecond

    require(now > blockTime)

    val newHeader = header.copy(unixTimestamp = now)
    val newBlock = block.copy(header = newHeader)

    log.info("***** Syncing the block")
    syncTheBlock(newBlock)
  }

  private def processMining(): Unit = {
    if(IAmTheLeader) {
      log.info("***** I am the leader, going to mine a block ...")
      val initialTime = blockGenerator.blockTimestampProvider.getEpochSecond
      val parentBlock = blockchain.getBestBlock()

      getBlockForMining(parentBlock) onComplete {
        case Success(PendingBlock(block, _)) =>
          val blockTime = block.header.unixTimestamp
          // Just to make sure we pass the relevant validation
          // FIXME This needs to be generalized (move all relevant validation to the consensus implementation)
          if(blockTime > initialTime) {
            syncTheBlock(block)
          }
          else {
            val delay = 5.seconds
            log.warning(s"***** Block mined too soon, inserting delay of ~ $delay")
            context.system.scheduler.scheduleOnce(delay, self, UpdateAndSyncTheBlock(block))
          }

        case Failure(ex) =>
          log.error(ex, "Unable to get block for mining")
          context.system.scheduler.scheduleOnce(10.seconds, self, ProcessMining)
      }
    }
    else {
      // Hoping to become the leader ...
      log.info("***** I am not the leader, hoping to become one in the future ...")
      context.system.scheduler.scheduleOnce(5.seconds, self, ProcessMining)
    }
  }

  private def getBlockForMining(parentBlock: Block): Future[PendingBlock] = {
    val ffPendingBlock: Future[Future[PendingBlock]] =
      for {
        pendingTxResponse ← getTransactionsFromPool
      } yield {
        val pendingTransactions = pendingTxResponse.pendingTransactions.map(_.stx)

        val errorOrPendingBlock = blockGenerator.generateBlockForMining(parentBlock, pendingTransactions, Nil, demoConsensusConfig.coinbase)
        errorOrPendingBlock match {
          case Left(error) ⇒
            Future.failed(new RuntimeException(s"Error while generating block for mining: $error"))

          case Right(pendingBlock) ⇒
            Future.successful(pendingBlock)
        }
      }

    ffPendingBlock.flatten
  }

  private def getTransactionsFromPool = {
    // FIXME Rename this since we do not use ommers?
    implicit val timeout = Timeout(demoConsensusConfig.ommerPoolQueryTimeout)

    (pendingTransactionsManager ? PendingTransactionsManager.GetPendingTransactions).mapTo[PendingTransactionsResponse]
      .recover { case ex =>
        log.error(ex, "Failed to get transactions, mining block with empty transactions list")
        PendingTransactionsResponse(Nil)
      }
  }
}

object DemoConsensusMiner {
  def props(blockchain: Blockchain,
    blockGenerator: BlockGenerator,
    ommersPool: ActorRef,
    pendingTransactionsManager: ActorRef,
    syncController: ActorRef,
    miningConfig: DemoConsensusConfig,
    ethService: EthService,
    consensus: Consensus
  ): Props =
    Props(new DemoConsensusMiner(blockchain, blockGenerator, ommersPool,
      pendingTransactionsManager, syncController, miningConfig, ethService, consensus))

  case object StartMining
  case object StopMining

  private case object ProcessMining
  private case class UpdateAndSyncTheBlock(block: Block) // FIXME this is just to ensure header validation succeeds

  // scalastyle:off magic.number
  val MaxNonce: BigInt = BigInt(2).pow(64) - 1

  val DagFilePrefix: ByteString = ByteString(Array(0xfe, 0xca, 0xdd, 0xba, 0xad, 0xde, 0xe1, 0xfe).map(_.toByte))

  sealed trait MiningResult {
    def triedHashes: Int
  }

  case class MiningSuccessful(triedHashes: Int, pow: ProofOfWork, nonce: ByteString) extends MiningResult
  case class MiningUnsuccessful(triedHashes: Int) extends MiningResult

}

