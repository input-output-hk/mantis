package io.iohk.ethereum.consensus
package atomixraft

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import io.iohk.ethereum.blockchain.sync.RegularSync
import io.iohk.ethereum.consensus.atomixraft.AtomixRaftMiner._
import io.iohk.ethereum.consensus.atomixraft.blocks.AtomixRaftBlockGenerator
import io.iohk.ethereum.consensus.blocks.PendingBlock
import io.iohk.ethereum.domain.{Address, Block, Blockchain}
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class AtomixRaftMiner(
  blockchain: Blockchain,
  pendingTransactionsManager: ActorRef,
  syncController: ActorRef,
  consensus: AtomixRaftConsensus
) extends Actor with ActorLogging {

  def receive: Receive = stopped

  private def consensusCofig: ConsensusConfig = consensus.config.generic
  private def coinbase: Address = consensusCofig.coinbase
  private def isLeader: Boolean = consensus.isLeader.getOrElse(false)
  private def blockGenerator: AtomixRaftBlockGenerator = consensus.blockGenerator

  private def scheduleOnce(delay: FiniteDuration, msg: Msg): Unit =
    context.system.scheduler.scheduleOnce(delay, self, msg)

  private def stopped: Receive = {
    case Init ⇒
      log.info("***** EthashMiner initialized")

    case IAmTheLeader ⇒
      log.info("***** I am the leader, will start mining")
      context become mining
      self ! StartMining
  }

  private def mining: Receive = {
    case StopMining ⇒ context become stopped
    case StartMining ⇒ startMining()
  }

  private def lostLeadership(): Unit = {
    log.info("***** Ouch, lost leadership")
    self ! StopMining
  }

  private def startMining(): Unit = {
    if(isLeader) {
      val parentBlock = blockchain.getBestBlock()

      getBlockForMining(parentBlock) onComplete {
        case Success(PendingBlock(block, _)) ⇒
          syncTheBlock(block)

        case Failure(ex) ⇒
          log.error(ex, "Unable to get block for mining")
          scheduleOnce(10.seconds, StartMining)
      }
    }
    else {
      lostLeadership()
    }
  }

  private def syncTheBlock(block: Block): Unit = {
    if(isLeader) {
      log.info("***** Mined block " + block.header.number)
      syncController ! RegularSync.MinedBlock(block)
      self ! StartMining
    }
    else {
      lostLeadership()
    }
  }

  //noinspection ScalaStyle
  private def getBlockForMining(parentBlock: Block): Future[PendingBlock] = {
    Thread.sleep(AtomixRaftMiner.ArtificialDelay)

    val ffPendingBlock: Future[Future[PendingBlock]] =
      for {
        pendingTxResponse ← getTransactionsFromPool
      } yield {
        val pendingTransactions = pendingTxResponse.pendingTransactions.map(_.stx)

        val errorOrPendingBlock = blockGenerator.generateBlockForMining(
          parent = parentBlock,
          transactions = pendingTransactions,
          beneficiary = coinbase,
          ommers = Nil // No ommers
        )
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
    implicit val timeout: Timeout = consensusCofig.getTransactionFromPoolTimeout

    (pendingTransactionsManager ? PendingTransactionsManager.GetPendingTransactions).mapTo[PendingTransactionsResponse]
      .recover { case ex =>
        log.error(ex, "Failed to get transactions, mining block with empty transactions list")
        PendingTransactionsResponse(Nil)
      }
  }
}

object AtomixRaftMiner {
  final val ArtificialDelay = 3001 // FIXME Delete

  sealed trait Msg
  case object Init extends Msg
  case object IAmTheLeader extends Msg
  case object StartMining extends Msg
  case object StopMining extends Msg

  private def props(
    blockchain: Blockchain,
    pendingTransactionsManager: ActorRef,
    syncController: ActorRef,
    consensus: AtomixRaftConsensus
  ): Props =
    Props(
      new AtomixRaftMiner(blockchain, pendingTransactionsManager, syncController, consensus)
    )

  private[atomixraft] def apply(node: Node): ActorRef = {
    node.consensus match {
      case consensus: AtomixRaftConsensus ⇒
        val minerProps = props(
          blockchain = node.blockchain,
          pendingTransactionsManager = node.pendingTransactionsManager,
          syncController = node.syncController,
          consensus = consensus
        )

        node.actorSystem.actorOf(minerProps)

      case consensus ⇒
        wrongConsensusArgument[AtomixRaftConsensus](consensus)
    }
  }
}
