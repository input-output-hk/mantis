package io.iohk.ethereum.consensus
package atomixraft

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import io.iohk.ethereum.blockchain.sync.regular.RegularSync
import io.iohk.ethereum.consensus.atomixraft.AtomixRaftForger._
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

class AtomixRaftForger(
  blockchain: Blockchain,
  pendingTransactionsManager: ActorRef,
  syncController: ActorRef,
  consensus: AtomixRaftConsensus,
  getTransactionFromPoolTimeout: FiniteDuration
) extends Actor with ActorLogging {

  def receive: Receive = stopped

  private def consensusConfig: ConsensusConfig = consensus.config.generic
  private def atomixRaftConfig: AtomixRaftConfig = consensus.config.specific
  private def coinbase: Address = consensusConfig.coinbase
  private def isLeader: Boolean = consensus.isLeader.getOrElse(false)
  private def blockGenerator: AtomixRaftBlockGenerator = consensus.blockGenerator

  private def scheduleOnce(delay: FiniteDuration, msg: Msg): Unit =
    context.system.scheduler.scheduleOnce(delay, self, msg)

  private def stopped: Receive = {
    case Init ⇒
      log.info("***** Forger initialized")

    case IAmTheLeader ⇒
      log.info("***** I am the leader, will start forging blocks")
      context become forging
      self ! StartForging
  }

  private def forging: Receive = {
    case StopForging ⇒ context become stopped
    case StartForging ⇒ startForging()
  }

  private def lostLeadership(): Unit = {
    log.info("***** Ouch, lost leadership")
    self ! StopForging
  }

  private def startForging(): Unit = {
    if(isLeader) {
      val parentBlock = blockchain.getBestBlock()

      getBlock(parentBlock) onComplete {
        case Success(PendingBlock(block, _)) ⇒
          syncTheBlock(block)

        case Failure(ex) ⇒
          log.error(ex, "Unable to get block")
          scheduleOnce(atomixRaftConfig.blockForgingDelay, StartForging)
      }
    }
    else {
      lostLeadership()
    }
  }

  private def syncTheBlock(block: Block): Unit = {
    if(isLeader) {
      log.info("***** Forged block " + block.header.number)
      syncController ! RegularSync.MinedBlock(block)
      scheduleOnce(atomixRaftConfig.blockForgingDelay, StartForging)
    }
    else {
      lostLeadership()
    }
  }

  private def getBlock(parentBlock: Block): Future[PendingBlock] = {
    val ffPendingBlock: Future[Future[PendingBlock]] =
      for {
        pendingTxResponse ← getTransactionsFromPool
      } yield {
        val pendingTransactions = pendingTxResponse.pendingTransactions.map(_.stx)

        val errorOrPendingBlock = blockGenerator.generateBlock(
          parent = parentBlock,
          transactions = pendingTransactions,
          beneficiary = coinbase,
          x = Nil
        )
        errorOrPendingBlock match {
          case Left(error) ⇒
            Future.failed(new RuntimeException(s"Error while generating block: $error"))

          case Right(pendingBlock) ⇒
            Future.successful(pendingBlock)
        }
      }

    ffPendingBlock.flatten
  }

  private def getTransactionsFromPool = {
    implicit val timeout: Timeout = getTransactionFromPoolTimeout

    (pendingTransactionsManager ? PendingTransactionsManager.GetPendingTransactions).mapTo[PendingTransactionsResponse]
      .recover { case ex =>
        log.error(ex, "Failed to get transactions, forging block with empty transactions list")
        PendingTransactionsResponse(Nil)
      }
  }
}

object AtomixRaftForger {
  sealed trait Msg
  case object Init extends Msg
  case object IAmTheLeader extends Msg
  case object StartForging extends Msg
  case object StopForging extends Msg

  private def props(
    blockchain: Blockchain,
    pendingTransactionsManager: ActorRef,
    syncController: ActorRef,
    consensus: AtomixRaftConsensus,
    getTransactionFromPoolTimeout: FiniteDuration
  ): Props =
    Props(
      new AtomixRaftForger(
        blockchain, pendingTransactionsManager, syncController, consensus,
        getTransactionFromPoolTimeout
      )
    )

  private[atomixraft] def apply(node: Node): ActorRef = {
    node.consensus match {
      case consensus: AtomixRaftConsensus ⇒
        val minerProps = props(
          blockchain = node.blockchain,
          pendingTransactionsManager = node.pendingTransactionsManager,
          syncController = node.syncController,
          consensus = consensus,
          getTransactionFromPoolTimeout = node.txPoolConfig.getTransactionFromPoolTimeout
        )

        node.system.actorOf(minerProps)

      case consensus ⇒
        wrongConsensusArgument[AtomixRaftConsensus](consensus)
    }
  }
}
