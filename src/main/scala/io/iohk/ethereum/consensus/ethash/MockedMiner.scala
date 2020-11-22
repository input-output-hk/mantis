package io.iohk.ethereum.consensus.ethash

import akka.actor.Status.Failure
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.consensus.blocks.PendingBlock
import io.iohk.ethereum.consensus.ethash.MinerProtocol.{StartMining, StopMining}
import io.iohk.ethereum.consensus.ethash.MinerResponses.{MinerIsWorking, MiningError, MiningOrdered}
import io.iohk.ethereum.consensus.ethash.MockedMiner.MineBlock
import io.iohk.ethereum.consensus.ethash.MockedMinerProtocol.MineBlocks
import io.iohk.ethereum.consensus.wrongConsensusArgument
import io.iohk.ethereum.domain.{Block, Blockchain}
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.ByteStringUtils

import scala.concurrent.duration._

class MockedMiner(
    blockchain: Blockchain,
    blockCreator: EthashBlockCreator,
    syncEventListener: ActorRef
) extends Actor
    with ActorLogging
    with MinerUtils {
  import akka.pattern.pipe
  import context.dispatcher

  override def receive: Receive = stopped

  def stopped: Receive = notSupportedMockedMinerMessages orElse { case StartMining =>
    context become waiting()
  }

  def waiting(): Receive = {
    case StopMining => context become stopped
    case mineBlocks: MineBlocks =>
      mineBlocks.parentBlock match {
        case Some(parentHash) =>
          blockchain.getBlockByHash(parentHash) match {
            case Some(parentBlock) =>
              self ! MineBlock
              sender() ! MiningOrdered
              context.become(working(mineBlocks.numBlocks, mineBlocks.withTransactions, parentBlock))
            case None =>
              val error = s"Unable to get parent block with hash ${ByteStringUtils.hash2string(parentHash)} for mining"
              sender() ! MiningError(error)
          }
        case None =>
          val parentBlock = blockchain.getBestBlock()
          self ! MineBlock
          sender() ! MiningOrdered
          context.become(working(mineBlocks.numBlocks, mineBlocks.withTransactions, parentBlock))
      }
  }

  def working(
      numBlocks: Int,
      withTransactions: Boolean,
      parentBlock: Block
  ): Receive = {
    case _: MineBlocks =>
      sender() ! MinerIsWorking

    case MineBlock =>
      if (numBlocks > 0) {
        blockCreator.getBlockForMining(parentBlock, withTransactions) pipeTo self
      } else {
        log.info(s"Mining all mocked blocks successful")
        context.become(waiting())
      }

    case pendingBlock: PendingBlock =>
      val minedBlock = pendingBlock.block
      log.info(
        s"Mining mocked block {} successful. Included transactions: {}",
        minedBlock.idTag,
        minedBlock.body.map(_.hashAsHexString)
      )
      syncEventListener ! SyncProtocol.MinedBlock(minedBlock)
      // because of using seconds to calculate block timestamp, we can't mine blocks faster than one block per second
      context.system.scheduler.scheduleOnce(1.second, self, MineBlock)
      context.become(working(numBlocks - 1, withTransactions, minedBlock))

    case Failure(t) =>
      log.error(t, "Unable to get block for mining")
      context.become(waiting())
  }
}

object MockedMiner {
  final val BlockForgerDispatcherId = "mantis.async.dispatchers.block-forger"

  case object MineBlock

  private[ethash] def props(
      blockchain: Blockchain,
      blockCreator: EthashBlockCreator,
      syncEventListener: ActorRef
  ): Props =
    Props(
      new MockedMiner(
        blockchain,
        blockCreator,
        syncEventListener
      )
    ).withDispatcher(BlockForgerDispatcherId)

  def apply(node: Node): ActorRef = {
    node.consensus match {
      case consensus: EthashConsensus =>
        val blockCreator = new EthashBlockCreator(
          pendingTransactionsManager = node.pendingTransactionsManager,
          getTransactionFromPoolTimeout = node.txPoolConfig.getTransactionFromPoolTimeout,
          consensus = consensus,
          ommersPool = node.ommersPool
        )
        val minerProps = props(
          blockchain = node.blockchain,
          blockCreator = blockCreator,
          syncEventListener = node.syncController
        )
        node.system.actorOf(minerProps)
      case consensus =>
        wrongConsensusArgument[EthashConsensus](consensus)
    }
  }
}
