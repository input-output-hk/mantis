package io.iohk.ethereum.consensus.pow.miners

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.Status.Failure
import akka.util.ByteString

import monix.execution.Scheduler

import scala.concurrent.duration._

import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.consensus.blocks.PendingBlockAndState
import io.iohk.ethereum.consensus.mining.wrongConsensusArgument
import io.iohk.ethereum.consensus.pow.PoWBlockCreator
import io.iohk.ethereum.consensus.pow.PoWMining
import io.iohk.ethereum.consensus.pow.miners.MinerProtocol._
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MineBlock
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MineBlocks
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerProtocol
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponses.MinerIsWorking
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponses.MinerNotSupported
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponses.MiningError
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponses.MiningOrdered
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.ByteStringUtils.ByteStringOps

class MockedMiner(
    blockchain: Blockchain,
    blockchainReader: BlockchainReader,
    blockCreator: PoWBlockCreator,
    syncEventListener: ActorRef
) extends Actor
    with ActorLogging {
  import akka.pattern.pipe
  implicit val scheduler: Scheduler = Scheduler(context.dispatcher)

  override def receive: Receive = stopped

  def stopped: Receive = notSupportedMockedMinerMessages.orElse { case StartMining =>
    context.become(waiting())
  }

  def waiting(): Receive = {
    case StopMining => context.become(stopped)
    case mineBlocks: MineBlocks =>
      mineBlocks.parentBlock match {
        case Some(parentHash) =>
          blockchainReader.getBlockByHash(parentHash) match {
            case Some(parentBlock) => startMiningBlocks(mineBlocks, parentBlock)
            case None =>
              val error = s"Unable to get parent block with hash ${ByteStringUtils.hash2string(parentHash)} for mining"
              sender() ! MiningError(error)
          }
        case None =>
          val parentBlock = blockchainReader.getBestBlock()
          startMiningBlocks(mineBlocks, parentBlock.get)
      }
  }

  private def startMiningBlocks(mineBlocks: MineBlocks, parentBlock: Block) = {
    self ! MineBlock
    sender() ! MiningOrdered
    context.become(working(mineBlocks.numBlocks, mineBlocks.withTransactions, parentBlock, None))
  }

  def working(
      numBlocks: Int,
      withTransactions: Boolean,
      parentBlock: Block,
      initialWorldStateBeforeExecution: Option[InMemoryWorldStateProxy]
  ): Receive = {
    case _: MineBlocks =>
      sender() ! MinerIsWorking

    case MineBlock =>
      if (numBlocks > 0) {
        blockCreator
          .getBlockForMining(parentBlock, withTransactions, initialWorldStateBeforeExecution)
          .runToFuture
          .pipeTo(self)
      } else {
        log.info(s"Mining all mocked blocks successful")
        context.become(waiting())
      }

    case PendingBlockAndState(pendingBlock, state) =>
      val minedBlock = pendingBlock.block
      log.info(
        s"Mining mocked block {} successful. Included transactions: {}",
        minedBlock.idTag,
        minedBlock.body.transactionList.map(_.hash.toHex)
      )
      syncEventListener ! SyncProtocol.MinedBlock(minedBlock)
      // because of using seconds to calculate block timestamp, we can't mine blocks faster than one block per second
      context.system.scheduler.scheduleOnce(1.second, self, MineBlock)
      context.become(working(numBlocks - 1, withTransactions, minedBlock, Some(state)))

    case Failure(t) =>
      log.error(t, "Unable to get block for mining")
      context.become(waiting())
  }

  private def notSupportedMockedMinerMessages: Receive = { case msg: MockedMinerProtocol =>
    sender() ! MinerNotSupported(msg)
  }
}

object MockedMiner {
  final val BlockForgerDispatcherId = "mantis.async.dispatchers.block-forger"

  case object MineBlock

  private[pow] def props(
      blockchain: Blockchain,
      blockchainReader: BlockchainReader,
      blockCreator: PoWBlockCreator,
      syncEventListener: ActorRef
  ): Props =
    Props(
      new MockedMiner(
        blockchain,
        blockchainReader,
        blockCreator,
        syncEventListener
      )
    ).withDispatcher(BlockForgerDispatcherId)

  def apply(node: Node): ActorRef =
    node.mining match {
      case consensus: PoWMining =>
        val blockCreator = new PoWBlockCreator(
          pendingTransactionsManager = node.pendingTransactionsManager,
          getTransactionFromPoolTimeout = node.txPoolConfig.getTransactionFromPoolTimeout,
          mining = consensus,
          ommersPool = node.ommersPool
        )
        val minerProps = props(
          blockchain = node.blockchain,
          blockchainReader = node.blockchainReader,
          blockCreator = blockCreator,
          syncEventListener = node.syncController
        )
        node.system.actorOf(minerProps)
      case consensus =>
        wrongConsensusArgument[PoWMining](consensus)
    }

  // TODO to be removed in ETCM-773
  sealed trait MockedMinerProtocol extends MinerProtocol
  case object StartMining extends MockedMinerProtocol
  case object StopMining extends MockedMinerProtocol

  case class MineBlocks(numBlocks: Int, withTransactions: Boolean, parentBlock: Option[ByteString] = None)
      extends MockedMinerProtocol

  trait MockedMinerResponse

  object MockedMinerResponses {
    case object MinerIsWorking extends MockedMinerResponse

    case object MiningOrdered extends MockedMinerResponse

    case object MinerNotExist extends MockedMinerResponse

    case class MiningError(errorMsg: String) extends MockedMinerResponse

    case class MinerNotSupported(msg: MockedMinerProtocol) extends MockedMinerResponse
  }
}
