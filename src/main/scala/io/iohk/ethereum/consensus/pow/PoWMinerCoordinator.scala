package io.iohk.ethereum.consensus.pow

import akka.actor.typed.{Behavior, DispatcherSelector}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.util.Timeout
import io.iohk.ethereum.consensus.pow.PoWMinerCoordinator.CoordinatorProtocol
import io.iohk.ethereum.consensus.pow.miners.{EthashMiner, KeccakMiner, MinerProtocol}
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.nodebuilder.Node

import scala.concurrent.duration.DurationInt

object PoWMinerCoordinator {
  // TODO in ETCM-773 make trait sealed
  trait CoordinatorProtocol
  final case class StartMining(mode: MinerMode) extends CoordinatorProtocol
  case object ProcessMining extends CoordinatorProtocol
  case object StopMining extends CoordinatorProtocol
  case object MiningCompleted extends CoordinatorProtocol

  // Miner Mode will allow to remove MockerMiner
  sealed trait MinerMode
  case object RecurrentMining extends MinerMode // for normal mining
  case object OnDemandMining extends MinerMode // for testing

  sealed trait MiningResponse
  case object MiningComplete extends MiningResponse

  def apply(
      node: Node,
      blocKCreator: PoWBlockCreator,
      blockchain: Blockchain,
      ecip1049BlockNumber: Option[BigInt]
  ): Behavior[CoordinatorProtocol] =
    Behaviors.setup(context =>
      new PoWMinerCoordinator(context, node, blocKCreator, blockchain, ecip1049BlockNumber).idle()
    )
}

class PoWMinerCoordinator private (
    context: ActorContext[CoordinatorProtocol],
    node: Node,
    blocKCreator: PoWBlockCreator,
    blockchain: Blockchain,
    ecip1049BlockNumber: Option[BigInt]
) {
  import PoWMinerCoordinator._
  private implicit val timeout: Timeout = 5.seconds

  private def idle(): Behavior[CoordinatorProtocol] = Behaviors.receiveMessage { case StartMining(mode) =>
    context.log.info("Received message {}", StartMining(mode))
    switchMiningMode(mode)
  }

  private def handleMiningRecurrent(): Behavior[CoordinatorProtocol] = Behaviors.receiveMessage {
    case StartMining(mode) =>
      context.log.info("Received message {}", StartMining(mode))
      switchMiningMode(mode)
    case ProcessMining =>
      context.log.info("Received message ProcessMining")
      blockchain.getBestBlock().map { block =>
        if (mineWithKeccak(block.header.number)) mineWithKeccak() else mineWithEthash()
      }
      Behaviors.same
    case MiningCompleted =>
      context.log.info("Received message MiningCompleted")
      context.self ! ProcessMining
      Behaviors.same
  }

  // To be used for testing
  private def handleMiningOnDemand(): Behavior[CoordinatorProtocol] = Behaviors.receiveMessage {
    case StartMining(mode) =>
      context.log.info("Received message {}", StartMining(mode))
      switchMiningMode(mode)
  }

  private def switchMiningMode(mode: MinerMode): Behavior[CoordinatorProtocol] = mode match {
    case RecurrentMining =>
      context.self ! ProcessMining
      handleMiningRecurrent()
    case OnDemandMining => handleMiningOnDemand()
  }

  private def mineWithKeccak(currentBlockNumber: BigInt): Boolean = {
    ecip1049BlockNumber match {
      case None => false
      case Some(blockNumber) => (currentBlockNumber + 1) >= blockNumber
    }
  }

  private def mineWithEthash(): Unit = {
    val ethashMiner = EthashMiner(node, blocKCreator)

    blockchain.getBestBlock() match {
      case Some(bestBlock) => ethashMiner ! MinerProtocol.ProcessMining(bestBlock, context.self)
      case None =>
        context.log.error("Unable to get block for mining: blockchain.getBestBlock() returned None")
    }
  }

  private def mineWithKeccak(): Unit = {
    val keccakMiner =
      context.spawn(KeccakMiner(node, blocKCreator), name = "KeccakMiner", DispatcherSelector.sameAsParent())

    // repeating code because miners have different types
    blockchain.getBestBlock() match {
      case Some(bestBlock) => keccakMiner ! MinerProtocol.ProcessMining(bestBlock, context.self)
      case None =>
        context.log.error("Unable to get block for mining: blockchain.getBestBlock() returned None")
    }
  }
}
