package io.iohk.ethereum.consensus.pow

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.{ActorRef => ClassicActorRef}
import akka.util.Timeout
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.CoordinatorProtocol
import io.iohk.ethereum.consensus.pow.miners.{EthashDAGManager, EthashMiner, KeccakMiner, Miner}
import io.iohk.ethereum.domain.{Block, Blockchain}
import io.iohk.ethereum.jsonrpc.EthMiningService
import monix.execution.{CancelableFuture, Scheduler}

import scala.concurrent.duration.DurationInt

object PoWMiningCoordinator {
  // TODO in ETCM-773 make trait sealed
  trait CoordinatorProtocol

  final case class SetMiningMode(mode: MiningMode) extends CoordinatorProtocol

  case object MineNext extends CoordinatorProtocol

  case object StopMining extends CoordinatorProtocol

  case object MiningSuccessful extends CoordinatorProtocol

  case object MiningUnsuccessful extends CoordinatorProtocol

  // MiningMode will allow to remove MockerMiner
  sealed trait MiningMode

  case object RecurrentMining extends MiningMode // for normal mining

  case object OnDemandMining extends MiningMode // for testing

  sealed trait MiningResponse

  case object MiningComplete extends MiningResponse

  def apply(
      syncController: ClassicActorRef,
      ethMiningService: EthMiningService,
      blockCreator: PoWBlockCreator,
      blockchain: Blockchain,
      ecip1049BlockNumber: Option[BigInt]
  ): Behavior[CoordinatorProtocol] =
    Behaviors
      .setup[CoordinatorProtocol](context =>
        new PoWMiningCoordinator(
          context,
          syncController,
          ethMiningService,
          blockCreator,
          blockchain,
          ecip1049BlockNumber
        )
      )
}

class PoWMiningCoordinator private (
    context: ActorContext[CoordinatorProtocol],
    syncController: ClassicActorRef,
    ethMiningService: EthMiningService,
    blockCreator: PoWBlockCreator,
    blockchain: Blockchain,
    ecip1049BlockNumber: Option[BigInt]
) extends AbstractBehavior[CoordinatorProtocol](context) {

  import PoWMiningCoordinator._

  private implicit val scheduler: Scheduler = Scheduler(context.executionContext)
  private implicit val timeout: Timeout = 5.seconds
  private val log = context.log
  private val dagManager = new EthashDAGManager(blockCreator)

  override def onMessage(msg: CoordinatorProtocol): Behavior[CoordinatorProtocol] = msg match {
    case SetMiningMode(mode) =>
      log.info("Received message {}", SetMiningMode(mode))
      switchMiningMode(mode)
  }

  private def handleMiningRecurrent(): Behavior[CoordinatorProtocol] = Behaviors.receiveMessage {
    case SetMiningMode(mode) =>
      log.info("Received message {}", SetMiningMode(mode))
      switchMiningMode(mode)

    case MineNext =>
      log.debug("Received message MineNext")
      blockchain
        .getBestBlock()
        .fold {
          log.error("Unable to get block for mining: blockchain.getBestBlock() returned None")
          context.self ! MineNext
        } { block =>
          if (shouldMineWithKeccak(block.header.number)) mineWithKeccak(block) else mineWithEthash(block)
        }
      Behaviors.same

    case StopMining =>
      log.info("Stopping PoWMiningCoordinator...")
      Behaviors.stopped
  }

  // TODO To be used for testing and finished on ETCM-773
  private def handleMiningOnDemand(): Behavior[CoordinatorProtocol] = Behaviors.receiveMessage {
    case SetMiningMode(mode) =>
      log.info("Received message {}", SetMiningMode(mode))
      switchMiningMode(mode)
  }

  private def switchMiningMode(mode: MiningMode): Behavior[CoordinatorProtocol] = mode match {
    case RecurrentMining =>
      context.self ! MineNext
      handleMiningRecurrent()
    case OnDemandMining => handleMiningOnDemand()
  }

  private def shouldMineWithKeccak(currentBlockNumber: BigInt): Boolean = {
    ecip1049BlockNumber match {
      case None => false
      case Some(blockNumber) => (currentBlockNumber + 1) >= blockNumber
    }
  }

  private def mineWithEthash(bestBlock: Block): Unit = {
    log.debug("Mining with Ethash")
    val ethashMiner = new EthashMiner(dagManager, blockCreator, syncController, ethMiningService)
    mine(ethashMiner, bestBlock)
  }

  private def mineWithKeccak(bestBlock: Block): Unit = {
    log.debug("Mining with Keccak")
    val keccakMiner = new KeccakMiner(blockCreator, syncController, ethMiningService)
    mine(keccakMiner, bestBlock)
  }

  private def mine(miner: Miner, bestBlock: Block): CancelableFuture[Unit] =
    miner.processMining(bestBlock).map(_ => context.self ! MineNext)
}
