package io.iohk.ethereum.consensus.pow

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.{ActorRef => ClassicActorRef}

import monix.execution.CancelableFuture
import monix.execution.Scheduler

import scala.concurrent.duration.DurationInt

import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.CoordinatorProtocol
import io.iohk.ethereum.consensus.pow.miners.EthashDAGManager
import io.iohk.ethereum.consensus.pow.miners.EthashMiner
import io.iohk.ethereum.consensus.pow.miners.KeccakMiner
import io.iohk.ethereum.consensus.pow.miners.Miner
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.jsonrpc.EthMiningService

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

  sealed trait MiningAlgorithm
  case object EthashAlgorithm extends MiningAlgorithm
  case object KeccakAlgorithm extends MiningAlgorithm

  def apply(
      syncController: ClassicActorRef,
      ethMiningService: EthMiningService,
      blockCreator: PoWBlockCreator,
      blockchainReader: BlockchainReader,
      ecip1049BlockNumber: Option[BigInt]
  ): Behavior[CoordinatorProtocol] =
    Behaviors
      .setup[CoordinatorProtocol](context =>
        new PoWMiningCoordinator(
          context,
          syncController,
          ethMiningService,
          blockCreator,
          blockchainReader,
          ecip1049BlockNumber
        )
      )
}

class PoWMiningCoordinator private (
    context: ActorContext[CoordinatorProtocol],
    syncController: ClassicActorRef,
    ethMiningService: EthMiningService,
    blockCreator: PoWBlockCreator,
    blockchainReader: BlockchainReader,
    ecip1049BlockNumber: Option[BigInt]
) extends AbstractBehavior[CoordinatorProtocol](context) {

  import PoWMiningCoordinator._

  implicit private val scheduler: Scheduler = Scheduler(context.executionContext)
  5.seconds
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
      blockchainReader
        .getBestBlock()
        .fold {
          log.error("Unable to get block for mining: blockchainReader.getBestBlock() returned None")
          context.self ! MineNext
        } { block =>
          getMiningAlgorithm(block.header.number) match {
            case EthashAlgorithm => mineWithEthash(block)
            case KeccakAlgorithm => mineWithKeccak(block)
          }
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

  private def getMiningAlgorithm(currentBlockNumber: BigInt): MiningAlgorithm =
    ecip1049BlockNumber match {
      case None              => EthashAlgorithm
      case Some(blockNumber) => if (currentBlockNumber + 1 >= blockNumber) KeccakAlgorithm else EthashAlgorithm
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
