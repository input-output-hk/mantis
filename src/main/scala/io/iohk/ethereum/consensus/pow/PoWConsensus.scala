package io.iohk.ethereum
package consensus
package pow

import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ActorRef, DispatcherSelector}
import akka.util.Timeout
import io.iohk.ethereum.consensus.Protocol._
import io.iohk.ethereum.consensus.blocks.TestBlockGenerator
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.{
  CoordinatorProtocol,
  MineOnDemand,
  MiningSuccessful,
  MiningUnsuccessful
}
import io.iohk.ethereum.consensus.pow.blocks.{PoWBlockGenerator, PoWBlockGeneratorImpl, RestrictedPoWBlockGeneratorImpl}
import io.iohk.ethereum.consensus.pow.miners.MinerProtocol
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import monix.eval.Task

import scala.concurrent.duration._

/**
  * Implements standard Ethereum consensus (Proof of Work).
  */
class PoWConsensus private (
    val vm: VMImpl,
    blockchain: BlockchainImpl,
    val blockchainConfig: BlockchainConfig,
    val config: FullConsensusConfig[EthashConfig],
    val validators: ValidatorsExecutor,
    val blockGenerator: PoWBlockGenerator,
    val difficultyCalculator: DifficultyCalculator
) extends TestConsensus
    with Logger {

  type Config = EthashConfig

  private[this] final val _blockPreparator = new BlockPreparator(
    vm = vm,
    signedTxValidator = validators.signedTransactionValidator,
    blockchain = blockchain,
    blockchainConfig = blockchainConfig
  )

  @volatile private[pow] var minerCoordinatorRef: Option[ActorRef[CoordinatorProtocol]] = None

  final val BlockForgerDispatcherId = "mantis.async.dispatchers.block-forger"
  private implicit val timeout: Timeout = 5.seconds

  override def sendMiner(msg: MinerProtocol): Unit =
    msg match {
      case MinerProtocol.OnDemandMining(mineBlocks) =>
        minerCoordinatorRef.foreach(
          _ ! PoWMiningCoordinator.SetMiningMode(
            PoWMiningCoordinator.OnDemandMining(mineBlocks)
          )
        )
      case MinerProtocol.StartMining =>
        minerCoordinatorRef.foreach(_ ! PoWMiningCoordinator.SetMiningMode(PoWMiningCoordinator.RecurrentMining))
      case MinerProtocol.StopMining =>
        minerCoordinatorRef.foreach(_ ! PoWMiningCoordinator.StopMining)
      case _ => log.warn("SendMiner method received unexpected message {}", msg)
    }

  override def askMiner(msg: MineOnDemand): Task[CoordinatorProtocol] = {
    val system: ActorSystem = ActorSystem("mantis_system")
    implicit val schedular: akka.actor.typed.Scheduler = system.scheduler.toTyped
    config.miningOnDemand match {
      case false => Task.now(MiningUnsuccessful)
      case true => {
        sendMiner(MinerProtocol.OnDemandMining(msg))
        Task.now(MiningSuccessful)
      }
    }
  }

  private[this] val mutex = new Object
  /*
   * guarantees one miner instance
   * this should not use a atomic* construct as it has side-effects
   *
   * TODO further refactors should focus on extracting two types - one with a miner, one without - based on the config
   */
  private[this] def startMiningProcess(
      node: Node,
      blockCreator: PoWBlockCreator,
      miningEnabled: Boolean,
      miningOnDemand: Boolean
  ): Unit = {
    mutex.synchronized {
      if (minerCoordinatorRef.isEmpty) {
        config.generic.protocol match {
          case PoW | RestrictedPoW =>
            log.info("Instantiating PoWMiningCoordinator")
            minerCoordinatorRef = Some(
              node.system.spawn(
                PoWMiningCoordinator(
                  node.syncController,
                  node.ethMiningService,
                  blockCreator,
                  blockchain,
                  blockchainConfig.ecip1049BlockNumber
                ),
                "PoWMinerCoordinator",
                DispatcherSelector.fromConfig(BlockForgerDispatcherId)
              )
            )
          case MockedPow =>
            log.info("Instantiating MockedMiner")
        }
        if (miningEnabled)
          sendMiner(MinerProtocol.StartMining)
      }
    }
  }

  private[this] def stopMiningProcess(): Unit = {
    sendMiner(MinerProtocol.StopMining)
  }

  /**
    * This is used by the [[io.iohk.ethereum.consensus.Consensus#blockGenerator blockGenerator]].
    */
  def blockPreparator: BlockPreparator = this._blockPreparator

  /**
    * Starts the consensus protocol on the current `node`.
    */
  def startProtocol(node: Node): Unit = {
    if (config.miningEnabled || config.miningOnDemand) {
      log.info("Mining is enabled. Will try to start configured miner actor")
      val blockCreator = node.consensus match {
        case consensus: PoWConsensus =>
          new PoWBlockCreator(
            pendingTransactionsManager = node.pendingTransactionsManager,
            getTransactionFromPoolTimeout = node.txPoolConfig.getTransactionFromPoolTimeout,
            consensus = consensus,
            ommersPool = node.ommersPool
          )
        case consensus => wrongConsensusArgument[PoWConsensus](consensus)
      }

      startMiningProcess(node, blockCreator, config.miningEnabled, config.miningOnDemand)
    } else log.info("Not starting any miner actor because mining is disabled")
  }

  def stopProtocol(): Unit = {
    if (config.miningEnabled) {
      stopMiningProcess()
    }
  }

  def protocol: Protocol = Protocol.PoW

  /** Internal API, used for testing */
  protected def newBlockGenerator(validators: Validators): PoWBlockGenerator = {
    validators match {
      case _validators: ValidatorsExecutor =>
        val blockPreparator = new BlockPreparator(
          vm = vm,
          signedTxValidator = validators.signedTransactionValidator,
          blockchain = blockchain,
          blockchainConfig = blockchainConfig
        )

        new PoWBlockGeneratorImpl(
          validators = _validators,
          blockchain = blockchain,
          blockchainConfig = blockchainConfig,
          consensusConfig = config.generic,
          blockPreparator = blockPreparator,
          difficultyCalculator,
          blockTimestampProvider = blockGenerator.blockTimestampProvider
        )

      case _ =>
        wrongValidatorsArgument[ValidatorsExecutor](validators)
    }
  }

  /** Internal API, used for testing */
  def withValidators(validators: Validators): PoWConsensus = {
    validators match {
      case _validators: ValidatorsExecutor =>
        val blockGenerator = newBlockGenerator(validators)

        new PoWConsensus(
          vm = vm,
          blockchain = blockchain,
          blockchainConfig = blockchainConfig,
          config = config,
          validators = _validators,
          blockGenerator = blockGenerator,
          difficultyCalculator
        )

      case _ =>
        wrongValidatorsArgument[ValidatorsExecutor](validators)
    }
  }

  def withVM(vm: VMImpl): PoWConsensus =
    new PoWConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator,
      difficultyCalculator
    )

  /** Internal API, used for testing */
  def withBlockGenerator(blockGenerator: TestBlockGenerator): PoWConsensus =
    new PoWConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator.asInstanceOf[PoWBlockGenerator],
      difficultyCalculator
    )

}

object PoWConsensus {
  def apply(
      vm: VMImpl,
      blockchain: BlockchainImpl,
      blockchainConfig: BlockchainConfig,
      config: FullConsensusConfig[EthashConfig],
      validators: ValidatorsExecutor,
      additionalEthashProtocolData: AdditionalPoWProtocolData
  ): PoWConsensus = {

    val difficultyCalculator = DifficultyCalculator(blockchainConfig)

    val blockPreparator = new BlockPreparator(
      vm = vm,
      signedTxValidator = validators.signedTransactionValidator,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig
    )

    val blockGenerator = additionalEthashProtocolData match {
      case RestrictedPoWMinerData(key) =>
        new RestrictedPoWBlockGeneratorImpl(
          validators = validators,
          blockchain = blockchain,
          blockchainConfig = blockchainConfig,
          consensusConfig = config.generic,
          blockPreparator = blockPreparator,
          difficultyCalc = difficultyCalculator,
          minerKeyPair = key
        )

      case NoAdditionalPoWData =>
        new PoWBlockGeneratorImpl(
          validators = validators,
          blockchain = blockchain,
          blockchainConfig = blockchainConfig,
          consensusConfig = config.generic,
          blockPreparator = blockPreparator,
          difficultyCalc = difficultyCalculator
        )
    }

    new PoWConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator,
      difficultyCalculator
    )
  }
}
