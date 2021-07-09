package io.iohk.ethereum
package consensus
package pow

import akka.actor.typed.ActorRef
import akka.actor.typed.DispatcherSelector
import akka.actor.typed.scaladsl.adapter._
import akka.util.Timeout

import monix.eval.Task

import scala.concurrent.duration._

import io.iohk.ethereum.consensus.Protocol._
import io.iohk.ethereum.consensus.blocks.TestBlockGenerator
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.CoordinatorProtocol
import io.iohk.ethereum.consensus.pow.blocks.PoWBlockGenerator
import io.iohk.ethereum.consensus.pow.blocks.PoWBlockGeneratorImpl
import io.iohk.ethereum.consensus.pow.blocks.RestrictedPoWBlockGeneratorImpl
import io.iohk.ethereum.consensus.pow.miners.MinerProtocol
import io.iohk.ethereum.consensus.pow.miners.MockedMiner
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerProtocol
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponse
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponses.MinerNotExist
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.jsonrpc.AkkaTaskOps.TaskActorOps
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Logger

/** Implements standard Ethereum mining (Proof of Work).
  */
class PoWMining private (
    val vm: VMImpl,
    evmCodeStorage: EvmCodeStorage,
    blockchain: BlockchainImpl,
    blockchainReader: BlockchainReader,
    val blockchainConfig: BlockchainConfig,
    val config: FullConsensusConfig[EthashConfig],
    val validators: ValidatorsExecutor,
    val blockGenerator: PoWBlockGenerator,
    val difficultyCalculator: DifficultyCalculator
) extends TestMining
    with Logger {

  type Config = EthashConfig

  final private[this] val _blockPreparator = new BlockPreparator(
    vm = vm,
    signedTxValidator = validators.signedTransactionValidator,
    blockchain = blockchain,
    blockchainReader = blockchainReader,
    blockchainConfig = blockchainConfig
  )

  @volatile private[pow] var minerCoordinatorRef: Option[ActorRef[CoordinatorProtocol]] = None
  // TODO in ETCM-773 remove MockedMiner
  @volatile private[pow] var mockedMinerRef: Option[akka.actor.ActorRef] = None

  final val BlockForgerDispatcherId = "mantis.async.dispatchers.block-forger"
  implicit private val timeout: Timeout = 5.seconds

  override def sendMiner(msg: MinerProtocol): Unit =
    msg match {
      case mineBlocks: MockedMiner.MineBlocks => mockedMinerRef.foreach(_ ! mineBlocks)
      case MinerProtocol.StartMining =>
        mockedMinerRef.foreach(_ ! MockedMiner.StartMining)
        minerCoordinatorRef.foreach(_ ! PoWMiningCoordinator.SetMiningMode(PoWMiningCoordinator.RecurrentMining))
      case MinerProtocol.StopMining =>
        mockedMinerRef.foreach(_ ! MockedMiner.StopMining)
        minerCoordinatorRef.foreach(_ ! PoWMiningCoordinator.StopMining)
      case _ => log.warn("SendMiner method received unexpected message {}", msg)
    }

  // no interactions are done with minerCoordinatorRef using the ask pattern
  override def askMiner(msg: MockedMinerProtocol): Task[MockedMinerResponse] =
    mockedMinerRef
      .map(_.askFor[MockedMinerResponse](msg))
      .getOrElse(Task.now(MinerNotExist))

  private[this] val mutex = new Object

  /*
   * guarantees one miner instance
   * this should not use a atomic* construct as it has side-effects
   *
   * TODO further refactors should focus on extracting two types - one with a miner, one without - based on the config
   */
  private[this] def startMiningProcess(node: Node, blockCreator: PoWBlockCreator): Unit =
    mutex.synchronized {
      if (minerCoordinatorRef.isEmpty && mockedMinerRef.isEmpty) {
        config.generic.protocol match {
          case PoW | RestrictedPoW =>
            log.info("Instantiating PoWMiningCoordinator")
            minerCoordinatorRef = Some(
              node.system.spawn(
                PoWMiningCoordinator(
                  node.syncController,
                  node.ethMiningService,
                  blockCreator,
                  blockchainReader,
                  blockchainConfig.forkBlockNumbers.ecip1049BlockNumber
                ),
                "PoWMinerCoordinator",
                DispatcherSelector.fromConfig(BlockForgerDispatcherId)
              )
            )
          case MockedPow =>
            log.info("Instantiating MockedMiner")
            mockedMinerRef = Some(MockedMiner(node))
        }
        sendMiner(MinerProtocol.StartMining)
      }
    }

  private[this] def stopMiningProcess(): Unit =
    sendMiner(MinerProtocol.StopMining)

  /** This is used by the [[io.iohk.ethereum.consensus.Mining#blockGenerator blockGenerator]].
    */
  def blockPreparator: BlockPreparator = this._blockPreparator

  /** Starts the consensus protocol on the current `node`.
    */
  def startProtocol(node: Node): Unit =
    if (config.miningEnabled) {
      log.info("Mining is enabled. Will try to start configured miner actor")
      val blockCreator = node.mining match {
        case mining: PoWMining =>
          new PoWBlockCreator(
            pendingTransactionsManager = node.pendingTransactionsManager,
            getTransactionFromPoolTimeout = node.txPoolConfig.getTransactionFromPoolTimeout,
            mining = mining,
            ommersPool = node.ommersPool
          )
        case mining => wrongConsensusArgument[PoWMining](mining)
      }

      startMiningProcess(node, blockCreator)
    } else log.info("Not starting any miner actor because mining is disabled")

  def stopProtocol(): Unit =
    if (config.miningEnabled) {
      stopMiningProcess()
    }

  def protocol: Protocol = Protocol.PoW

  /** Internal API, used for testing */
  protected def newBlockGenerator(validators: Validators): PoWBlockGenerator =
    validators match {
      case _validators: ValidatorsExecutor =>
        val blockPreparator = new BlockPreparator(
          vm = vm,
          signedTxValidator = validators.signedTransactionValidator,
          blockchain = blockchain,
          blockchainReader = blockchainReader,
          blockchainConfig = blockchainConfig
        )

        new PoWBlockGeneratorImpl(
          evmCodeStorage = evmCodeStorage,
          validators = _validators,
          blockchainReader = blockchainReader,
          blockchainConfig = blockchainConfig,
          miningConfig = config.generic,
          blockPreparator = blockPreparator,
          difficultyCalculator,
          blockTimestampProvider = blockGenerator.blockTimestampProvider
        )

      case _ =>
        wrongValidatorsArgument[ValidatorsExecutor](validators)
    }

  /** Internal API, used for testing */
  def withValidators(validators: Validators): PoWMining =
    validators match {
      case _validators: ValidatorsExecutor =>
        val blockGenerator = newBlockGenerator(validators)

        new PoWMining(
          vm = vm,
          evmCodeStorage = evmCodeStorage,
          blockchain = blockchain,
          blockchainReader = blockchainReader,
          blockchainConfig = blockchainConfig,
          config = config,
          validators = _validators,
          blockGenerator = blockGenerator,
          difficultyCalculator
        )

      case _ =>
        wrongValidatorsArgument[ValidatorsExecutor](validators)
    }

  def withVM(vm: VMImpl): PoWMining =
    new PoWMining(
      vm = vm,
      evmCodeStorage = evmCodeStorage,
      blockchain = blockchain,
      blockchainReader = blockchainReader,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator,
      difficultyCalculator
    )

  /** Internal API, used for testing */
  def withBlockGenerator(blockGenerator: TestBlockGenerator): PoWMining =
    new PoWMining(
      evmCodeStorage = evmCodeStorage,
      vm = vm,
      blockchain = blockchain,
      blockchainReader = blockchainReader,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator.asInstanceOf[PoWBlockGenerator],
      difficultyCalculator = difficultyCalculator
    )

}

object PoWMining {
  // scalastyle:off method.length
  def apply(
      vm: VMImpl,
      evmCodeStorage: EvmCodeStorage,
      blockchain: BlockchainImpl,
      blockchainReader: BlockchainReader,
      blockchainConfig: BlockchainConfig,
      config: FullConsensusConfig[EthashConfig],
      validators: ValidatorsExecutor,
      additionalEthashProtocolData: AdditionalPoWProtocolData
  ): PoWMining = {
    val difficultyCalculator = DifficultyCalculator(blockchainConfig)
    val blockPreparator = new BlockPreparator(
      vm = vm,
      signedTxValidator = validators.signedTransactionValidator,
      blockchain = blockchain,
      blockchainReader = blockchainReader,
      blockchainConfig = blockchainConfig
    )
    val blockGenerator = additionalEthashProtocolData match {
      case RestrictedPoWMinerData(key) =>
        new RestrictedPoWBlockGeneratorImpl(
          evmCodeStorage = evmCodeStorage,
          validators = validators,
          blockchainReader = blockchainReader,
          blockchainConfig = blockchainConfig,
          miningConfig = config.generic,
          blockPreparator = blockPreparator,
          difficultyCalc = difficultyCalculator,
          minerKeyPair = key
        )
      case NoAdditionalPoWData =>
        new PoWBlockGeneratorImpl(
          evmCodeStorage = evmCodeStorage,
          validators = validators,
          blockchainReader = blockchainReader,
          blockchainConfig = blockchainConfig,
          miningConfig = config.generic,
          blockPreparator = blockPreparator,
          difficultyCalc = difficultyCalculator
        )
    }
    new PoWMining(
      vm = vm,
      evmCodeStorage = evmCodeStorage,
      blockchain = blockchain,
      blockchainReader = blockchainReader,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator,
      difficultyCalculator
    )
  }
}
