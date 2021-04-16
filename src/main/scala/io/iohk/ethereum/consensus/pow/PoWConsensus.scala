package io.iohk.ethereum
package consensus
package pow

import akka.actor.ActorRef
import akka.util.Timeout
import io.iohk.ethereum.consensus.Protocol._
import io.iohk.ethereum.consensus.blocks.TestBlockGenerator
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.pow.MinerResponses.MinerNotExist
import io.iohk.ethereum.consensus.pow.blocks.{PoWBlockGenerator, PoWBlockGeneratorImpl, RestrictedPoWBlockGeneratorImpl}
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.jsonrpc.AkkaTaskOps.TaskActorOps
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

  @volatile private[this] var minerRef: Option[ActorRef] = None

  private implicit val timeout: Timeout = 5.seconds

  override def sendMiner(msg: MinerProtocol): Unit =
    minerRef.foreach(_ ! msg)

  override def askMiner(msg: MinerProtocol): Task[MinerResponse] = {
    minerRef
      .map(_.askFor[MinerResponse](msg))
      .getOrElse(Task.now(MinerNotExist))
  }

  private[this] val mutex = new Object

  /*
   * guarantees one miner instance
   * this should not use a atomic* construct as it has side-effects
   *
   * TODO further refactors should focus on extracting two types - one with a miner, one without - based on the config
   */
  private[this] def startMiningProcess(node: Node): Unit = {
    if (minerRef.isEmpty) {
      mutex.synchronized {
        if (minerRef.isEmpty) {
          val miner = config.generic.protocol match {
            case PoW | RestrictedPoW => EthashMiner(node)
            case MockedPow => MockedMiner(node)
          }
          minerRef = Some(miner)
          sendMiner(MinerProtocol.StartMining)
        }
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
    if (config.miningEnabled) {
      startMiningProcess(node)
    }
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
