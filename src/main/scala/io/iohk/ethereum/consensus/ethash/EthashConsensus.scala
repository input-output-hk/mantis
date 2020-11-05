package io.iohk.ethereum
package consensus
package ethash

import akka.actor.ActorRef
import akka.util.Timeout
import io.iohk.ethereum.consensus.Protocol._
import io.iohk.ethereum.consensus.blocks.TestBlockGenerator
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.ethash.MinerResponses.MinerNotExist
import io.iohk.ethereum.consensus.ethash.blocks.{
  EthashBlockGenerator,
  EthashBlockGeneratorImpl,
  RestrictedEthashBlockGeneratorImpl
}
import io.iohk.ethereum.consensus.ethash.validators.ValidatorsExecutor
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
  * Implements standard Ethereum consensus (ethash PoW).
  */
class EthashConsensus private (
    val vm: VMImpl,
    blockchain: BlockchainImpl,
    val blockchainConfig: BlockchainConfig,
    val config: FullConsensusConfig[EthashConfig],
    val validators: ValidatorsExecutor,
    val blockGenerator: EthashBlockGenerator,
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

  private[this] def startMiningProcess(node: Node): Unit = {
    if (minerRef.isEmpty) {
      mutex.synchronized {
        if (minerRef.isEmpty) {
          val miner = config.generic.protocol match {
            case Ethash | RestrictedEthash => EthashMiner(node)
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

  def protocol: Protocol = Protocol.Ethash

  /** Internal API, used for testing */
  protected def newBlockGenerator(validators: Validators): EthashBlockGenerator = {
    validators match {
      case _validators: ValidatorsExecutor =>
        val blockPreparator = new BlockPreparator(
          vm = vm,
          signedTxValidator = validators.signedTransactionValidator,
          blockchain = blockchain,
          blockchainConfig = blockchainConfig
        )

        new EthashBlockGeneratorImpl(
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
  def withValidators(validators: Validators): EthashConsensus = {
    validators match {
      case _validators: ValidatorsExecutor =>
        val blockGenerator = newBlockGenerator(validators)

        new EthashConsensus(
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

  def withVM(vm: VMImpl): EthashConsensus =
    new EthashConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator,
      difficultyCalculator
    )

  /** Internal API, used for testing */
  def withBlockGenerator(blockGenerator: TestBlockGenerator): EthashConsensus =
    new EthashConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator.asInstanceOf[EthashBlockGenerator],
      difficultyCalculator
    )

}

object EthashConsensus {
  def apply(
      vm: VMImpl,
      blockchain: BlockchainImpl,
      blockchainConfig: BlockchainConfig,
      config: FullConsensusConfig[EthashConfig],
      validators: ValidatorsExecutor,
      additionalEthashProtocolData: AdditionalEthashProtocolData
  ): EthashConsensus = {

    val difficultyCalculator = DifficultyCalculator(blockchainConfig)

    val blockPreparator = new BlockPreparator(
      vm = vm,
      signedTxValidator = validators.signedTransactionValidator,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig
    )

    val blockGenerator = additionalEthashProtocolData match {
      case RestrictedEthashMinerData(key) =>
        new RestrictedEthashBlockGeneratorImpl(
          validators = validators,
          blockchain = blockchain,
          blockchainConfig = blockchainConfig,
          consensusConfig = config.generic,
          blockPreparator = blockPreparator,
          difficultyCalc = difficultyCalculator,
          minerKeyPair = key
        )

      case NoAdditionalEthashData =>
        new EthashBlockGeneratorImpl(
          validators = validators,
          blockchain = blockchain,
          blockchainConfig = blockchainConfig,
          consensusConfig = config.generic,
          blockPreparator = blockPreparator,
          difficultyCalc = difficultyCalculator
        )
    }

    new EthashConsensus(
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
