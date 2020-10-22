package io.iohk.ethereum
package consensus
package ethash

import akka.actor.ActorRef
import akka.util.Timeout
import io.iohk.ethereum.consensus.Protocol.{Ethash, MockedPow}
import io.iohk.ethereum.consensus.blocks.TestBlockGenerator
import io.iohk.ethereum.consensus.ethash.MinerResponses.MinerNotExist
import io.iohk.ethereum.consensus.ethash.blocks.{EthashBlockGenerator, EthashBlockGeneratorImpl}
import io.iohk.ethereum.consensus.ethash.validators.ValidatorsExecutor
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import java.util.concurrent.atomic.AtomicReference

import io.iohk.ethereum.consensus.ethash.difficulty.EthashDifficultyCalculator

import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * Implements standard Ethereum consensus (ethash PoW).
  */
class EthashConsensus private (
    val vm: VMImpl,
    blockchain: BlockchainImpl,
    blockchainConfig: BlockchainConfig,
    val config: FullConsensusConfig[EthashConfig],
    val validators: ValidatorsExecutor,
    val blockGenerator: EthashBlockGenerator,
    val difficultyCalculator: EthashDifficultyCalculator
) extends TestConsensus
    with Logger {

  type Config = EthashConfig

  private[this] final val _blockPreparator = new BlockPreparator(
    vm = vm,
    signedTxValidator = validators.signedTransactionValidator,
    blockchain = blockchain,
    blockchainConfig = blockchainConfig
  )

  private[this] val atomicMiner = new AtomicReference[Option[ActorRef]](None)

  private implicit val timeout: Timeout = 5.seconds

  override def sendMiner(msg: MinerProtocol): Future[MinerResponse] = {
    import akka.pattern.ask
    atomicMiner
      .get()
      .map(_.ask(msg).mapTo[MinerResponse])
      .getOrElse(Future.successful(MinerNotExist))
  }

  private[this] def startMiningProcess(node: Node): Unit = {
    atomicMiner.get() match {
      case None =>
        val miner = config.generic.protocol match {
          case Ethash => EthashMiner(node)
          case MockedPow => MockedMiner(node)
        }
        atomicMiner.set(Some(miner))
        sendMiner(MinerProtocol.StartMining)

      case _ =>
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
      validators: ValidatorsExecutor
  ): EthashConsensus = {

    val difficultyCalculator = new EthashDifficultyCalculator(blockchainConfig)

    val blockPreparator = new BlockPreparator(
      vm = vm,
      signedTxValidator = validators.signedTransactionValidator,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig
    )

    val blockGenerator = new EthashBlockGeneratorImpl(
      validators = validators,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      consensusConfig = config.generic,
      blockPreparator = blockPreparator,
      difficultyCalculator
    )

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
