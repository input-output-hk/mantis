package io.iohk.ethereum
package consensus
package ethash

import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorRef
import io.iohk.ethereum.consensus.blocks.TestBlockGenerator
import io.iohk.ethereum.consensus.ethash.EthashMiner.MinerMsg
import io.iohk.ethereum.consensus.ethash.blocks.{EthashBlockGenerator, EthashBlockGeneratorImpl}
import io.iohk.ethereum.consensus.ethash.validators.EthashValidators
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}

/**
 * Implements standard Ethereum consensus (ethash PoW).
 */
class EthashConsensus private(
  val vm: VMImpl,
  blockchain: BlockchainImpl,
  blockchainConfig: BlockchainConfig,
  val config: FullConsensusConfig[EthashConfig],
  val validators: EthashValidators,
  val blockGenerator: EthashBlockGenerator
) extends TestConsensus with Logger  {

  type Config = EthashConfig

  private[this] final val _blockPreparator = new BlockPreparator(
    vm = vm,
    signedTxValidator = validators.signedTransactionValidator,
    blockchain = blockchain,
    blockchainConfig = blockchainConfig
  )

  private[this] val atomicMiner = new AtomicReference[Option[ActorRef]](None)
  private[this] def sendMiner(msg: MinerMsg): Unit =
    atomicMiner.get().foreach(_ ! msg)

  private[this] def startMiningProcess(node: Node): Unit = {
    atomicMiner.get() match {
      case None ⇒
        val miner = EthashMiner(node)
        atomicMiner.set(Some(miner))

        sendMiner(EthashMiner.StartMining)

      case _ ⇒
    }
  }

  private[this] def stopMiningProcess(): Unit = {
    sendMiner(EthashMiner.StopMining)
  }

  /**
   * This is used by the [[io.iohk.ethereum.consensus.Consensus#blockGenerator blockGenerator]].
   */
  def blockPreparator: BlockPreparator = this._blockPreparator

  /**
   * Starts the consensus protocol on the current `node`.
   */
  def startProtocol(node: Node): Unit = {
    if(config.miningEnabled) {
      startMiningProcess(node)
    }
  }

  def stopProtocol(): Unit = {
    if(config.miningEnabled) {
      stopMiningProcess()
    }
  }

  def protocol: Protocol = Protocol.Ethash

  /** Internal API, used for testing */
  protected def newBlockGenerator(validators: Validators): EthashBlockGenerator = {
    validators match {
      case _validators: EthashValidators ⇒
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
          blockTimestampProvider = blockGenerator.blockTimestampProvider
        )

      case _ ⇒
        wrongValidatorsArgument[EthashValidators](validators)
    }
  }


  /** Internal API, used for testing */
  def withValidators(validators: Validators): EthashConsensus = {
    validators match {
      case _validators: EthashValidators ⇒
        val blockGenerator = newBlockGenerator(validators)

        new EthashConsensus(
          vm = vm,
          blockchain = blockchain,
          blockchainConfig = blockchainConfig,
          config = config,
          validators = _validators,
          blockGenerator = blockGenerator
        )

      case _ ⇒
        wrongValidatorsArgument[EthashValidators](validators)
    }
  }

  def withVM(vm: VMImpl): EthashConsensus =
    new EthashConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator
    )

  /** Internal API, used for testing */
  def withBlockGenerator(blockGenerator: TestBlockGenerator): EthashConsensus =
    new EthashConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator.asInstanceOf[EthashBlockGenerator]
    )


}

object EthashConsensus {
  def apply(
    vm: VMImpl,
    blockchain: BlockchainImpl,
    blockchainConfig: BlockchainConfig,
    config: FullConsensusConfig[EthashConfig]
  ): EthashConsensus = {

    val validators = EthashValidators(blockchainConfig, blockchain)

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
      blockPreparator = blockPreparator
    )

    new EthashConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator
    )
  }
}
