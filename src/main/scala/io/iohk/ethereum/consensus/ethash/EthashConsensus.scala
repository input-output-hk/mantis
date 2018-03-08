package io.iohk.ethereum
package consensus
package ethash

import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorRef
import io.iohk.ethereum.consensus.blocks.BlockGenerator
import io.iohk.ethereum.consensus.ethash.EthashMiner.MinerMsg
import io.iohk.ethereum.consensus.ethash.blocks.{EthashBlockGenerator, EthashBlockGeneratorImpl}
import io.iohk.ethereum.consensus.ethash.validators.{EthashValidators, StdEthashValidators}
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.{Block, BlockchainImpl}
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockExecutionSuccess, BlockPreparator}
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.VM

/**
 * Implements standard Ethereum consensus (ethash PoW).
 */
class EthashConsensus private(
  vm: VM,
  blockchain: BlockchainImpl,
  blockchainConfig: BlockchainConfig,
  fullConsensusConfig: FullConsensusConfig[EthashConfig],
  _validators: EthashValidators,
  _blockGenerator: EthashBlockGenerator
) extends ConsensusImpl[EthashConfig](
  vm,
  blockchain,
  blockchainConfig,
  fullConsensusConfig
) {

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


  /**
   * Provides the set of validators specific to this consensus protocol.
   */
  def validators: EthashValidators = this._validators

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
          fullConsensusConfig = fullConsensusConfig,
          _validators = _validators,
          _blockGenerator = blockGenerator
        )

      case _ ⇒
        wrongValidatorsArgument[EthashValidators](validators)
    }
  }

  def withVM(vm: VM): EthashConsensus =
    new EthashConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      fullConsensusConfig = fullConsensusConfig,
      _validators = validators,
      _blockGenerator = blockGenerator
    )

  /** Internal API, used for testing */
  def withBlockGenerator(blockGenerator: BlockGenerator): EthashConsensus =
    new EthashConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      fullConsensusConfig = fullConsensusConfig,
      _validators = validators,
      _blockGenerator = blockGenerator.asInstanceOf[EthashBlockGenerator]
    )

  /**
   * Returns the [[io.iohk.ethereum.consensus.blocks.BlockGenerator BlockGenerator]]
   * this consensus protocol uses.
   */
  def blockGenerator: EthashBlockGenerator = this._blockGenerator

  override def validateBlockBeforeExecution(
    block: Block,
    getBlockHeaderByHash: GetBlockHeaderByHash,
    getNBlocksBack: GetNBlocksBack
  ): Either[BlockExecutionError.ValidationBeforeExecError, BlockExecutionSuccess] = {

    val blockHeaderV = validators.blockHeaderValidator
    val blockV = validators.blockValidator
    val ommersV = validators.ommersValidator

    val header = block.header
    val body = block.body

    val result = for {
      _ <- blockHeaderV.validate(header, getBlockHeaderByHash)
      _ <- blockV.validateHeaderAndBody(header, body)
      _ <- ommersV.validate(header.parentHash, header.number, body.uncleNodesList,
        getBlockHeaderByHash, getNBlocksBack)
    } yield BlockExecutionSuccess

    result.left.map(ValidationBeforeExecError)
  }
}

object EthashConsensus {
  def apply(
    vm: VM,
    blockchain: BlockchainImpl,
    blockchainConfig: BlockchainConfig,
    fullConsensusConfig: FullConsensusConfig[EthashConfig]
  ): EthashConsensus = {

    val validators = StdEthashValidators(blockchainConfig)

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
      consensusConfig = fullConsensusConfig.generic,
      blockPreparator = blockPreparator
    )

    new EthashConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      fullConsensusConfig = fullConsensusConfig,
      _validators = validators,
      _blockGenerator = blockGenerator
    )
  }
}
