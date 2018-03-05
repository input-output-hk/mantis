package io.iohk.ethereum
package consensus
package ethash

import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorRef
import io.iohk.ethereum.consensus.ethash.EthashMiner.MinerMsg
import io.iohk.ethereum.consensus.ethash.blocks.EthashBlockGenerator
import io.iohk.ethereum.consensus.ethash.validators.{EthashValidators, StdEthashValidators}
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.{Block, BlockchainImpl}
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockExecutionSuccess}
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.VM

/**
 * Implements standard Ethereum consensus (ethash PoW).
 */
class EthashConsensus(
  vm: VM,
  blockchain: BlockchainImpl,
  blockchainConfig: BlockchainConfig,
  fullConsensusConfig: FullConsensusConfig[EthashConfig],
  _validators: EthashValidators
) extends ConsensusImpl[EthashConfig](
  vm,
  blockchain,
  blockchainConfig,
  fullConsensusConfig
) {

  private[this] val _blockGenerator = new EthashBlockGenerator(
    validators = this._validators,
    blockchain = blockchain,
    blockchainConfig = blockchainConfig,
    consensusConfig = fullConsensusConfig.generic,
    blockPreparator = this._blockPreparator
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
  def withValidators(validators: Validators): TestConsensus = {
    validators match {
      case ethashValidators: EthashValidators ⇒
        new EthashConsensus(
          vm,
          blockchain,
          blockchainConfig,
          fullConsensusConfig,
          ethashValidators
        )

      case _ ⇒
        wrongValidatorsArgument[EthashValidators](validators)
    }
  }


  def withVM(vm: VM): TestConsensus =
    new EthashConsensus(
      vm,
      blockchain,
      blockchainConfig,
      fullConsensusConfig,
      validators
    )

  /**
   * Returns the [[io.iohk.ethereum.consensus.blocks.BlockGenerator BlockGenerator]]
   * this consensus protocol uses.
   */
  def blockGenerator: EthashBlockGenerator = this._blockGenerator

  /**
   * Override the default validation of
   * [[io.iohk.ethereum.consensus.ConsensusImpl#validateBlockBeforeExecution(io.iohk.ethereum.domain.Block, scala.Function1, scala.Function2) ConsensusImpl#validateBlockBeforeExecution]]
   * by also [[io.iohk.ethereum.consensus.ethash.validators.EthashValidators#ommersValidator() validating ommers]].
   *
   * @see [[io.iohk.ethereum.consensus.ethash.validators.EthashValidators EthashValidators]].
   */
  //noinspection ScalaStyle
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
  ): EthashConsensus =
    new EthashConsensus(
      vm,
      blockchain,
      blockchainConfig,
      fullConsensusConfig,
      StdEthashValidators(blockchainConfig)
    )
}
