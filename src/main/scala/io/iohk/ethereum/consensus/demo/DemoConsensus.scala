package io.iohk.ethereum.consensus
package demo

import io.iohk.ethereum.consensus.demo.blocks.DemoBlockGenerator
import io.iohk.ethereum.consensus.validators.std.StdValidators
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.VM

/**
 * Ridiculously simple (non-)consensus.
 * A node with the stake declares to be the leader.
 * All other nodes know about it via hard-coded configuration.
 */
class DemoConsensus(
  vm: VM,
  blockchain: BlockchainImpl,
  blockchainConfig: BlockchainConfig,
  fullConsensusConfig: FullConsensusConfig[DemoConsensusConfig],
  _validators: StdValidators
) extends ConsensusImpl[DemoConsensusConfig](
  vm,
  blockchain,
  blockchainConfig,
  fullConsensusConfig
) {

  type Validators = StdValidators

  private[this] val _blockGenerator = new DemoBlockGenerator(
    blockchain = blockchain,
    blockchainConfig = blockchainConfig,
    consensusConfig = fullConsensusConfig.generic,
    blockPreparator = this._blockPreparator
  )

  /**
   * Starts the consensus protocol on the current `node`.
   */
  def startProtocol(node: Node): Unit = {
    if(config.miningEnabled) {
      startMiningProcess(node)
    }
    log.info(s"Howdy, started consensus protocol $this")
  }

  def stopProtocol(): Unit = {}

  private[this] def startMiningProcess(node: Node): Unit = {
    val miner = DemoConsensusMiner(node)
    miner ! DemoConsensusMiner.StartMining
  }

  def protocol: Protocol = Protocol.Demo0

  /**
   * Provides the set of validators specific to this consensus protocol.
   */
  def validators: Validators = this._validators

  def withValidators(validators: StdValidators): DemoConsensus =
    new DemoConsensus(
      vm,
      blockchain,
      blockchainConfig,
      fullConsensusConfig,
      validators
    )

  def withVM(vm: VM): Consensus =
    new DemoConsensus(
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
  def blockGenerator: DemoBlockGenerator = this._blockGenerator
}

object DemoConsensus {
  def apply(
    vm: VM,
    blockchain: BlockchainImpl,
    blockchainConfig: BlockchainConfig,
    fullConsensusConfig: FullConsensusConfig[DemoConsensusConfig]
  ): DemoConsensus =
    new DemoConsensus(
      vm,
      blockchain,
      blockchainConfig,
      fullConsensusConfig,
      StdValidators(blockchainConfig)
    )
}
