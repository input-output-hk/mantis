package io.iohk.ethereum.consensus
package demo
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import io.iohk.ethereum.validators.{BlockHeaderValidator, BlockHeaderValidatorImpl}

/**
 * Ridiculously simple (non-)consensus.
 * A node with the stake declares to be the leader.
 * All other nodes know about it via hard-coded configuration.
 */
class DemoConsensus(
  blockchainConfig: BlockchainConfig,
  demoConsensusConfig: DemoConsensusConfig
) extends Consensus with Logger {

  private[this] val defaultValidator = new BlockHeaderValidatorImpl(blockchainConfig)

  /**
   * Starts the consensus protocol on the current `node`.
   * This call must be made before `startMiningProcess`.
   */
  def startProtocol(node: Node): Unit = {
    log.info(s"Howdy, started consensus protocol $this")
  }

  /**
   * Starts the mining process on the current `node`.
   * It is up to the consensus protocol to define the semantics of mining.
   */
  def startMiningProcess(node: Node): Unit = {
    val minerBuilder = new DemoConsensusMinerBuilder(node, demoConsensusConfig)
    val miner = minerBuilder.miner
    miner ! DemoConsensusMiner.StartMining
  }

  /**
   * Provides the [[io.iohk.ethereum.validators.BlockHeaderValidator BlockHeaderValidator]] that is specific
   * to this consensus protocol.
   */
  def blockHeaderValidator: BlockHeaderValidator = defaultValidator
}
