package io.iohk.ethereum.consensus
package demo
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import io.iohk.ethereum.consensus.validators.{BlockHeaderValidator, BlockHeaderValidatorImpl}

/**
 * Ridiculously simple (non-)consensus.
 * A node with the stake declares to be the leader.
 * All other nodes know about it via hard-coded configuration.
 */
class DemoConsensus(
  blockchainConfig: BlockchainConfig,
  val config: FullConsensusConfig[DemoConsensusConfig]
) extends Consensus with Logger {

  private[this] val defaultValidator = new BlockHeaderValidatorImpl(blockchainConfig)

  type Config = DemoConsensusConfig

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
    val minerBuilder = new DemoConsensusMinerBuilder(node, config.specific)
    val miner = minerBuilder.miner
    miner ! DemoConsensusMiner.StartMining
  }

  def protocol: Protocol = Protocol.Demo0

  /**
   * Provides the [[io.iohk.ethereum.validators.BlockHeaderValidator BlockHeaderValidator]] that is specific
   * to this consensus protocol.
   */
  def blockHeaderValidator: BlockHeaderValidator = defaultValidator
}
