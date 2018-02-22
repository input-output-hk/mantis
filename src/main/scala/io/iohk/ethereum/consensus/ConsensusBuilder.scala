package io.iohk.ethereum.consensus

import io.iohk.ethereum.consensus.atomixraft.AtomixRaftConsensus
import io.iohk.ethereum.nodebuilder.BlockchainConfigBuilder
import io.iohk.ethereum.utils.{Config, Logger}

/**
 * A consensus builder is responsible to instantiate the consensus protocol.
 * This is done dynamically when Mantis boots, based on its configuration.
 *
 * @see [[io.iohk.ethereum.consensus.Consensus Consensus]]
 */
trait ConsensusBuilder {
  self: BlockchainConfigBuilder with ConsensusConfigBuilder with Logger =>

  private lazy val mantisConfig = Config.config

  private def newConfig[C <: AnyRef](c: C): FullConsensusConfig[C] =
    FullConsensusConfig(consensusConfig, c)

  private def loadEthashConsensus(): ethash.EthashConsensus = {
    val specificConfig = ethash.EthashConfig(mantisConfig)
    val fullConfig = newConfig(specificConfig)
    val consensus = new ethash.EthashConsensus(blockchainConfig, fullConfig)
    consensus
  }

  private def loadDemoConsensus(): demo.DemoConsensus = {
    val specificConfig = demo.DemoConsensusConfig(mantisConfig)
    val fullConfig = newConfig(specificConfig)
    val consensus = new demo.DemoConsensus(blockchainConfig, fullConfig)
    consensus
  }

  private def loadAtomixRaftConsensus(): atomixraft.AtomixRaftConsensus = {
    val specificConfig = atomixraft.AtomixRaftConfig(mantisConfig)
    val fullConfig = newConfig(specificConfig)
    val consensus = new AtomixRaftConsensus(blockchainConfig, fullConfig)
    consensus
  }

  private def loadConsensus(): Consensus = {
    val config = consensusConfig
    val protocol = config.protocol
    log.info(s"Configured consensus protocol: ${protocol.name}")

    val consensus =
      config.protocol match {
        case Protocol.Ethash ⇒ loadEthashConsensus()
        case Protocol.Demo0 ⇒ loadDemoConsensus()
        case Protocol.AtomixRaft ⇒ loadAtomixRaftConsensus()
      }
    log.info(s"'${protocol.name}' protocol implemented by ${consensus.getClass.getName}")

    consensus
  }

  lazy val consensus: Consensus = loadConsensus()
}
