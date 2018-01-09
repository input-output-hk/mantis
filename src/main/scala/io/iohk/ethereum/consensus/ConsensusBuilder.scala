package io.iohk.ethereum.consensus

import io.iohk.ethereum.nodebuilder.{BlockchainConfigBuilder, ShutdownHookBuilder}
import io.iohk.ethereum.utils.{Config, Logger}

/**
 * A consensus builder is responsible to instantiate the consensus protocol.
 */
trait ConsensusBuilder {
  self: BlockchainConfigBuilder with ConsensusConfigBuilder with Logger =>

  private lazy val mantisConfig = Config.config

  private def loadEthashConsensus(): ethash.EthashConsensus = {
    val config = ethash.MiningConfig(mantisConfig)
    val consensus = new ethash.EthashConsensus(blockchainConfig, config)
    consensus
  }

  private def loadDemoConsensus(): demo.DemoConsensus = {
    val config = demo.DemoConsensusConfig(mantisConfig)
    val consensus = new demo.DemoConsensus(blockchainConfig, config)
    consensus
  }

  private def loadConsensus(): Consensus = {
    val config = consensusConfig
    val protocol = config.protocol
    log.info(s"Configured consensus protocol: ${protocol.name}")

    val consensus =
      config.protocol match {
        case Ethash ⇒ loadEthashConsensus()
        case DemoPoS ⇒ loadDemoConsensus()
      }
    log.info(s"${protocol.name} protocol implemented by ${consensus.getClass.getName}")

    consensus
  }

  lazy val consensus: Consensus = loadConsensus()
}

trait ConsensusConfigBuilder { self: ShutdownHookBuilder ⇒
  lazy val consensusConfig: ConsensusConfig = ConsensusConfig(Config.config)(this)
}
