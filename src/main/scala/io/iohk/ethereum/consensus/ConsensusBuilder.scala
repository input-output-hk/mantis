package io.iohk.ethereum.consensus

import io.iohk.ethereum.consensus.atomixraft.AtomixRaftConsensus
import io.iohk.ethereum.consensus.demo.DemoConsensus
import io.iohk.ethereum.consensus.ethash.EthashConsensus
import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.utils.{Config, Logger}

/**
 * A consensus builder is responsible to instantiate the consensus protocol.
 * This is done dynamically when Mantis boots, based on its configuration.
 *
 * @see [[io.iohk.ethereum.consensus.Consensus Consensus]]
 */
trait ConsensusBuilder {
  self: VmBuilder with BlockchainBuilder with BlockchainConfigBuilder with ConsensusConfigBuilder with Logger ⇒

  private lazy val mantisConfig = Config.config

  private def newConfig[C <: AnyRef](c: C): FullConsensusConfig[C] =
    FullConsensusConfig(consensusConfig, c)

  private def loadEthashConsensus(): ethash.EthashConsensus = {
    val specificConfig = ethash.EthashConfig(mantisConfig)
    val fullConfig = newConfig(specificConfig)
    val consensus = EthashConsensus(vm, blockchain, blockchainConfig, fullConfig)
    consensus
  }

  private def loadDemoConsensus(): demo.DemoConsensus = {
    val specificConfig = demo.DemoConsensusConfig(mantisConfig)
    val fullConfig = newConfig(specificConfig)
    val consensus = DemoConsensus(vm, blockchain, blockchainConfig, fullConfig)
    consensus
  }

  private def loadAtomixRaftConsensus(): atomixraft.AtomixRaftConsensus = {
    val specificConfig = atomixraft.AtomixRaftConfig(mantisConfig)
    val fullConfig = newConfig(specificConfig)
    val consensus = AtomixRaftConsensus(vm, blockchain, blockchainConfig, fullConfig)
    consensus
  }

  protected def loadConsensus(): Consensus = {
    val config = consensusConfig
    val protocol = config.protocol

    val consensus =
      config.protocol match {
        case Protocol.Ethash ⇒ loadEthashConsensus()
        case Protocol.Demo0 ⇒ loadDemoConsensus()
        case Protocol.AtomixRaft ⇒ loadAtomixRaftConsensus()
      }
    log.info(s"Using '${protocol.name}' consensus [${consensus.getClass.getName}]")

    consensus
  }

  lazy val consensus: Consensus = loadConsensus()
}

/** A standard [[io.iohk.ethereum.consensus.ConsensusBuilder ConsensusBuilder]] cake. */
trait StdConsensusBuilder extends ConsensusBuilder
  with VmBuilder
  with BlockchainBuilder
  with StorageBuilder
  with BlockchainConfigBuilder
  with ConsensusConfigBuilder
  with ShutdownHookBuilder
  with Logger
