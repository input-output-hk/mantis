package io.iohk.ethereum.consensus

import io.iohk.ethereum.consensus.Protocol.{NoAdditionalEthashData, RestrictedEthashMinerData}
import io.iohk.ethereum.consensus.ethash.EthashConsensus
import io.iohk.ethereum.consensus.ethash.validators.ValidatorsExecutor
import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.utils.{Config, Logger}

trait ConsensusBuilder {
  def consensus: Consensus
}

/** A consensus builder is responsible to instantiate the consensus protocol.
  * This is done dynamically when Mantis boots, based on its configuration.
  *
  * @see [[io.iohk.ethereum.consensus.Consensus Consensus]],
  *      [[io.iohk.ethereum.consensus.ethash.EthashConsensus EthashConsensus]],
  */
trait StdConsensusBuilder extends ConsensusBuilder {
  self: VmBuilder
    with BlockchainBuilder
    with BlockchainConfigBuilder
    with ConsensusConfigBuilder
    with NodeKeyBuilder
    with Logger =>

  private lazy val mantisConfig = Config.config

  private def newConfig[C <: AnyRef](c: C): FullConsensusConfig[C] =
    FullConsensusConfig(consensusConfig, c)

  //TODO [ETCM-397] refactor configs to avoid possibility of running mocked or
  // restricted-ethash consensus on real network like ETC or Mordor
  protected def buildEthashConsensus(): ethash.EthashConsensus = {
    val specificConfig = ethash.EthashConfig(mantisConfig)

    val fullConfig = newConfig(specificConfig)

    val validators = ValidatorsExecutor(blockchainConfig, consensusConfig.protocol)

    val additionalEthashData = consensusConfig.protocol match {
      case Protocol.Ethash | Protocol.MockedPow => NoAdditionalEthashData
      case Protocol.RestrictedEthash            => RestrictedEthashMinerData(nodeKey)
    }
    val consensus = EthashConsensus(vm, blockchain, blockchainConfig, fullConfig, validators, additionalEthashData)
    consensus
  }

  protected def buildConsensus(): Consensus = {
    val config = consensusConfig
    val protocol = config.protocol

    val consensus =
      config.protocol match {
        case Protocol.Ethash | Protocol.MockedPow | Protocol.RestrictedEthash => buildEthashConsensus()
      }

    log.info(s"Using '${protocol.name}' consensus [${consensus.getClass.getName}]")

    consensus
  }

  lazy val consensus: Consensus = buildConsensus()
}
