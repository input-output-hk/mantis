package io.iohk.ethereum.consensus

import io.iohk.ethereum.consensus.Protocol.NoAdditionalPoWData
import io.iohk.ethereum.consensus.Protocol.RestrictedPoWMinerData
import io.iohk.ethereum.consensus.pow.PoWConsensus
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Logger

trait ConsensusBuilder {
  def mining: Mining
}

/** A consensus builder is responsible to instantiate the consensus protocol.
  * This is done dynamically when Mantis boots, based on its configuration.
  *
  * @see [[io.iohk.ethereum.consensus.Mining Consensus]],
  *      [[io.iohk.ethereum.consensus.pow.PoWConsensus PoWConsensus]],
  */
trait StdConsensusBuilder extends ConsensusBuilder {
  self: VmBuilder
    with StorageBuilder
    with BlockchainBuilder
    with BlockchainConfigBuilder
    with ConsensusConfigBuilder
    with NodeKeyBuilder
    with Logger =>

  private lazy val mantisConfig = Config.config

  private def newConfig[C <: AnyRef](c: C): FullConsensusConfig[C] =
    FullConsensusConfig(consensusConfig, c)

  //TODO [ETCM-397] refactor configs to avoid possibility of running mocked or
  // restricted-pow consensus on real network like ETC or Mordor
  protected def buildPoWConsensus(): pow.PoWConsensus = {
    val specificConfig = pow.EthashConfig(mantisConfig)

    val fullConfig = newConfig(specificConfig)

    val validators = ValidatorsExecutor(blockchainConfig, consensusConfig.protocol)

    val additionalPoWData = consensusConfig.protocol match {
      case Protocol.PoW | Protocol.MockedPow => NoAdditionalPoWData
      case Protocol.RestrictedPoW            => RestrictedPoWMinerData(nodeKey)
    }
    val consensus =
      PoWConsensus(
        vm,
        storagesInstance.storages.evmCodeStorage,
        blockchain,
        blockchainReader,
        blockchainConfig,
        fullConfig,
        validators,
        additionalPoWData
      )
    consensus
  }

  protected def buildMining(): Mining = {
    val config = consensusConfig
    val protocol = config.protocol

    val consensus =
      config.protocol match {
        case Protocol.PoW | Protocol.MockedPow | Protocol.RestrictedPoW => buildPoWConsensus()
      }

    log.info(s"Using '${protocol.name}' consensus [${consensus.getClass.getName}]")

    consensus
  }

  lazy val mining: Mining = buildMining()
}
