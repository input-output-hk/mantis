package io.iohk.ethereum.consensus

import io.iohk.ethereum.consensus.Protocol.NoAdditionalPoWData
import io.iohk.ethereum.consensus.Protocol.RestrictedPoWMinerData
import io.iohk.ethereum.consensus.pow.PoWMining
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Logger

trait MiningBuilder {
  def mining: Mining
}

/** A consensus builder is responsible to instantiate the consensus protocol.
  * This is done dynamically when Mantis boots, based on its configuration.
  *
  * @see [[io.iohk.ethereum.consensus.Mining Consensus]],
  *      [[io.iohk.ethereum.consensus.pow.PoWMining PoWConsensus]],
  */
trait StdMiningBuilder extends MiningBuilder {
  self: VmBuilder
    with StorageBuilder
    with BlockchainBuilder
    with BlockchainConfigBuilder
    with MiningConfigBuilder
    with NodeKeyBuilder
    with Logger =>

  private lazy val mantisConfig = Config.config

  private def newConfig[C <: AnyRef](c: C): FullConsensusConfig[C] =
    FullConsensusConfig(miningConfig, c)

  //TODO [ETCM-397] refactor configs to avoid possibility of running mocked or
  // restricted-pow consensus on real network like ETC or Mordor
  protected def buildPoWConsensus(): pow.PoWMining = {
    val specificConfig = pow.EthashConfig(mantisConfig)

    val fullConfig = newConfig(specificConfig)

    val validators = ValidatorsExecutor(blockchainConfig, miningConfig.protocol)

    val additionalPoWData = miningConfig.protocol match {
      case Protocol.PoW | Protocol.MockedPow => NoAdditionalPoWData
      case Protocol.RestrictedPoW            => RestrictedPoWMinerData(nodeKey)
    }

    val mining =
      PoWMining(
        vm,
        storagesInstance.storages.evmCodeStorage,
        blockchain,
        blockchainReader,
        blockchainConfig,
        fullConfig,
        validators,
        additionalPoWData
      )

    mining
  }

  protected def buildMining(): Mining = {
    val config = miningConfig
    val protocol = config.protocol

    val mining =
      config.protocol match {
        case Protocol.PoW | Protocol.MockedPow | Protocol.RestrictedPoW => buildPoWConsensus()
      }

    log.info(s"Using '${protocol.name}' mining protocol [${mining.getClass.getName}]")

    mining
  }

  lazy val mining: Mining = buildMining()
}
