package io.iohk.ethereum.consensus

import io.iohk.ethereum.utils.Config

trait ConsensusConfigBuilder {
  protected def buildConsensusConfig(): MiningConfig = MiningConfig(Config.config)

  lazy val miningConfig: MiningConfig = buildConsensusConfig()
}
