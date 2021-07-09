package io.iohk.ethereum.consensus.mining

import io.iohk.ethereum.utils.Config

trait MiningConfigBuilder {
  protected def buildMiningConfig(): MiningConfig = MiningConfig(Config.config)

  lazy val miningConfig: MiningConfig = buildMiningConfig()
}
