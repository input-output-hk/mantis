package io.iohk.ethereum.consensus

import io.iohk.ethereum.utils.Config

trait ConsensusConfigBuilder {
  protected def buildConsensusConfig(): ConsensusConfig = ConsensusConfig(Config.config)

  lazy val consensusConfig: ConsensusConfig = buildConsensusConfig()
}
