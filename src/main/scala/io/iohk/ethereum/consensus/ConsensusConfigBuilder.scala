package io.iohk.ethereum.consensus

import io.iohk.ethereum.nodebuilder.ShutdownHookBuilder
import io.iohk.ethereum.utils.Config

trait ConsensusConfigBuilder { self: ShutdownHookBuilder ⇒
  protected def buildConsensusConfig(): ConsensusConfig = ConsensusConfig(Config.config)(this)

  lazy val consensusConfig: ConsensusConfig = buildConsensusConfig()
}
