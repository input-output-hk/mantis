package io.iohk.ethereum.consensus

import io.iohk.ethereum.nodebuilder.ShutdownHookBuilder
import io.iohk.ethereum.utils.Config

trait ConsensusConfigBuilder { self: ShutdownHookBuilder ⇒
  lazy val consensusConfig: ConsensusConfig = ConsensusConfig(Config.config)(this)
}
