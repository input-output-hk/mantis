package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.consensus.{ConsensusBuilder, ConsensusConfigBuilder}
import io.iohk.ethereum.nodebuilder.{BlockchainConfigBuilder, ShutdownHookBuilder}
import io.iohk.ethereum.utils.Logger

trait ScenarioSetup extends BlockchainConfigBuilder
  with ConsensusBuilder
  with ConsensusConfigBuilder
  with ShutdownHookBuilder
  with Logger
