package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.consensus.{ConsensusBuilder, ConsensusConfigBuilder}
import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.vm.VM

trait ScenarioSetup extends BlockchainBuilder
  with StorageBuilder
  with BlockchainConfigBuilder
  with VmBuilder
  with ConsensusBuilder
  with ConsensusConfigBuilder
  with ShutdownHookBuilder
  with Logger {

  def vm: VM = VM
}
