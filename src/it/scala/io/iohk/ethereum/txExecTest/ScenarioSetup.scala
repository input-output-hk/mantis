package io.iohk.ethereum.txExecTest

import io.iohk.ethereum.consensus.StdConsensusBuilder
import io.iohk.ethereum.domain.{BlockchainImpl, BlockchainStorages}
import io.iohk.ethereum.vm.VM

trait ScenarioSetup extends StdConsensusBuilder {
  protected val testBlockchainStorages: BlockchainStorages
  override lazy val blockchain: BlockchainImpl = BlockchainImpl(testBlockchainStorages)

  def vm: VM = VM
  def VM: VM = vm // to avoid refactoring every test case that uses a literal `VM` value
}
