package io.iohk.ethereum.txExecTest

import io.iohk.ethereum.blockchain.sync
import io.iohk.ethereum.domain.{BlockchainImpl, BlockchainStorages}
import io.iohk.ethereum.vm.VM

trait ScenarioSetup extends sync.ScenarioSetup {
  protected val testBlockchainStorages: BlockchainStorages

  override lazy val blockchain: BlockchainImpl = BlockchainImpl(testBlockchainStorages)
  override lazy val vm: VM = VM
}
