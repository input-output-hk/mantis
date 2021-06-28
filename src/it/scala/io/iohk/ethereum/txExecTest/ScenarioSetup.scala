package io.iohk.ethereum.txExecTest

import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.{BlockchainImpl, BlockchainStorages}
import io.iohk.ethereum.ledger.VMImpl

trait ScenarioSetup extends EphemBlockchainTestSetup {
  protected val testBlockchainStorages: BlockchainStorages
  override lazy val blockchain: BlockchainImpl = BlockchainImpl(testBlockchainStorages)
  override lazy val vm: VMImpl = new VMImpl
}
