package io.iohk.ethereum.txExecTest

import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.BlockchainStorages
import io.iohk.ethereum.ledger.VMImpl

trait ScenarioSetup extends EphemBlockchainTestSetup {
  protected val testBlockchainStorages: BlockchainStorages

  override lazy val blockchainReader: BlockchainReader = BlockchainReader(testBlockchainStorages)
  override lazy val blockchain: BlockchainImpl = BlockchainImpl(testBlockchainStorages, blockchainReader)
  override lazy val vm: VMImpl = new VMImpl
}
