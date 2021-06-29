package io.iohk.ethereum.txExecTest

import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.{BlockchainImpl, BlockchainMetadata, BlockchainReader, BlockchainStorages}
import io.iohk.ethereum.ledger.VMImpl

trait ScenarioSetup extends EphemBlockchainTestSetup {
  protected val testBlockchainStorages: BlockchainStorages

  override lazy val blockchainMetadata = new BlockchainMetadata(
    testBlockchainStorages.appStateStorage.getBestBlockNumber(),
    testBlockchainStorages.appStateStorage.getLatestCheckpointBlockNumber()
  )
  override lazy val blockchainReader: BlockchainReader = BlockchainReader(testBlockchainStorages)
  override lazy val blockchain: BlockchainImpl =
    BlockchainImpl(testBlockchainStorages, blockchainReader, blockchainMetadata)
  override lazy val vm: VMImpl = new VMImpl
}
