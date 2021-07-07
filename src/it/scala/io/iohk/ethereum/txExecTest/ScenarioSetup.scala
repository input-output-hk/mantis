package io.iohk.ethereum.txExecTest

import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.BlockchainStorages
import io.iohk.ethereum.domain.BlockchainWriter
import io.iohk.ethereum.ledger.VMImpl

trait ScenarioSetup extends EphemBlockchainTestSetup {
  protected val testBlockchainStorages: BlockchainStorages

  val blockchainMetadata = getNewBlockchainMetadata
  override lazy val blockchainReader: BlockchainReader = BlockchainReader(testBlockchainStorages, blockchainMetadata)
  override lazy val blockchainWriter: BlockchainWriter = BlockchainWriter(testBlockchainStorages, blockchainMetadata)
  override lazy val blockchain: BlockchainImpl =
    BlockchainImpl(testBlockchainStorages, blockchainReader, blockchainMetadata)
  override lazy val vm: VMImpl = new VMImpl
}
