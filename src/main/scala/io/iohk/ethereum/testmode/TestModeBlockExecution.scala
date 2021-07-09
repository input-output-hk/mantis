package io.iohk.ethereum.testmode

import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.BlockchainWriter
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.ledger.BlockExecution
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.ledger.BlockValidation
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.EvmConfig

class TestModeBlockExecution(
    blockchain: BlockchainImpl,
    blockchainReader: BlockchainReader,
    blockchainWriter: BlockchainWriter,
    evmCodeStorage: EvmCodeStorage,
    blockchainConfig: BlockchainConfig,
    blockPreparator: BlockPreparator,
    blockValidation: BlockValidation,
    saveStoragePreimage: (UInt256) => Unit
) extends BlockExecution(
      blockchain,
      blockchainReader,
      blockchainWriter,
      evmCodeStorage,
      blockchainConfig,
      blockPreparator,
      blockValidation
    ) {

  override protected def buildInitialWorld(block: Block, parentHeader: BlockHeader): InMemoryWorldStateProxy =
    TestModeWorldStateProxy(
      evmCodeStorage = evmCodeStorage,
      nodesKeyValueStorage = blockchain.getBackingMptStorage(block.header.number),
      getBlockHashByNumber = (number: BigInt) =>
        blockchainReader.getBlockHeaderByNumber(blockchainReader.getBestBranch(), number).map(_.hash),
      accountStartNonce = blockchainConfig.accountStartNonce,
      stateRootHash = parentHeader.stateRoot,
      noEmptyAccounts = EvmConfig.forBlock(parentHeader.number, blockchainConfig).noEmptyAccounts,
      ethCompatibleStorage = blockchainConfig.ethCompatibleStorage,
      saveStoragePreimage = saveStoragePreimage
    )
}
