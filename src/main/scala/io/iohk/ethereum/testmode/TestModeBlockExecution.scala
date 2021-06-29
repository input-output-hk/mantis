package io.iohk.ethereum.testmode

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.{Block, BlockHeader, BlockchainImpl, BlockchainReader, UInt256}
import io.iohk.ethereum.ledger.{BlockExecution, BlockPreparator, BlockValidation, InMemoryWorldStateProxy}
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.EvmConfig

class TestModeBlockExecution(
    blockchain: BlockchainImpl,
    blockchainReader: BlockchainReader,
    evmCodeStorage: EvmCodeStorage,
    blockchainConfig: BlockchainConfig,
    blockPreparator: BlockPreparator,
    blockValidation: BlockValidation,
    saveStoragePreimage: (UInt256) => Unit
) extends BlockExecution(
      blockchain,
      blockchainReader,
      evmCodeStorage,
      blockchainConfig,
      blockPreparator,
      blockValidation
    ) {

  override protected def buildInitialWorld(block: Block, parentHeader: BlockHeader): InMemoryWorldStateProxy =
    TestModeWorldStateProxy(
      evmCodeStorage = evmCodeStorage,
      nodesKeyValueStorage = blockchain.getBackingMptStorage(block.header.number),
      getBlockHashByNumber = (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      accountStartNonce = blockchainConfig.accountStartNonce,
      stateRootHash = parentHeader.stateRoot,
      noEmptyAccounts = EvmConfig.forBlock(parentHeader.number, blockchainConfig).noEmptyAccounts,
      ethCompatibleStorage = blockchainConfig.ethCompatibleStorage,
      saveStoragePreimage = saveStoragePreimage
    )
}
