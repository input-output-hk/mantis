package io.iohk.ethereum.ledger

import io.iohk.ethereum.domain.{Account, BlockHeader, BlockchainImpl, SignedTransactionWithSender}
import io.iohk.ethereum.ledger.Ledger.TxResult
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.EvmConfig

class StxLedger(blockchain: BlockchainImpl, blockchainConfig: BlockchainConfig, blockPreparator: BlockPreparator) {

  def simulateTransaction(
      stx: SignedTransactionWithSender,
      blockHeader: BlockHeader,
      world: Option[InMemoryWorldStateProxy]
  ): TxResult = {
    val tx = stx.tx

    val world1 = world.getOrElse(
      blockchain.getReadOnlyWorldStateProxy(
        blockNumber = None,
        accountStartNonce = blockchainConfig.accountStartNonce,
        stateRootHash = blockHeader.stateRoot,
        noEmptyAccounts = EvmConfig.forBlock(blockHeader.number, blockchainConfig).noEmptyAccounts,
        ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
      )
    )

    val senderAddress = stx.senderAddress
    val world2 =
      if (world1.getAccount(senderAddress).isEmpty) {
        world1.saveAccount(senderAddress, Account.empty(blockchainConfig.accountStartNonce))
      } else {
        world1
      }

    val worldForTx = blockPreparator.updateSenderAccountBeforeExecution(tx, senderAddress, world2)
    val result = blockPreparator.runVM(tx, senderAddress, blockHeader, worldForTx)
    val totalGasToRefund = blockPreparator.calcTotalGasToRefund(tx, result)

    TxResult(result.world, tx.tx.gasLimit - totalGasToRefund, result.logs, result.returnData, result.error)
  }

  def binarySearchGasEstimation(
      stx: SignedTransactionWithSender,
      blockHeader: BlockHeader,
      world: Option[InMemoryWorldStateProxy]
  ): BigInt = {
    val lowLimit = EvmConfig.forBlock(blockHeader.number, blockchainConfig).feeSchedule.G_transaction
    val tx = stx.tx
    val highLimit = tx.tx.gasLimit

    if (highLimit < lowLimit) {
      highLimit
    } else {
      LedgerUtils.binaryChop(lowLimit, highLimit) { gasLimit =>
        simulateTransaction(stx.copy(tx = tx.copy(tx = tx.tx.copy(gasLimit = gasLimit))), blockHeader, world).vmError
      }
    }
  }

}
