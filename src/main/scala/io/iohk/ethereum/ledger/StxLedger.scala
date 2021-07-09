package io.iohk.ethereum.ledger

import scala.annotation.tailrec

import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.SignedTransactionWithSender
import io.iohk.ethereum.ledger.TxResult
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.EvmConfig

class StxLedger(
    blockchain: BlockchainImpl,
    blockchainReader: BlockchainReader,
    evmCodeStorage: EvmCodeStorage,
    blockchainConfig: BlockchainConfig,
    blockPreparator: BlockPreparator
) {

  def simulateTransaction(
      stx: SignedTransactionWithSender,
      blockHeader: BlockHeader,
      world: Option[InMemoryWorldStateProxy]
  ): TxResult = {
    val tx = stx.tx

    val world1 = world.getOrElse(
      InMemoryWorldStateProxy(
        evmCodeStorage = evmCodeStorage,
        mptStorage = blockchain.getReadOnlyMptStorage(),
        getBlockHashByNumber = (number: BigInt) =>
          blockchainReader.getBlockHeaderByNumber(blockchainReader.getBestBranch(), number).map(_.hash),
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
      StxLedger.binaryChop(lowLimit, highLimit) { gasLimit =>
        simulateTransaction(stx.copy(tx = tx.copy(tx = tx.tx.copy(gasLimit = gasLimit))), blockHeader, world).vmError
      }
    }
  }
}

object StxLedger {

  /** Function finds minimal value in some interval for which provided function do not return error
    * If searched value is not in provided interval, function returns maximum value of searched interval
    * @param min minimum of searched interval
    * @param max maximum of searched interval
    * @param f function which return error in case to little value provided
    * @return minimal value for which provided function do not return error
    */
  @tailrec
  private[ledger] def binaryChop[Error](min: BigInt, max: BigInt)(f: BigInt => Option[Error]): BigInt = {
    assert(min <= max)

    if (min == max)
      max
    else {
      val mid = min + (max - min) / 2
      val possibleError = f(mid)
      if (possibleError.isEmpty)
        binaryChop(min, mid)(f)
      else
        binaryChop(mid + 1, max)(f)
    }
  }
}
