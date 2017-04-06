package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.vm.FeeSchedule.Key._

object GasFee {

  /**
    * Calculate gas cost of memory usage. Incur a blocking gas cost if memory usage exceeds reasonable limits.
    *
    * @param memSize  current memory size in bytes
    * @param offset   memory offset to be written/read
    * @param dataSize size of data to be written/read in bytes
    * @param config evm config
    * @return gas cost
    */
  def calcMemCost(memSize: UInt256, offset: UInt256, dataSize: UInt256, config: EvmConfig): UInt256 = {
    /** See YP H.1 (222) */
    def c(m: UInt256): UInt256 = {
      val a = wordsForBytes(m)
      config.feeSchedule(G_memory) * a + a * a / 512
    }

    val memNeeded = if (dataSize.isZero) UInt256.Zero else offset + dataSize
    if (memNeeded > config.maxMemory)
      UInt256.MaxValue / 2
    else if (memNeeded <= memSize)
      0
    else
      c(memNeeded) - c(memSize)
  }

  /**
    * Calculates transaction intrinsic gas. See YP section 6.2
    *
    */
  def calcTransactionIntrinsicGas(txData: ByteString, isContractCreation: Boolean, blockNumber: BigInt, config: EvmConfig): BigInt = {
    val txDataZero = txData.count(_ == 0)
    val txDataNonZero = txData.length - txDataZero

    val txCreateGas = if (isContractCreation) config.feeSchedule(G_txcreate) else UInt256(0)

    txDataZero * config.feeSchedule(G_txdatazero) +
    txDataNonZero * config.feeSchedule(G_txdatanonzero) +
    txCreateGas +
    config.feeSchedule(G_transaction)
  }

  /**
    * If the initialization code completes successfully, a final contract-creation cost is paid, the code-deposit cost,
    * proportional to the size of the created contract’s code. See YP equation (96)
    *
    * @param executionResultData Transaction code initialization result
    * @return Calculated gas cost
    */
  def calcCodeDepositCost(executionResultData: ByteString, config: EvmConfig): BigInt =
    config.feeSchedule(G_codedeposit) * executionResultData.size

  /**
    * Number of 32-byte UInt256s required to hold n bytes (~= math.ceil(n / 32))
    */
  def wordsForBytes(n: UInt256): UInt256 =
   if (n.isZero) 0 else (n - 1) / UInt256.Size + 1
}
