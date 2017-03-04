package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.utils.Config

// scalastyle:off magic.number
object GasFee {
  /**
    * Used to artificially limit memory usage by incurring maximum gas cost.
    * Consider moving to configuration
    */
  val MaxMemory: Long = Int.MaxValue

  /**
    * Calculate gas cost of memory usage. Incur a blocking gas cost if memory usage exceeds reasonable limits.
    *
    * @param memSize  current memory size in bytes
    * @param addr     memory address to be written/read
    * @param dataSize size of data to be written/read in bytes
    * @return gas cost
    */
  def calcMemCost(memSize: BigInt, addr: BigInt, dataSize: BigInt): BigInt = {
    val memNeeded = if (dataSize == 0) BigInt(0) else addr + dataSize

    if (memNeeded > MaxMemory)
      DataWord.MaxValue
    else if (memNeeded <= memSize)
      0
    else
      c(memNeeded) - c(memSize)
  }

  /**
    * Calculates transaction intrinsic gas. See YP section 6.2
    *
    */
  def calcTransactionIntrinsicGas(txData: ByteString, isContractCreation: Boolean, blockNumber: BigInt): BigInt = {
    val txDataZero = txData.count(_ == 0)
    val txDataNonZero = txData.length - txDataZero

    txDataZero * G_txdatazero +
    txDataNonZero * G_txdatanonzero +
    (if(isContractCreation && blockNumber > Config.Blockchain.HomesteadBlock) G_txcreate else 0 ) +
    G_transaction
  }

  /**
    * If the initialization code completes successfully, a final contract-creation cost is paid, the code-deposit cost,
    * proportional to the size of the created contract’s code. See YP equation (96)
    *
    * @param executionResultData Transaction code initialization result
    * @return Calculated gas cost
    */
  def calcCodeDepositCost(executionResultData: ByteString): BigInt = G_codedeposit * executionResultData.size

  /** See YP H.1 (222) */
  private def c(m: BigInt): BigInt = {
    val a = wordsForBytes(m)
    G_memory * a + a * a / 512
  }

  /**
    * Number of 32-byte DataWords required to hold n bytes (~= math.ceil(n / 32))
    */
  def wordsForBytes(n: BigInt): BigInt =
   if (n == 0) 0 else (n - 1) / DataWord.Size + 1


  // See YP, appendix G
  val G_zero           = BigInt(0)
  val G_base           = BigInt(2)
  val G_verylow        = BigInt(3)
  val G_low            = BigInt(5)
  val G_mid            = BigInt(8)
  val G_high           = BigInt(10)
  val G_extcode        = BigInt(700)
  val G_balance        = BigInt(400)
  val G_sload          = BigInt(200)
  val G_jumpdest       = BigInt(1)
  val G_sset           = BigInt(20000)
  val G_sreset         = BigInt(5000)
  val R_sclear         = BigInt(15000)
  val R_suicide        = BigInt(24000)
  val G_suicide        = BigInt(5000)
  val G_create         = BigInt(32000)
  val G_codedeposit    = BigInt(200)
  val G_call           = BigInt(700)
  val G_callvalue      = BigInt(9000)
  val G_callstipend    = BigInt(2300)
  val G_newaccount     = BigInt(25000)
  val G_exp            = BigInt(10)
  val G_expbyte        = BigInt(10)
  val G_memory         = BigInt(3)
  val G_txcreate       = BigInt(32000)
  val G_txdatazero     = BigInt(4)
  val G_txdatanonzero  = BigInt(68)
  val G_transaction    = BigInt(21000)
  val G_log            = BigInt(375)
  val G_logdata        = BigInt(8)
  val G_logtopic       = BigInt(375)
  val G_sha3           = BigInt(30)
  val G_sha3word       = BigInt(6)
  val G_copy           = BigInt(3)
  val G_blockhash      = BigInt(20)
}
