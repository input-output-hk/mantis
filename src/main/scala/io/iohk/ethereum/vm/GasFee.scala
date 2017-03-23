package io.iohk.ethereum.vm

// scalastyle:off magic.number
object GasFee {
  /**
    * Used to artificially limit memory usage by incurring maximum gas cost.
    * Consider moving to configuration
    */
  val MaxMemory: UInt256 = Int.MaxValue

  /**
    * Calculate gas cost of memory usage. Incur a blocking gas cost if memory usage exceeds reasonable limits.
    *
    * @param memSize  current memory size in bytes
    * @param offset   memory offset to be written/read
    * @param dataSize size of data to be written/read in bytes
    * @return gas cost
    */
  def calcMemCost(memSize: UInt256, offset: UInt256, dataSize: UInt256): UInt256 = {
    val memNeeded = if (dataSize.isZero) UInt256.Zero else offset + dataSize

    if (memNeeded > MaxMemory)
      UInt256.MaxValue / 2
    else if (memNeeded <= memSize)
      0
    else
      c(memNeeded) - c(memSize)
  }

  /** See YP H.1 (222) */
  private def c(m: UInt256): UInt256 = {
    val a = wordsForBytes(m)
    G_memory * a + a * a / 512
  }

  /**
    * Number of 32-byte DataWords required to hold n bytes (~= math.ceil(n / 32))
    */
  def wordsForBytes(n: UInt256): UInt256 =
   if (n.isZero) 0 else (n - 1) / UInt256.Size + 1


  // See YP, appendix G
  val G_zero           = UInt256(0)
  val G_base           = UInt256(2)
  val G_verylow        = UInt256(3)
  val G_low            = UInt256(5)
  val G_mid            = UInt256(8)
  val G_high           = UInt256(10)
  val G_extcode        = UInt256(700)
  val G_balance        = UInt256(400)
  val G_sload          = UInt256(200)
  val G_jumpdest       = UInt256(1)
  val G_sset           = UInt256(20000)
  val G_sreset         = UInt256(5000)
  val R_sclear         = UInt256(15000)
  val R_selfdestruct   = UInt256(24000)
  val G_selfdestruct   = UInt256(5000)
  val G_create         = UInt256(32000)
  val G_codedeposit    = UInt256(200)
  val G_call           = UInt256(700)
  val G_callvalue      = UInt256(9000)
  val G_callstipend    = UInt256(2300)
  val G_newaccount     = UInt256(25000)
  val G_exp            = UInt256(10)
  val G_expbyte        = UInt256(10)
  val G_memory         = UInt256(3)
  val G_txcreate       = UInt256(32000)
  val G_txdatazero     = UInt256(4)
  val G_txdatanonzero  = UInt256(68)
  val G_transaction    = UInt256(21000)
  val G_log            = UInt256(375)
  val G_logdata        = UInt256(8)
  val G_logtopic       = UInt256(375)
  val G_sha3           = UInt256(30)
  val G_sha3word       = UInt256(6)
  val G_copy           = UInt256(3)
  val G_blockhash      = UInt256(20)
}
