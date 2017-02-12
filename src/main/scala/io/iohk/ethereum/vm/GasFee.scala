package io.iohk.ethereum.vm

// scalastyle:off magic.number
object GasFee {
  /**
    * Used to artificially limit memory usage by incurring maximum gas cost.
    * Consider moving to configuration
    */
  val MaxMemory: Long = Int.MaxValue

  /**
    * Calculate gas cost of memory usage. Incur a blocking gas cost if memory usage exceeds reasonable limits.
    * @param memSize current memory size in bytes
    * @param addr memory address to be written/read
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

  /** See YP H.1 (222) */
  private def c(m: BigInt): BigInt = {
    val a = wordsForBytes(m)
    G_memory.value * a + a * a / 512
  }

  /**
    * Number of 32-byte DataWords required to hold n bytes (~= math.ceil(n / 32))
    */
  def wordsForBytes(n: BigInt): BigInt =
    if (n == 0) 0 else (n - 1) / DataWord.Size + 1
}

/**
  * See YP, appendix G
  */
sealed abstract class GasFee(val value: BigInt)

case object G_zero          extends GasFee(0)
case object G_base          extends GasFee(2)
case object G_verylow       extends GasFee(3)
case object G_low           extends GasFee(5)
case object G_mid           extends GasFee(8)
case object G_high          extends GasFee(10)
case object G_extcode       extends GasFee(700)
case object G_balance       extends GasFee(400)
case object G_sload         extends GasFee(200)
case object G_jumpdest      extends GasFee(1)
case object G_sset          extends GasFee(20000)
case object G_sreset        extends GasFee(5000)
case object R_sclear        extends GasFee(15000)
case object R_suicide       extends GasFee(24000)
case object G_suicide       extends GasFee(5000)
case object G_create        extends GasFee(32000)
case object G_codedeposit   extends GasFee(200)
case object G_call          extends GasFee(700)
case object G_callvalue     extends GasFee(9000)
case object G_callstipend   extends GasFee(2300)
case object G_newaccount    extends GasFee(25000)
case object G_exp           extends GasFee(10)
case object G_expbyte       extends GasFee(10)
case object G_memory        extends GasFee(3)
case object G_txcreate      extends GasFee(32000)
case object G_txdatazero    extends GasFee(4)
case object G_txdatanonzero extends GasFee(68)
case object G_transaction   extends GasFee(21000)
case object G_log           extends GasFee(375)
case object G_logdata       extends GasFee(8)
case object G_logtopic      extends GasFee(375)
case object G_sha3          extends GasFee(30)
case object G_sha3word      extends GasFee(6)
case object G_copy          extends GasFee(3)
case object G_blockhash     extends GasFee(20)
