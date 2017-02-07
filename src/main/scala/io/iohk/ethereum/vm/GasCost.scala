package io.iohk.ethereum.vm

// scalastyle:off magic.number
object GasCost {
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
    val memNeeded = addr.longValue + dataSize

    if (memNeeded > MaxMemory)
      DataWord.MaxValue
    else if (memNeeded <= memSize)
      0
    else {
      // See YP H.1 (222)
      def c(m: BigInt) = {
        val w = wordsForBytes(m)
        G_memory.value * w + w * w / 512
      }
      c(memNeeded) - c(memSize)
    }
  }

  /**
    * Number of 32-byte DataWords required to hold n bytes (~= math.ceil(n / 32))
    */
  def wordsForBytes(n: BigInt): BigInt =
    (n - 1) / DataWord.Size + 1
}

/**
  * See YP, appendix G
  */
sealed abstract class GasCost(val value: BigInt)

case object G_zero          extends GasCost(0)
case object G_base          extends GasCost(2)
case object G_verylow       extends GasCost(3)
case object G_low           extends GasCost(5)
case object G_mid           extends GasCost(8)
case object G_high          extends GasCost(10)
case object G_extcode       extends GasCost(700)
case object G_balance       extends GasCost(400)
case object G_sload         extends GasCost(200)
case object G_jumpdest      extends GasCost(1)
case object G_sset          extends GasCost(20000)
case object G_sreset        extends GasCost(5000)
case object R_sclear        extends GasCost(15000)
case object R_suicide       extends GasCost(24000)
case object G_suicide       extends GasCost(5000)
case object G_create        extends GasCost(32000)
case object G_codedeposit   extends GasCost(200)
case object G_call          extends GasCost(700)
case object G_callvalue     extends GasCost(9000)
case object G_callstipend   extends GasCost(2300)
case object G_newaccount    extends GasCost(25000)
case object G_exp           extends GasCost(10)
case object G_expbyte       extends GasCost(10)
case object G_memory        extends GasCost(3)
case object G_txcreate      extends GasCost(32000)
case object G_txdatazero    extends GasCost(4)
case object G_txdatanonzero extends GasCost(68)
case object G_transaction   extends GasCost(21000)
case object G_log           extends GasCost(375)
case object G_logdata       extends GasCost(8)
case object G_logtopic      extends GasCost(375)
case object G_sha3          extends GasCost(30)
case object G_sha3word      extends GasCost(6)
case object G_copy          extends GasCost(3)
case object G_blockhash     extends GasCost(20)
