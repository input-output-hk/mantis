package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.FeeSchedule.Key.G_memory

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
    * Number of 32-byte DataWords required to hold n bytes (~= math.ceil(n / 32))
    */
  def wordsForBytes(n: UInt256): UInt256 =
   if (n.isZero) 0 else (n - 1) / UInt256.Size + 1
}
