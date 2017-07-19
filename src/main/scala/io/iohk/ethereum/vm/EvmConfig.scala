package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.utils.BlockchainConfig

// scalastyle:off number.of.methods
// scalastyle:off number.of.types
// scalastyle:off magic.number
object EvmConfig {

  val MaxCallDepth: Int = 1024

  val MaxMemory: UInt256 = UInt256(Int.MaxValue) /* used to artificially limit memory usage by incurring maximum gas cost */

  /**
    * returns the evm config that should be used for given block
    */
  def forBlock(blockNumber: BigInt, blockchainConfig: BlockchainConfig): EvmConfig = {
    val transitionBlockToConfigMapping: Map[BigInt, EvmConfigBuilder] = Map(
      blockchainConfig.frontierBlockNumber -> FrontierConfigBuilder,
      blockchainConfig.homesteadBlockNumber -> HomesteadConfigBuilder,
      blockchainConfig.eip150BlockNumber -> PostEIP150ConfigBuilder,
      blockchainConfig.eip160BlockNumber -> PostEIP160ConfigBuilder)

    // highest transition block that is less/equal to `blockNumber`
    val evmConfigBuilder = transitionBlockToConfigMapping
      .filterKeys(_ <= blockNumber)
      .maxBy(_._1)
      ._2
    evmConfigBuilder(blockchainConfig.maxCodeSize)
  }

  type EvmConfigBuilder = Option[BigInt] => EvmConfig

  def FrontierConfigBuilder(maxCodeSize: Option[BigInt]): EvmConfig = EvmConfig(
    feeSchedule = new FeeSchedule.FrontierFeeSchedule,
    opCodes = OpCodes.FrontierOpCodes,
    exceptionalFailedCodeDeposit = false,
    subGasCapDivisor = None,
    chargeSelfDestructForNewAccount = false,
    maxCodeSize = maxCodeSize)

  def HomesteadConfigBuilder(maxCodeSize: Option[BigInt]): EvmConfig = EvmConfig(
    feeSchedule = new FeeSchedule.HomesteadFeeSchedule,
    opCodes = OpCodes.HomesteadOpCodes,
    exceptionalFailedCodeDeposit = true,
    subGasCapDivisor = None,
    chargeSelfDestructForNewAccount = false,
    maxCodeSize = maxCodeSize)

  def PostEIP150ConfigBuilder(maxCodeSize: Option[BigInt]): EvmConfig = HomesteadConfigBuilder(maxCodeSize).copy(
    feeSchedule = new FeeSchedule.PostEIP150FeeSchedule,
    subGasCapDivisor = Some(64),
    chargeSelfDestructForNewAccount = true)

  def PostEIP160ConfigBuilder(maxCodeSize: Option[BigInt]): EvmConfig = PostEIP150ConfigBuilder(maxCodeSize).copy(
    feeSchedule = new FeeSchedule.PostEIP160FeeSchedule)
}

case class EvmConfig(
    feeSchedule: FeeSchedule,
    opCodes: List[OpCode],
    exceptionalFailedCodeDeposit: Boolean,
    subGasCapDivisor: Option[Long],
    chargeSelfDestructForNewAccount: Boolean,
    maxCodeSize: Option[BigInt]) {

  import feeSchedule._
  import EvmConfig._

  val byteToOpCode: Map[Byte, OpCode] =
    opCodes.map(op => op.code -> op).toMap

  /**
    * Calculate gas cost of memory usage. Incur a blocking gas cost if memory usage exceeds reasonable limits.
    *
    * @param memSize  current memory size in bytes
    * @param offset   memory offset to be written/read
    * @param dataSize size of data to be written/read in bytes
    * @return gas cost
    */
  def calcMemCost(memSize: BigInt, offset: BigInt, dataSize: BigInt): BigInt = {
    /** See YP H.1 (222) */
    def c(m: BigInt): BigInt = {
      val a = wordsForBytes(m)
      G_memory * a + a * a / 512
    }

    val memNeeded = if (dataSize == 0) BigInt(0) else offset + dataSize
    if (memNeeded > MaxMemory)
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
  def calcTransactionIntrinsicGas(txData: ByteString, isContractCreation: Boolean): BigInt = {
    val txDataZero = txData.count(_ == 0)
    val txDataNonZero = txData.length - txDataZero

    txDataZero * G_txdatazero +
      txDataNonZero * G_txdatanonzero +
      (if (isContractCreation) G_txcreate else 0) +
      G_transaction
  }

  /**
    * If the initialization code completes successfully, a final contract-creation cost is paid, the code-deposit cost,
    * proportional to the size of the created contract’s code. See YP equation (96)
    *
    * @param executionResultData Transaction code initialization result
    * @return Calculated gas cost
    */
  def calcCodeDepositCost(executionResultData: ByteString): BigInt =
    G_codedeposit * executionResultData.size

  /**
    * a helper method used for gas adjustment in CALL and CREATE opcode, see YP eq. (224)
    */
  def gasCap(g: BigInt): BigInt =
    subGasCapDivisor.map(d => g - g / d).getOrElse(g)
}

object FeeSchedule {

  class FrontierFeeSchedule extends FeeSchedule {
      override val G_zero = 0
      override val G_base = 2
      override val G_verylow = 3
      override val G_low = 5
      override val G_mid = 8
      override val G_high = 10
      override val G_balance = 20
      override val G_sload = 50
      override val G_jumpdest = 1
      override val G_sset = 20000
      override val G_sreset = 5000
      override val R_sclear = 15000
      override val R_selfdestruct = 24000
      override val G_selfdestruct = 0
      override val G_create = 32000
      override val G_codedeposit = 200
      override val G_call = 40
      override val G_callvalue = 9000
      override val G_callstipend = 2300
      override val G_newaccount = 25000
      override val G_exp = 10
      override val G_expbyte = 10
      override val G_memory = 3
      override val G_txcreate = 0
      override val G_txdatazero = 4
      override val G_txdatanonzero = 68
      override val G_transaction = 21000
      override val G_log = 375
      override val G_logdata = 8
      override val G_logtopic = 375
      override val G_sha3 = 30
      override val G_sha3word = 6
      override val G_copy = 3
      override val G_blockhash = 20
      override val G_extcode = 20
  }

  class HomesteadFeeSchedule extends FrontierFeeSchedule {
    override val G_txcreate = 32000
  }

  class PostEIP150FeeSchedule extends HomesteadFeeSchedule {
    override val G_sload = 200
    override val G_call = 700
    override val G_balance = 400
    override val G_selfdestruct = 5000
    override val G_extcode = 700
  }

  class PostEIP160FeeSchedule extends PostEIP150FeeSchedule {
    override val G_expbyte = 50
  }
}

trait FeeSchedule {
  val G_zero: BigInt
  val G_base: BigInt
  val G_verylow: BigInt
  val G_low: BigInt
  val G_mid: BigInt
  val G_high: BigInt
  val G_balance: BigInt
  val G_sload: BigInt
  val G_jumpdest: BigInt
  val G_sset: BigInt
  val G_sreset: BigInt
  val R_sclear: BigInt
  val R_selfdestruct: BigInt
  val G_selfdestruct: BigInt
  val G_create: BigInt
  val G_codedeposit: BigInt
  val G_call: BigInt
  val G_callvalue: BigInt
  val G_callstipend: BigInt
  val G_newaccount: BigInt
  val G_exp: BigInt
  val G_expbyte: BigInt
  val G_memory: BigInt
  val G_txcreate: BigInt
  val G_txdatazero: BigInt
  val G_txdatanonzero: BigInt
  val G_transaction: BigInt
  val G_log: BigInt
  val G_logdata: BigInt
  val G_logtopic: BigInt
  val G_sha3: BigInt
  val G_sha3word: BigInt
  val G_copy: BigInt
  val G_blockhash: BigInt
  val G_extcode: BigInt
}
