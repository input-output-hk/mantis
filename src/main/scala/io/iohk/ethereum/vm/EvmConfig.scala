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
    val transitionBlockToConfigMapping: Map[BigInt, EvmConfig] = Map(
      blockchainConfig.frontierBlockNumber -> FrontierConfig,
      blockchainConfig.homesteadBlockNumber -> HomesteadConfig,
      blockchainConfig.eip150BlockNumber -> PostEIP150Config,
      blockchainConfig.eip160BlockNumber -> PostEIP160Config)

    // highest transition block that is less/equal to `blockNumber`
    transitionBlockToConfigMapping
      .filterKeys(_ <= blockNumber)
      .maxBy(_._1)
      ._2
  }

  val FrontierConfig = EvmConfig(
    feeSchedule = new FeeSchedule.FrontierFeeSchedule,
    opCodes = OpCodes.FrontierOpCodes,
    exceptionalFailedCodeDeposit = false,
    subGasCapDivisor = None,
    chargeSelfDestructForNewAccount = false)

  /*
    TODO (CREATE):
    If contract creation does not have enough gas to pay for the final gas fee
    for adding the contract code to the state, the contract creation fails (ie. goes out-of-gas)
    rather than leaving an empty contract.

    See: exceptional_failed_code_deposit in Parity
   */
  val HomesteadConfig = EvmConfig(
    feeSchedule = new FeeSchedule.HomesteadFeeSchedule,
    opCodes = OpCodes.HomesteadOpCodes,
    exceptionalFailedCodeDeposit = true,
    subGasCapDivisor = None,
    chargeSelfDestructForNewAccount = false)

  /*
  TODO(CREATE): sub_gas_cap_divisor
    If Some(x): let limit = GAS * (x - 1) / x; let CALL's gas = min(requested, limit). let CREATE's gas = limit.
    If None: let CALL's gas = (requested > GAS ? [OOG] : GAS). let CREATE's gas = GAS
   */
  val PostEIP150Config = HomesteadConfig.copy(
    feeSchedule = new FeeSchedule.PostEIP150FeeSchedule,
    subGasCapDivisor = Some(64),
    chargeSelfDestructForNewAccount = true)

  val PostEIP160Config = PostEIP150Config.copy(
    feeSchedule = new FeeSchedule.PostEIP160FeeSchedule)
}

case class EvmConfig(
    feeSchedule: FeeSchedule,
    opCodes: List[OpCode],
    exceptionalFailedCodeDeposit: Boolean,
    subGasCapDivisor: Option[Long],
    chargeSelfDestructForNewAccount: Boolean) {

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
  def calcMemCost(memSize: UInt256, offset: UInt256, dataSize: UInt256): UInt256 = {
    /** See YP H.1 (222) */
    def c(m: UInt256): UInt256 = {
      val a = wordsForBytes(m)
      G_memory * a + a * a / 512
    }

    val memNeeded = if (dataSize.isZero) UInt256.Zero else offset + dataSize
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
  def calcTransactionIntrinsicGas(txData: ByteString, isContractCreation: Boolean, blockNumber: BigInt): UInt256 = {
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
  def calcCodeDepositCost(executionResultData: ByteString): UInt256 =
    G_codedeposit * executionResultData.size

  /**
    * a helper method used for gas adjustment in CALL and CREATE opcode, see YP eq. (224)
    */
  def gasCap(g: UInt256): UInt256 =
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
  val G_zero: UInt256
  val G_base: UInt256
  val G_verylow: UInt256
  val G_low: UInt256
  val G_mid: UInt256
  val G_high: UInt256
  val G_balance: UInt256
  val G_sload: UInt256
  val G_jumpdest: UInt256
  val G_sset: UInt256
  val G_sreset: UInt256
  val R_sclear: UInt256
  val R_selfdestruct: UInt256
  val G_selfdestruct: UInt256
  val G_create: UInt256
  val G_codedeposit: UInt256
  val G_call: UInt256
  val G_callvalue: UInt256
  val G_callstipend: UInt256
  val G_newaccount: UInt256
  val G_exp: UInt256
  val G_expbyte: UInt256
  val G_memory: UInt256
  val G_txcreate: UInt256
  val G_txdatazero: UInt256
  val G_txdatanonzero: UInt256
  val G_transaction: UInt256
  val G_log: UInt256
  val G_logdata: UInt256
  val G_logtopic: UInt256
  val G_sha3: UInt256
  val G_sha3word: UInt256
  val G_copy: UInt256
  val G_blockhash: UInt256
  val G_extcode: UInt256
}
