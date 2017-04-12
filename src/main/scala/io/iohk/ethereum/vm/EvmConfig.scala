package io.iohk.ethereum.vm

import io.iohk.ethereum.utils.Config

// scalastyle:off number.of.methods
// scalastyle:off number.of.types
// scalastyle:off magic.number
object EvmConfig {

  val MaxCallDepth: Int = 1024

  val MaxMemory: UInt256 = UInt256(Int.MaxValue) /* used to artificially limit memory usage by incurring maximum gas cost */

  /**
    * returns the evm config that should be used for given block
    */
  def forBlock(blockNumber: BigInt): EvmConfig = {
    // highest transition block that is less/equal to `blockNumber`
    transitionBlockToConfigMapping
      .filterKeys(_ <= blockNumber)
      .maxBy(_._1)
      ._2
  }

  val FrontierConfig = EvmConfig(
    feeSchedule = new FeeSchedule.FrontierFeeSchedule,
    opCodes = OpCodes.FrontierOpCodes,
    exceptionalFailedCodeDeposit = false)

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
    exceptionalFailedCodeDeposit = true)

  /*
  TODO(CREATE): sub_gas_cap_divisor
    If Some(x): let limit = GAS * (x - 1) / x; let CALL's gas = min(requested, limit). let CREATE's gas = limit.
    If None: let CALL's gas = (requested > GAS ? [OOG] : GAS). let CREATE's gas = GAS
   */
  val PostEIP150Config = HomesteadConfig.copy(
    feeSchedule = new FeeSchedule.PostEIP150FeeSchedule,
    subGasCapDivisor = Some(64))

  val PostEIP160Config = PostEIP150Config.copy(
    feeSchedule = new FeeSchedule.PostEIP160FeeSchedule)

  private val transitionBlockToConfigMapping: Map[BigInt, EvmConfig] = Map(
    Config.Blockchain.frontierBlockNumber -> FrontierConfig,
    Config.Blockchain.homesteadBlockNumber -> HomesteadConfig,
    Config.Blockchain.eip150BlockNumber -> PostEIP150Config,
    Config.Blockchain.eip160BlockNumber -> PostEIP160Config)

}

case class EvmConfig(
    feeSchedule: FeeSchedule,
    opCodes: List[OpCode],
    exceptionalFailedCodeDeposit: Boolean,
    subGasCapDivisor: Option[Long] = None) {

  val byteToOpCode: Map[Byte, OpCode] =
    opCodes.map(op => op.code -> op).toMap
}

object FeeSchedule {

  class FrontierFeeSchedule extends FeeSchedule {
      override val G_zero = UInt256(0)
      override val G_base = UInt256(2)
      override val G_verylow = UInt256(3)
      override val G_low = UInt256(5)
      override val G_mid = UInt256(8)
      override val G_high = UInt256(10)
      override val G_balance = UInt256(20)
      override val G_sload = UInt256(50)
      override val G_jumpdest = UInt256(1)
      override val G_sset = UInt256(20000)
      override val G_sreset = UInt256(5000)
      override val R_sclear = UInt256(15000)
      override val R_selfdestruct = UInt256(24000)
      override val G_selfdestruct = UInt256(0)
      override val G_create = UInt256(32000)
      override val G_codedeposit = UInt256(200)
      override val G_call = UInt256(40)
      override val G_callvalue = UInt256(9000)
      override val G_callstipend = UInt256(2300)
      override val G_newaccount = UInt256(25000)
      override val G_exp = UInt256(10)
      override val G_expbyte = UInt256(10)
      override val G_memory = UInt256(3)
      override val G_txcreate = UInt256(21000)
      override val G_txdatazero = UInt256(4)
      override val G_txdatanonzero = UInt256(68)
      override val G_transaction = UInt256(21000)
      override val G_log = UInt256(375)
      override val G_logdata = UInt256(8)
      override val G_logtopic = UInt256(375)
      override val G_sha3 = UInt256(30)
      override val G_sha3word = UInt256(6)
      override val G_copy = UInt256(3)
      override val G_blockhash = UInt256(20)
      override val G_extcodesize = UInt256(20)
      override val G_extcode = UInt256(20)
      override val G_selfdestruct_to_new_account = UInt256(0)
  }

  class HomesteadFeeSchedule extends FrontierFeeSchedule {
    override val G_txcreate = UInt256(53000)
  }

  class PostEIP150FeeSchedule extends HomesteadFeeSchedule {
    override val G_sload = UInt256(200)
    override val G_call = UInt256(700)
    override val G_balance = UInt256(400)
    override val G_selfdestruct = UInt256(5000)
    override val G_extcodesize = UInt256(700)
    override val G_extcode = UInt256(700)
    override val G_selfdestruct_to_new_account = UInt256(25000)
  }

  class PostEIP160FeeSchedule extends PostEIP150FeeSchedule {
    override val G_expbyte = UInt256(50)
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
  val G_extcodesize: UInt256
  val G_extcode: UInt256
  val G_selfdestruct_to_new_account: UInt256
}
