package io.iohk.ethereum.vm

import io.iohk.ethereum.utils.Config

// scalastyle:off number.of.methods
// scalastyle:off number.of.types
// scalastyle:off magic.number
object EvmConfig {

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
    feeSchedule = FeeSchedule.FrontierFeeSchedule,
    opCodes = OpCodes.FrontierOpCodes,
    exceptionalFailedCodeDeposit = false)

  /*
    TODO:
    If contract creation does not have enough gas to pay for the final gas fee
    for adding the contract code to the state, the contract creation fails (ie. goes out-of-gas)
    rather than leaving an empty contract.

    See: exceptional_failed_code_deposit in Parity
   */
  val HomesteadConfig = EvmConfig(
    feeSchedule = FeeSchedule.HomesteadFeeSchedule,
    opCodes = OpCodes.HomesteadOpCodes,
    exceptionalFailedCodeDeposit = true)

  /*
  TODO:
    sub_gas_cap_divisor
  	  If Some(x): let limit = GAS * (x - 1) / x; let CALL's gas = min(requested, limit). let CREATE's gas = limit.
      If None: let CALL's gas = (requested > GAS ? [OOG] : GAS). let CREATE's gas = GAS
   */
  // TODO: no_empty, kill_empty from schedule.rs, parity
  val PostEIP150Config = EvmConfig(
    feeSchedule = FeeSchedule.PostEIP150FeeSchedule,
    opCodes = OpCodes.PostEIP150OpCodes,
    exceptionalFailedCodeDeposit = true)

  private val transitionBlockToConfigMapping: Map[BigInt, EvmConfig] = Map(
    Config.Blockchain.frontierBlockNumber -> FrontierConfig,
    Config.Blockchain.homesteadBlockNumber -> HomesteadConfig,
    Config.Blockchain.eip150BlockNumber -> PostEIP150Config)

}

case class EvmConfig(
    feeSchedule: FeeSchedule,
    opCodes: List[OpCode],
    exceptionalFailedCodeDeposit: Boolean,
    maxMemory: UInt256 = UInt256(Long.MaxValue), /* used to artificially limit memory usage by incurring maximum gas cost */
    maxCallDepth: Int = 1024,
    createDataLimit: BigInt = BigInt(Long.MaxValue), /* TODO: check me when creating contracts - seems it's only used in test networks (?) */ ) {

  val byteToOpCode: Map[Byte, OpCode] =
    opCodes.map(op => op.code -> op).toMap
}

object FeeSchedule {

  // See YP, appendix G
  sealed trait Key
  object Key {
    case object G_zero extends Key
    case object G_base extends Key
    case object G_verylow extends Key
    case object G_low extends Key
    case object G_mid extends Key
    case object G_high extends Key
    case object G_extcode extends Key
    case object G_balance extends Key
    case object G_sload extends Key
    case object G_jumpdest extends Key
    case object G_sset extends Key
    case object G_sreset extends Key
    case object R_sclear extends Key
    case object R_selfdestruct extends Key
    case object G_selfdestruct extends Key
    case object G_create extends Key
    case object G_codedeposit extends Key
    case object G_call extends Key
    case object G_callvalue extends Key
    case object G_callstipend extends Key
    case object G_newaccount extends Key
    case object G_exp extends Key
    case object G_expbyte extends Key
    case object G_memory extends Key
    case object G_txcreate extends Key
    case object G_txdatazero extends Key
    case object G_txdatanonzero extends Key
    case object G_transaction extends Key
    case object G_log extends Key
    case object G_logdata extends Key
    case object G_logtopic extends Key
    case object G_sha3 extends Key
    case object G_sha3word extends Key
    case object G_copy extends Key
    case object G_blockhash extends Key
    case object G_extcodesize extends Key
    case object G_extcodecopy_base_gas extends Key
    case object G_suicide_to_new_account_gas extends Key
  }

  val FrontierFeeSchedule: FeeSchedule = FeeSchedule(Map(
      Key.G_zero -> UInt256(0),
      Key.G_base -> UInt256(2),
      Key.G_verylow -> UInt256(3),
      Key.G_low -> UInt256(5),
      Key.G_mid -> UInt256(8),
      Key.G_high -> UInt256(10),
      Key.G_extcode -> UInt256(700),
      Key.G_balance -> UInt256(20),
      Key.G_sload -> UInt256(50),
      Key.G_jumpdest -> UInt256(1),
      Key.G_sset -> UInt256(20000),
      Key.G_sreset -> UInt256(5000),
      Key.R_sclear -> UInt256(15000),
      Key.R_selfdestruct -> UInt256(24000),
      Key.G_selfdestruct -> UInt256(0),
      Key.G_create -> UInt256(32000),
      Key.G_codedeposit -> UInt256(200),
      Key.G_call -> UInt256(40),
      Key.G_callvalue -> UInt256(9000),
      Key.G_callstipend -> UInt256(2300),
      Key.G_newaccount -> UInt256(25000),
      Key.G_exp -> UInt256(10),
      Key.G_expbyte -> UInt256(10),
      Key.G_memory -> UInt256(3),
      Key.G_txcreate -> UInt256(21000),
      Key.G_txdatazero -> UInt256(4),
      Key.G_txdatanonzero -> UInt256(68),
      Key.G_transaction -> UInt256(21000),
      Key.G_log -> UInt256(375),
      Key.G_logdata -> UInt256(8),
      Key.G_logtopic -> UInt256(375),
      Key.G_sha3 -> UInt256(30),
      Key.G_sha3word -> UInt256(6),
      Key.G_copy -> UInt256(3),
      Key.G_blockhash -> UInt256(20),
      Key.G_extcodesize -> UInt256(20),
      Key.G_extcodecopy_base_gas -> UInt256(20),
      Key.G_suicide_to_new_account_gas -> UInt256(0)))

  val HomesteadFeeSchedule: FeeSchedule =
    FrontierFeeSchedule.copy(values = FrontierFeeSchedule.values ++ Map(
      Key.G_txcreate -> UInt256(53000)))

  val PostEIP150FeeSchedule: FeeSchedule =
    HomesteadFeeSchedule.copy(values = HomesteadFeeSchedule.values ++ Map(
      Key.G_sload -> UInt256(200),
      Key.G_call -> UInt256(700),
      Key.G_balance -> UInt256(400),
      Key.G_selfdestruct -> UInt256(5000),
      Key.G_extcodesize -> UInt256(700),
      Key.G_extcodecopy_base_gas -> UInt256(700),
      Key.G_suicide_to_new_account_gas -> UInt256(25000)))
}

case class FeeSchedule(values: Map[FeeSchedule.Key, UInt256]) {
  def apply(key: FeeSchedule.Key): UInt256 = values(key)
}
