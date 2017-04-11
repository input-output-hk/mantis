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
    TODO (CREATE):
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
  TODO(CREATE): sub_gas_cap_divisor
    If Some(x): let limit = GAS * (x - 1) / x; let CALL's gas = min(requested, limit). let CREATE's gas = limit.
    If None: let CALL's gas = (requested > GAS ? [OOG] : GAS). let CREATE's gas = GAS
   */
  val PostEIP150Config = HomesteadConfig.copy(
    feeSchedule = FeeSchedule.PostEIP150FeeSchedule,
    subGasCapDivisor = Some(64))

  val PostEIP160Config = PostEIP150Config.copy(
    feeSchedule = FeeSchedule.PostEIP160FeeSchedule)

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
    maxMemory: UInt256 = UInt256(Long.MaxValue), /* used to artificially limit memory usage by incurring maximum gas cost */
    maxCallDepth: Int = 1024,
    subGasCapDivisor: Option[Long] = None) {

  val byteToOpCode: Map[Byte, OpCode] =
    opCodes.map(op => op.code -> op).toMap
}

object FeeSchedule {

  // See YP, appendix G
  sealed trait GasCost
  object GasCost {
    case object G_zero extends GasCost
    case object G_base extends GasCost
    case object G_verylow extends GasCost
    case object G_low extends GasCost
    case object G_mid extends GasCost
    case object G_high extends GasCost
    case object G_extcode extends GasCost
    case object G_balance extends GasCost
    case object G_sload extends GasCost
    case object G_jumpdest extends GasCost
    case object G_sset extends GasCost
    case object G_sreset extends GasCost
    case object R_sclear extends GasCost
    case object R_selfdestruct extends GasCost
    case object G_selfdestruct extends GasCost
    case object G_create extends GasCost
    case object G_codedeposit extends GasCost
    case object G_call extends GasCost
    case object G_callvalue extends GasCost
    case object G_callstipend extends GasCost
    case object G_newaccount extends GasCost
    case object G_exp extends GasCost
    case object G_expbyte extends GasCost
    case object G_memory extends GasCost
    case object G_txcreate extends GasCost
    case object G_txdatazero extends GasCost
    case object G_txdatanonzero extends GasCost
    case object G_transaction extends GasCost
    case object G_log extends GasCost
    case object G_logdata extends GasCost
    case object G_logtopic extends GasCost
    case object G_sha3 extends GasCost
    case object G_sha3word extends GasCost
    case object G_copy extends GasCost
    case object G_blockhash extends GasCost
    case object G_extcodesize extends GasCost
    case object G_extcodecopy_base extends GasCost
    case object G_selfdestruct_to_new_account extends GasCost
  }

  val FrontierFeeSchedule: FeeSchedule = FeeSchedule(Map(
      GasCost.G_zero -> UInt256(0),
      GasCost.G_base -> UInt256(2),
      GasCost.G_verylow -> UInt256(3),
      GasCost.G_low -> UInt256(5),
      GasCost.G_mid -> UInt256(8),
      GasCost.G_high -> UInt256(10),
      GasCost.G_extcode -> UInt256(700),
      GasCost.G_balance -> UInt256(20),
      GasCost.G_sload -> UInt256(50),
      GasCost.G_jumpdest -> UInt256(1),
      GasCost.G_sset -> UInt256(20000),
      GasCost.G_sreset -> UInt256(5000),
      GasCost.R_sclear -> UInt256(15000),
      GasCost.R_selfdestruct -> UInt256(24000),
      GasCost.G_selfdestruct -> UInt256(0),
      GasCost.G_create -> UInt256(32000),
      GasCost.G_codedeposit -> UInt256(200),
      GasCost.G_call -> UInt256(40),
      GasCost.G_callvalue -> UInt256(9000),
      GasCost.G_callstipend -> UInt256(2300),
      GasCost.G_newaccount -> UInt256(25000),
      GasCost.G_exp -> UInt256(10),
      GasCost.G_expbyte -> UInt256(10),
      GasCost.G_memory -> UInt256(3),
      GasCost.G_txcreate -> UInt256(21000),
      GasCost.G_txdatazero -> UInt256(4),
      GasCost.G_txdatanonzero -> UInt256(68),
      GasCost.G_transaction -> UInt256(21000),
      GasCost.G_log -> UInt256(375),
      GasCost.G_logdata -> UInt256(8),
      GasCost.G_logtopic -> UInt256(375),
      GasCost.G_sha3 -> UInt256(30),
      GasCost.G_sha3word -> UInt256(6),
      GasCost.G_copy -> UInt256(3),
      GasCost.G_blockhash -> UInt256(20),
      GasCost.G_extcodesize -> UInt256(20),
      GasCost.G_extcodecopy_base -> UInt256(20),
      GasCost.G_selfdestruct_to_new_account -> UInt256(0)))

  val HomesteadFeeSchedule: FeeSchedule =
    FrontierFeeSchedule.copy(values = FrontierFeeSchedule.values ++ Map(
      GasCost.G_txcreate -> UInt256(53000)))

  val PostEIP150FeeSchedule: FeeSchedule =
    HomesteadFeeSchedule.copy(values = HomesteadFeeSchedule.values ++ Map(
      GasCost.G_sload -> UInt256(200),
      GasCost.G_call -> UInt256(700),
      GasCost.G_balance -> UInt256(400),
      GasCost.G_selfdestruct -> UInt256(5000),
      GasCost.G_extcodesize -> UInt256(700),
      GasCost.G_extcodecopy_base -> UInt256(700),
      GasCost.G_selfdestruct_to_new_account -> UInt256(25000)))

  val PostEIP160FeeSchedule: FeeSchedule =
    PostEIP150FeeSchedule.copy(values = PostEIP150FeeSchedule.values ++ Map(
      GasCost.G_expbyte -> UInt256(50)))
}

case class FeeSchedule(values: Map[FeeSchedule.GasCost, UInt256]) {
  def apply(key: FeeSchedule.GasCost): UInt256 = values(key)
}
