package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{Address, TxLogEntry}
import io.iohk.ethereum.vm.GasFee._

// scalastyle:off magic.number
// scalastyle:off number.of.types
// scalastyle:off method.length
object OpCode {

  val opcodes: List[OpCode] = List(
    STOP,
    ADD,
    MUL,
    SUB,
    DIV,
    SDIV,
    MOD,
    SMOD,
    ADDMOD,
    MULMOD,
    EXP,
    SIGNEXTEND,

    LT,
    GT,
    SLT,
    SGT,
    EQ,
    ISZERO,
    AND,
    OR,
    XOR,
    NOT,
    BYTE,

    SHA3,

    ADDRESS,
    //BALANCE,
    ORIGIN,
    CALLER,
    CALLVALUE,
    CALLDATALOAD,
    CALLDATASIZE,
    CALLDATACOPY,
    //CODESIZE,
    CODECOPY,
    //GASPRICE,
    EXTCODESIZE,
    EXTCODECOPY,

    BLOCKHASH,
    COINBASE,
    TIMESTAMP,
    NUMBER,
    DIFFICULTY,
    GASLIMIT,

    POP,
    MLOAD,
    MSTORE,
    //MSTORE8,
    SLOAD,
    SSTORE,
    JUMP,
    JUMPI,
    //PC,
    //MSIZE,
    GAS,
    JUMPDEST,

    PUSH1,
    PUSH2,
    PUSH3,
    PUSH4,
    PUSH5,
    PUSH6,
    PUSH7,
    PUSH8,
    PUSH9,
    PUSH10,
    PUSH11,
    PUSH12,
    PUSH13,
    PUSH14,
    PUSH15,
    PUSH16,
    PUSH17,
    PUSH18,
    PUSH19,
    PUSH20,
    PUSH21,
    PUSH22,
    PUSH23,
    PUSH24,
    PUSH25,
    PUSH26,
    PUSH27,
    PUSH28,
    PUSH29,
    PUSH30,
    PUSH31,
    PUSH32,

    DUP1,
    DUP2,
    DUP3,
    DUP4,
    DUP5,
    DUP6,
    DUP7,
    DUP8,
    DUP9,
    DUP10,
    DUP11,
    DUP12,
    DUP13,
    DUP14,
    DUP15,
    DUP16,

    SWAP1,
    SWAP2,
    SWAP3,
    SWAP4,
    SWAP5,
    SWAP6,
    SWAP7,
    SWAP8,
    SWAP9,
    SWAP10,
    SWAP11,
    SWAP12,
    SWAP13,
    SWAP14,
    SWAP15,
    SWAP16,

    LOG0,
    LOG1,
    LOG2,
    LOG3,
    LOG4,

    //CREATE,
    CALL,
    CALLCODE,
    RETURN,
    DELEGATECALL,
    INVALID
    //SUICIDE
  )

  val byteToOpCode: Map[Byte, OpCode] =
    opcodes.map(op => op.code -> op).toMap

  val MaxCallDepth: Int = 1024

  def sliceBytes(bytes: ByteString, offset: Int, size: Int): ByteString =
    bytes.slice(offset, offset + size).padTo(size, 0.toByte)
}

/**
  * Base class for all the opcodes of the EVM
  *
  * @param code Opcode byte representation
  * @param delta number of words to be popped from stack
  * @param alpha number of words to be pushed to stack
  */
sealed abstract class OpCode(val code: Byte, val delta: Int, val alpha: Int, val constGas: UInt256) {
  def this(code: Int, pop: Int, push: Int, constGas: UInt256) = this(code.toByte, pop, push, constGas)

  def execute[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    if (state.stack.size < delta)
      state.withError(StackUnderflow)
    else if (state.stack.size - delta + alpha > state.stack.maxSize)
      state.withError(StackOverflow)
    else {
      val gas = constGas + varGas(state)
      if (gas > state.gas)
        state.copy(gas = 0).withError(OutOfGas)
      else
        exec(state).spendGas(gas)
    }
  }

  protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): UInt256

  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S]
}

sealed trait ConstGas { self: OpCode =>
  protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): UInt256 = 0
}

case object STOP extends OpCode(0x00, 0, 0, G_zero) with ConstGas {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] =
    state.halt
}

sealed abstract class UnaryOp(code: Int, constGas: UInt256)(val f: UInt256 => UInt256) extends OpCode(code, 1, 1, constGas) with ConstGas {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (a, stack1) = state.stack.pop
    val res = f(a)
    val stack2 = stack1.push(res)
    state.withStack(stack2).step()
  }
}

sealed abstract class BinaryOp(code: Int, constGas: UInt256)(val f: (UInt256, UInt256) => UInt256)
  extends OpCode(code.toByte, 2, 1, constGas) {

  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(a, b), stack1) = state.stack.pop(2)
    val res = f(a, b)
    val stack2 = stack1.push(res)
    state.withStack(stack2).step()
  }
}

sealed abstract class TernaryOp(code: Int, constGas: UInt256)(val f: (UInt256, UInt256, UInt256) => UInt256)
    extends OpCode(code.toByte, 3, 1, constGas) {

  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(a, b, c), stack1) = state.stack.pop(3)
    val res = f(a, b, c)
    val stack2 = stack1.push(res)
    state.withStack(stack2).step()
  }
}

sealed abstract class ConstOp(code: Int)(val f: ProgramState[_ <: WorldStateProxy[_, _ <: Storage[_]], _ <: Storage[_]] => UInt256)
  extends OpCode(code, 0, 1, G_base) with ConstGas {

  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val stack1 = state.stack.push(f(state))
    state.withStack(stack1).step()
  }
}

case object ADD extends BinaryOp(0x01, G_verylow)(_ + _) with ConstGas

case object MUL extends BinaryOp(0x02, G_low)(_ * _) with ConstGas

case object SUB extends BinaryOp(0x03, G_verylow)(_ - _) with ConstGas

case object DIV extends BinaryOp(0x04, G_low)(_ div _) with ConstGas

case object SDIV extends BinaryOp(0x05, G_low)(_ sdiv _) with ConstGas

case object MOD extends BinaryOp(0x06, G_low)(_ mod _) with ConstGas

case object SMOD extends BinaryOp(0x06, G_low)(_ smod _) with ConstGas

case object ADDMOD extends TernaryOp(0x07, G_mid)(_.addmod(_, _)) with ConstGas

case object MULMOD extends TernaryOp(0x08, G_mid)(_.mulmod(_, _)) with ConstGas

case object EXP extends BinaryOp(0x0a, G_exp)(_ ** _) {
  protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): UInt256 = {
    val (Seq(_, m: UInt256), _) = state.stack.pop(2)
    G_expbyte * m.byteSize
  }
}

case object SIGNEXTEND extends BinaryOp(0x0b, G_low)((a, b) => b signExtend a) with ConstGas

case object LT extends BinaryOp(0x10, G_verylow)(_ < _) with ConstGas

case object GT extends BinaryOp(0x11, G_verylow)(_ > _) with ConstGas

case object SLT extends BinaryOp(0x12, G_verylow)(_ slt _) with ConstGas

case object SGT extends BinaryOp(0x13, G_verylow)(_ sgt _) with ConstGas

case object EQ extends BinaryOp(0x14, G_verylow)(_ == _) with ConstGas

case object ISZERO extends UnaryOp(0x15, G_verylow)(_.isZero) with ConstGas

case object AND extends BinaryOp(0x16, G_verylow)(_ & _) with ConstGas

case object OR extends BinaryOp(0x17, G_verylow)(_ | _) with ConstGas

case object XOR extends BinaryOp(0x18, G_verylow)(_ ^ _) with ConstGas

case object NOT extends UnaryOp(0x19, G_verylow)(~_) with ConstGas

case object BYTE extends BinaryOp(0x1a, G_verylow)((a, b) => b getByte a) with ConstGas

case object SHA3 extends OpCode(0x20, 2, 1, G_sha3) {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(offset, size), stack1) = state.stack.pop(2)
    val (input, mem1) = state.memory.load(offset, size)
    val hash = kec256(input.toArray)
    val ret = UInt256(hash)
    val stack2 = stack1.push(ret)
    state.withStack(stack2).withMemory(mem1).step()
  }

  protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): UInt256 = {
    val (Seq(offset, size), _) = state.stack.pop(2)
    val memCost = calcMemCost(state.memory.size, offset, size)
    val shaCost = G_sha3word * wordsForBytes(size)
    memCost + shaCost
  }
}

case object ADDRESS extends ConstOp(0x30)(_.env.ownerAddr.toUInt256)

case object ORIGIN extends ConstOp(0x32)(_.env.originAddr.toUInt256)

case object CALLER extends ConstOp(0x33)(_.env.callerAddr.toUInt256)

case object CALLVALUE extends ConstOp(0x34)(_.env.value)

case object CALLDATALOAD extends OpCode(0x35, 1, 1, G_verylow) with ConstGas {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (offset, stack1) = state.stack.pop
    val data = OpCode.sliceBytes(state.inputData, offset.toInt, 32)
    val stack2 = stack1.push(UInt256(data))
    state.withStack(stack2).step()
  }
}

case object CALLDATASIZE extends ConstOp(0x36)(_.inputData.size)

case object CALLDATACOPY extends OpCode(0x37, 3, 0, G_verylow) {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(memOffset, dataOffset, size), stack1) = state.stack.pop(3)
    val data = OpCode.sliceBytes(state.inputData, dataOffset.toInt, size.toInt)
    val mem1 = state.memory.store(memOffset, data)
    state.withStack(stack1).withMemory(mem1).step()
  }

  protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): UInt256 = {
    val (Seq(offset, _, size), _) = state.stack.pop(3)
    val memCost = calcMemCost(state.memory.size, offset, size)
    val copyCost = G_copy * wordsForBytes(size)
    memCost + copyCost
  }
}

case object CODECOPY extends OpCode(0x39, 3, 0, G_verylow) {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(memOffset, codeOffset, size), stack1) = state.stack.pop(3)
    val bytes = state.program.getBytes(codeOffset.toInt, size.toInt)
    val mem1 = state.memory.store(memOffset, bytes)
    state.withStack(stack1).withMemory(mem1).step()
  }

  protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): UInt256 = {
    val (Seq(offset, _, size), _) = state.stack.pop(3)
    val memCost = calcMemCost(state.memory.size, offset, size)
    val copyCost = G_copy * wordsForBytes(size)
    memCost + copyCost
  }
}

case object EXTCODESIZE extends OpCode(0x3b, 1, 1, G_extcode) with ConstGas {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (addr, stack1) = state.stack.pop
    val codeSize = state.world.getCode(Address(addr)).size
    val stack2 = stack1.push(UInt256(codeSize))
    state.withStack(stack2).step()
  }
}

case object EXTCODECOPY extends OpCode(0x3c, 4, 0, G_extcode) {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(address, memOffset, codeOffset, size), stack1) = state.stack.pop(4)
    val codeCopy = OpCode.sliceBytes(state.world.getCode(Address(address)), codeOffset.toInt, size.toInt)
    val mem1 = state.memory.store(memOffset, codeCopy)
    state.withStack(stack1).withMemory(mem1).step()
  }

  protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): UInt256 = {
    val (Seq(_, memOffset, _, size), _) = state.stack.pop(4)
    val memCost = calcMemCost(state.memory.size, memOffset, size)
    val copyCost = G_copy * wordsForBytes(size)
    memCost + copyCost
  }
}

case object BLOCKHASH extends OpCode(0x40, 1, 1, G_blockhash) with ConstGas {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (blockNumber, stack1) = state.stack.pop

    val outOfLimits = state.env.blockHeader.number - blockNumber > 256 || blockNumber >= state.env.blockHeader.number
    val hash = if (outOfLimits) UInt256.Zero else state.world.getBlockHash(blockNumber).getOrElse(UInt256.Zero)

    val stack2 = stack1.push(hash)
    state.withStack(stack2).step()
  }
}

case object COINBASE extends ConstOp(0x41)(s => UInt256(s.env.blockHeader.beneficiary))

case object TIMESTAMP extends ConstOp(0x42)(s => UInt256(s.env.blockHeader.unixTimestamp))

case object NUMBER extends ConstOp(0x43)(s => UInt256(s.env.blockHeader.number))

case object DIFFICULTY extends ConstOp(0x44)(s => UInt256(s.env.blockHeader.difficulty))

case object GASLIMIT extends ConstOp(0x45)(s => UInt256(s.env.blockHeader.gasLimit))

case object POP extends OpCode(0x50, 1, 0, G_base) with ConstGas {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (_, stack1) = state.stack.pop
    state.withStack(stack1).step()
  }
}

case object MLOAD extends OpCode(0x51, 1, 1, G_verylow) {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (offset, stack1) = state.stack.pop
    val (word, mem1) = state.memory.load(offset)
    val stack2 = stack1.push(word)
    state.withStack(stack2).withMemory(mem1).step()
  }

  protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): UInt256 = {
    val (offset, _) = state.stack.pop
    calcMemCost(state.memory.size, offset, UInt256.Size)
  }
}

case object MSTORE extends OpCode(0x52, 2, 0, G_verylow) {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(offset, value), stack1) = state.stack.pop(2)
    val updatedMem = state.memory.store(offset, value)
    state.withStack(stack1).withMemory(updatedMem).step()
  }

  protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): UInt256 = {
    val (offset, _) = state.stack.pop
    calcMemCost(state.memory.size, offset, UInt256.Size)
  }
}

case object SLOAD extends OpCode(0x54, 1, 1, G_sload) with ConstGas {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (offset, stack1) = state.stack.pop
    val value = state.storage.load(offset)
    val stack2 = stack1.push(value)
    state.withStack(stack2).step()
  }
}

case object SSTORE extends OpCode(0x55, 2, 0, G_zero) {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(offset, value), stack1) = state.stack.pop(2)
    val oldValue = state.storage.load(offset)
    val refund = if (value.isZero && !oldValue.isZero) R_sclear else UInt256.Zero
    val updatedStorage = state.storage.store(offset, value)
    state.withStack(stack1).withStorage(updatedStorage).refundGas(refund).step()
  }

  protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): UInt256 = {
    val (Seq(offset, value), _) = state.stack.pop(2)
    val oldValue = state.storage.load(offset)
    if (oldValue.isZero && !value.isZero) G_sset else G_sreset
  }
}

case object JUMP extends OpCode(0x56, 1, 0, G_mid) with ConstGas {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (pos, stack1) = state.stack.pop
    //TODO: JUMPDEST validation
    state.withStack(stack1).goto(pos.toInt)
  }
}

case object JUMPI extends OpCode(0x57, 2, 0, G_high) with ConstGas {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(pos, cond), stack1) = state.stack.pop(2)
    val nextPos = if (!cond.isZero) pos.toInt else state.pc + 1
    //TODO: JUMPDEST validation
    state.withStack(stack1).goto(nextPos)
  }
}

case object GAS extends ConstOp(0x5a)(_.gas - G_base)

case object JUMPDEST extends OpCode(0x5b, 0, 0, G_jumpdest) with ConstGas {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    state.step()
  }
}

sealed abstract class PushOp(code: Int) extends OpCode(code, 0, 1, G_verylow) with ConstGas {
  val i: Int = code - 0x60

  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val n = i + 1
    val bytes = state.program.getBytes(state.pc + 1, n)
    val word = UInt256(bytes)
    val stack1 = state.stack.push(word)
    state.withStack(stack1).step(n + 1)
  }
}

case object PUSH1  extends PushOp(0x60)
case object PUSH2  extends PushOp(0x61)
case object PUSH3  extends PushOp(0x62)
case object PUSH4  extends PushOp(0x63)
case object PUSH5  extends PushOp(0x64)
case object PUSH6  extends PushOp(0x65)
case object PUSH7  extends PushOp(0x66)
case object PUSH8  extends PushOp(0x67)
case object PUSH9  extends PushOp(0x68)
case object PUSH10 extends PushOp(0x69)
case object PUSH11 extends PushOp(0x6a)
case object PUSH12 extends PushOp(0x6b)
case object PUSH13 extends PushOp(0x6c)
case object PUSH14 extends PushOp(0x6d)
case object PUSH15 extends PushOp(0x6e)
case object PUSH16 extends PushOp(0x6f)
case object PUSH17 extends PushOp(0x70)
case object PUSH18 extends PushOp(0x71)
case object PUSH19 extends PushOp(0x72)
case object PUSH20 extends PushOp(0x73)
case object PUSH21 extends PushOp(0x74)
case object PUSH22 extends PushOp(0x75)
case object PUSH23 extends PushOp(0x76)
case object PUSH24 extends PushOp(0x77)
case object PUSH25 extends PushOp(0x78)
case object PUSH26 extends PushOp(0x79)
case object PUSH27 extends PushOp(0x7a)
case object PUSH28 extends PushOp(0x7b)
case object PUSH29 extends PushOp(0x7c)
case object PUSH30 extends PushOp(0x7d)
case object PUSH31 extends PushOp(0x7e)
case object PUSH32 extends PushOp(0x7f)


sealed abstract class DupOp private(code: Int, val i: Int) extends OpCode(code, i + 1, i + 2, G_verylow) with ConstGas {
  def this(code: Int) = this(code, code - 0x80)

  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val stack1 = state.stack.dup(i)
    state.withStack(stack1).step()
  }
}

case object DUP1  extends DupOp(0x80)
case object DUP2  extends DupOp(0x81)
case object DUP3  extends DupOp(0x82)
case object DUP4  extends DupOp(0x83)
case object DUP5  extends DupOp(0x84)
case object DUP6  extends DupOp(0x85)
case object DUP7  extends DupOp(0x86)
case object DUP8  extends DupOp(0x87)
case object DUP9  extends DupOp(0x88)
case object DUP10 extends DupOp(0x89)
case object DUP11 extends DupOp(0x8a)
case object DUP12 extends DupOp(0x8b)
case object DUP13 extends DupOp(0x8c)
case object DUP14 extends DupOp(0x8d)
case object DUP15 extends DupOp(0x8e)
case object DUP16 extends DupOp(0x8f)


sealed abstract class SwapOp(code: Int, val i: Int) extends OpCode(code, i + 2, i + 2, G_verylow) with ConstGas {
  def this(code: Int) = this(code, code - 0x90)

  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val stack1 = state.stack.swap(i + 1)
    state.withStack(stack1).step()
  }
}

case object SWAP1  extends SwapOp(0x90)
case object SWAP2  extends SwapOp(0x91)
case object SWAP3  extends SwapOp(0x92)
case object SWAP4  extends SwapOp(0x93)
case object SWAP5  extends SwapOp(0x94)
case object SWAP6  extends SwapOp(0x95)
case object SWAP7  extends SwapOp(0x96)
case object SWAP8  extends SwapOp(0x97)
case object SWAP9  extends SwapOp(0x98)
case object SWAP10 extends SwapOp(0x99)
case object SWAP11 extends SwapOp(0x9a)
case object SWAP12 extends SwapOp(0x9b)
case object SWAP13 extends SwapOp(0x9c)
case object SWAP14 extends SwapOp(0x9d)
case object SWAP15 extends SwapOp(0x9e)
case object SWAP16 extends SwapOp(0x9f)


sealed abstract class LogOp(code: Int, val i: Int) extends OpCode(code, i + 2, 0, G_log) {
  def this(code: Int) = this(code, code - 0xa0)

  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(offset, size, topics @ _*), stack1) = state.stack.pop(delta)
    val (data, memory) = state.memory.load(offset, size)
    val logEntry = TxLogEntry(state.env.ownerAddr, topics.map(_.bytes), data)

    state.withStack(stack1).withMemory(memory).withLog(logEntry).step()
  }

  protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): UInt256 = {
    val (Seq(offset, size, _*), stack1) = state.stack.pop(delta)
    val memCost = calcMemCost(state.memory.size, offset, size)
    val logCost = G_logdata * size + i * G_logtopic
    memCost + logCost
  }
}

case object LOG0 extends LogOp(0xa0)
case object LOG1 extends LogOp(0xa1)
case object LOG2 extends LogOp(0xa2)
case object LOG3 extends LogOp(0xa3)
case object LOG4 extends LogOp(0xa4)


sealed abstract class CallOp(code: Int, delta: Int, alpha: Int) extends OpCode(code, delta, alpha, G_zero) {

  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(gas, to, callValue, inOffset, inSize, outOffset, outSize), stack1) = getParams(state)

    val (inputData, mem1) = state.memory.load(inOffset, inSize)
    val endowment = if (this == DELEGATECALL) UInt256.Zero else callValue

    val startGas = {
      val gExtra = gasExtra(state, endowment, Address(to))
      val gCap = gasCap(state, gas, gExtra)
      if (endowment.isZero) gCap else gCap + G_callstipend
    }

    lazy val result = {
      val toAddr = Address(to)

      val (world1, owner, caller) = this match {
        case CALL =>
          val withTransfer = state.world.transfer(state.ownAddress, toAddr, endowment)
          (withTransfer, toAddr, state.ownAddress)

        case CALLCODE =>
          (state.world, state.ownAddress, state.ownAddress)

        case DELEGATECALL =>
          (state.world, state.ownAddress, state.env.callerAddr)
      }

      val env = state.env.copy(
        ownerAddr = owner,
        callerAddr = caller,
        inputData = inputData,
        value = callValue,
        program = Program(world1.getCode(toAddr)),
        callDepth = state.env.callDepth + 1)

      val context: ProgramContext[W, S] = state.context.copy(
        env = env,
        startGas = startGas,
        world = world1)

      VM.run(context)
    }

    val validCall =
      state.env.callDepth < OpCode.MaxCallDepth &&
      endowment <= state.ownBalance

    if (!validCall || result.error.isDefined) {
      val stack2 = stack1.push(UInt256.Zero)
      val gasAdjustment = if (validCall) UInt256.Zero else -startGas

      state
        .withStack(stack2)
        .withMemory(mem1)
        .spendGas(gasAdjustment)
        .step()

    } else {
      val stack2 = stack1.push(UInt256.One)
      val output = result.returnData.take(outSize.toInt)
      val mem2 = mem1.store(outOffset, output)

      state
        .spendGas(-result.gasRemaining)
        .withStack(stack2)
        .withMemory(mem2)
        .withWorld(result.world)
        .withAddressesToDelete(result.addressesToDelete)
        .step()
    }
  }

  protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): UInt256 = {
    val (Seq(gas, to, callValue, inOffset, inSize, outOffset, outSize), _) = getParams(state)
    val endowment = if (this == DELEGATECALL) UInt256.Zero else callValue

    val memCostIn = calcMemCost(state.memory.size, inOffset, inSize)
    val memCostOut = calcMemCost(state.memory.size, outOffset, outSize)
    val memCost = memCostIn max memCostOut

    // FIXME: these are calculated twice (for gas and exec), especially account existence. Can we do better?
    val gExtra = gasExtra(state, endowment, Address(to))
    val gCap = gasCap(state, gas, gExtra)
    memCost + gCap + gExtra
  }

  private def getParams[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): (Seq[UInt256], Stack) = {
    val (Seq(gas, to), stack1) = state.stack.pop(2)
    val (value, stack2) = if (this == DELEGATECALL) (state.env.value, stack1) else stack1.pop
    val (Seq(inOffset, inSize, outOffset, outSize), stack3) = stack2.pop(4)
    Seq(gas, to, value, inOffset, inSize, outOffset, outSize) -> stack3
  }

  private def gasCap[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S], g: UInt256, gExtra: UInt256): UInt256 = {
    if (state.gas < gExtra)
      g
    else {
      val d = state.gas - gExtra
      val l = d - d / 64
      l min g
    }
  }

  private def gasExtra[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S], endowment: UInt256, to: Address): UInt256 = {
    val c_new = if (!state.world.accountExists(to) && this == CALL) G_newaccount else UInt256.Zero
    val c_xfer = if (endowment.isZero) UInt256.Zero else G_callvalue
    G_call + c_xfer + c_new
  }
}

case object CALL extends CallOp(0xf1, 7, 1)
case object CALLCODE extends CallOp(0xf2, 7, 1)
case object DELEGATECALL extends CallOp(0xf4, 6, 1)

case object RETURN extends OpCode(0xf3, 2, 0, G_zero) {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(offset, size), stack1) = state.stack.pop(2)
    val (ret, mem1) = state.memory.load(offset, size)
    state.withStack(stack1).withReturnData(ret).withMemory(mem1).halt
  }

  protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): UInt256 = {
    val (Seq(offset, size), _) = state.stack.pop(2)
    calcMemCost(state.memory.size, offset, size)
  }
}

case object INVALID extends OpCode(0xfe, 0, 0, G_zero) with ConstGas {
  protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] =
    state.withError(InvalidOpCode(code))
}
