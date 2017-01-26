package io.iohk.ethereum.vm

import akka.util.ByteString
import cats.syntax.either._
import io.iohk.ethereum.crypto.sha3

// scalastyle:off magic.number
object OpCode {

  val opcodes: List[OpCode] = List(
    STOP,
    ADD,
    MUL,
    SUB,
    DIV,
    //SDIV,
    //MOD,
    //SMOD,
    //ADDMOD,
    //MULMOD,
    EXP,
    //SIGNEXTEND,

    LT,
    //GT,
    //SLT,
    //SGT,
    EQ,
    ISZERO,
    AND,
    //OR,
    //XOR,
    NOT,
    //BYTE,

    SHA3,

    //ADDRESS,
    //BALANCE,
    //ORIGIN,
    //CALLER,
    CALLVALUE,
    CALLDATALOAD,
    //CALLDATASIZE,
    //CALLDATACOPY,
    //CODESIZE,
    CODECOPY,
    //GASPRICE,
    //EXTCODESIZE,
    EXTCODECOPY,

    //BLOCKHASH,
    //COINBASE,
    //TIMESTAMP,
    //NUMBER,
    //DIFFICULTY,
    //GASLIMIT,

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
    //GAS,
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
    //CALL,
    //CALLCODE,
    RETURN
    //DELEGATECALL,
    //SUICIDE
  )

  val byteToOpCode: Map[Byte, OpCode] =
    opcodes.map(op => op.code -> op).toMap
}

/**
  * @param code Opcode byte representation
  */
sealed abstract class OpCode(val code: Byte, val pop: Int, val push: Int) {
  def this(code: Int, pop: Int, push: Int) = this(code.toByte, pop, push)

  def execute(state: ProgramState): ProgramState

  val diff = push - pop
}

case object STOP extends OpCode(0x00, 0, 0) {
  def execute(state: ProgramState): ProgramState =
    state.halt
}

sealed abstract class BinaryOp(code: Byte, f: (DataWord, DataWord) => DataWord) extends OpCode(code, 2, 1) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(2)
      (Seq(a, b), stack1) = popped
      res = f(a, b)
      stack2 <- stack1.push(res)
    } yield state.withStack(stack2).step()

    updatedState.valueOr(state.withError)
  }
}

sealed abstract class UnaryOp(code: Byte, f: DataWord => DataWord) extends OpCode(code, 1, 1) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop
      (a, stack1) = popped
      res = f(a)
      stack2 <- stack1.push(res)
    } yield state.withStack(stack2).step()

    updatedState.valueOr(state.withError)
  }
}

case object ADD extends BinaryOp(0x01, _ + _)

case object MUL extends BinaryOp(0x02, _ * _)

case object SUB extends BinaryOp(0x03, _ - _)

case object DIV extends OpCode(0x04) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(2)
      (Seq(a, b), stack1) = popped
      res <- if (b != 0) (a / b).asRight else DivisionByZero.asLeft
      stack2 <- stack1.push(res)
    } yield state.withStack(stack2).step()

    updatedState.valueOr(state.withError)
  }
}

case object EXP extends BinaryOp(0x0a, _ ** _)

case object LT extends BinaryOp(0x10, (a, b) => DataWord(a < b))

case object EQ extends BinaryOp(0x14, (a, b) => DataWord(a == b))

case object ISZERO extends UnaryOp(0x15, a => DataWord(a == 0))

case object AND extends BinaryOp(0x16, _ & _)

case object NOT extends UnaryOp(0x19, ~_)

case object SHA3 extends OpCode(0x20) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(2)
      (Seq(offset, size), stack1: Stack) = popped
      //FIXME: use Memory functions with proper error handling
      (input, mem1) = state.memory.load(offset, size)
      hash = sha3(input.toArray)
      ret = DataWord(ByteString(hash))
      stack2 <- stack1.push(ret)
    } yield state.withStack(stack2).withMemory(mem1).step()

    updatedState.valueOr(state.withError)
  }
}

case object CALLVALUE extends OpCode(0x34) {
  def execute(state: ProgramState): ProgramState =
    state.stack.push(DataWord(state.invoke.callValue))
      .map(state.withStack(_).step())
      .valueOr(state.withError)
}

case object CALLDATALOAD extends OpCode(0x35) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop
      (offset, stack1) = popped

      i = offset.intValue
      data = state.invoke.callData.slice(i, i + 32).padTo(32, 0.toByte)

      stack2 <- stack1.push(DataWord(data))
    } yield state.withStack(stack2).step()

    updatedState.valueOr(state.withError)
  }
}

case object CODECOPY extends OpCode(0x39) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(3)
      (Seq(memOffset, codeOffset, size), stack1) = popped
      bytes <- state.program.getBytes(codeOffset.intValue, size.intValue)
      mem1 = state.memory.store(memOffset, bytes)
    } yield state.withStack(stack1).withMemory(mem1).step()

    updatedState.valueOr(state.withError)
  }
}

case object EXTCODECOPY extends OpCode(0x3c) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(4)
      (Seq(address, memOffset, codeOffset, size), stack1) = popped
      codeCopy: ByteString = ??? //TODO: program.getCode(address).drop(codeOffset)
      mem1 = state.memory.store(memOffset, codeCopy)
    } yield state.withStack(stack1).withMemory(mem1).step()

    updatedState.valueOr(state.withError)
  }
}

case object POP extends OpCode(0x50) {
  def execute(state: ProgramState): ProgramState = {
    state.stack.pop
      .map { case (_, stack) => state.withStack(stack).step() }
      .valueOr(state.withError)
  }
}

case object MLOAD extends OpCode(0x51) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop
      (addr, stack1) = popped
      (word, mem1) = state.memory.load(addr)
      stack2 <- stack1.push(word)
    } yield state.withStack(stack2).withMemory(mem1).step()

    updatedState.valueOr(state.withError)
  }
}

case object MSTORE extends OpCode(0x52) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(2)
      (Seq(addr, value), stack1) = popped
      updatedMem = state.memory.store(addr, value)
    } yield state.withStack(stack1).withMemory(updatedMem).step()

    updatedState.valueOr(state.withError)
  }
}

case object SLOAD extends OpCode(0x54) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop
      (addr, stack1) = popped
      //TODO: handle invalid address
      value = state.storage.load(addr.intValue)
      stack2 <- stack1.push(value)
    } yield state.withStack(stack2).step()

    updatedState.valueOr(state.withError)
  }
}


case object SSTORE extends OpCode(0x55) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(2)
      (Seq(addr, value), stack1) = popped
      //TODO: handle invalid address
      updatedStorage = state.storage.save(addr.intValue, value)
    } yield state.withStack(stack1).withStorage(updatedStorage).step()

    updatedState.valueOr(state.withError)
  }
}


case object JUMP extends OpCode(0x56) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop
      (pos, stack1) = popped
    } yield state.withStack(stack1).goto(pos.intValue)

    updatedState.valueOr(state.withError)
  }
}

case object JUMPI extends OpCode(0x57) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(2)
      (Seq(pos, cond), stack1) = popped
      nextPos = if (cond != 0) pos.intValue else state.pc + 1
    } yield state.withStack(stack1).goto(nextPos)

    updatedState.valueOr(state.withError)
  }
}

case object JUMPDEST extends OpCode(0x5b) {
  def execute(state: ProgramState): ProgramState = {
    //TODO: what is it for, really?
    state.step()
  }
}

sealed abstract class PushOp(code: Int) extends OpCode(code.toByte, 0, 1) {
  def execute(state: ProgramState): ProgramState = {
    val n = code - PUSH1.code + 1

    val updatedState = for {
      bytes <- state.program.getBytes(state.pc + 1, n)
      word = DataWord(bytes)
      stack <- state.stack.push(word)
    } yield state.withStack(stack).step(n + 1)

    updatedState.valueOr(state.withError)
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


sealed abstract class DupOp(code: Int) extends OpCode(code.toByte, code - DUP1.code + 1, code - DUP1.code + 2) {
  def execute(state: ProgramState): ProgramState = {
    val i = code - DUP1.code
    val updatedState = state.stack.dup(i).map(state.withStack(_).step())
    updatedState.valueOr(state.withError)
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


sealed trait SwapOp {
  def code: Byte

  def execute(state: ProgramState): ProgramState = {
    val i = code - SWAP1.code + 1
    val updatedState = state.stack.swap(i).map(state.withStack(_).step())
    updatedState.valueOr(state.withError)
  }
}

case object SWAP1  extends OpCode(0x90) with SwapOp
case object SWAP2  extends OpCode(0x91) with SwapOp
case object SWAP3  extends OpCode(0x92) with SwapOp
case object SWAP4  extends OpCode(0x93) with SwapOp
case object SWAP5  extends OpCode(0x94) with SwapOp
case object SWAP6  extends OpCode(0x95) with SwapOp
case object SWAP7  extends OpCode(0x96) with SwapOp
case object SWAP8  extends OpCode(0x97) with SwapOp
case object SWAP9  extends OpCode(0x98) with SwapOp
case object SWAP10 extends OpCode(0x99) with SwapOp
case object SWAP11 extends OpCode(0x9a) with SwapOp
case object SWAP12 extends OpCode(0x9b) with SwapOp
case object SWAP13 extends OpCode(0x9c) with SwapOp
case object SWAP14 extends OpCode(0x9d) with SwapOp
case object SWAP15 extends OpCode(0x9e) with SwapOp
case object SWAP16 extends OpCode(0x9f) with SwapOp


sealed trait LogOp {
  def code: Byte

  def execute(state: ProgramState): ProgramState = {
    val i = code - LOG1.code + 2
    val updatedState = for {
      popped <- state.stack.pop(i)
      (_, stack1) = popped
      //TODO: implement logging
    } yield state.withStack(stack1).step()

    updatedState.valueOr(state.withError)
  }
}

case object LOG0 extends OpCode(0xa0) with LogOp
case object LOG1 extends OpCode(0xa1) with LogOp
case object LOG2 extends OpCode(0xa2) with LogOp
case object LOG3 extends OpCode(0xa3) with LogOp
case object LOG4 extends OpCode(0xa4) with LogOp


case object RETURN extends OpCode(0xf3) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(2)
      (Seq(offset, size), stack1) = popped
      (ret, mem1) = state.memory.load(offset, size)
    } yield state.withStack(stack1).withReturnData(ret).withMemory(mem1).halt

    updatedState.valueOr(state.withError)
  }
}
