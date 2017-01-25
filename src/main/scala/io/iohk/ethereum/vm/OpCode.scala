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
sealed abstract class OpCode(val code: Byte) {
  def this(code: Int) = this(code.toByte)

  def execute(state: ProgramState): ProgramState
}

case object STOP extends OpCode(0x00) {
  def execute(state: ProgramState): ProgramState =
    state.halt
}

sealed abstract class BinaryOp(code: Byte, f: (DataWord, DataWord) => DataWord) extends OpCode(code) {
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

sealed abstract class UnaryOp(code: Byte, f: DataWord => DataWord) extends OpCode(code) {
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
      (Seq(offset, size), stack1) = popped
      //FIXME: use Memory functions with proper error handling
      input = state.memory.buffer.slice(offset.intValue, size.intValue)
      hash = sha3(input.toArray)
      ret = DataWord(ByteString(hash))
      stack2 <- stack1.push(ret)
    } yield state.withStack(stack2).step()

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
      mem1 = state.memory.storeBytes(memOffset.intValue, bytes)
    } yield state.withStack(stack1).withMemory(mem1).step()

    updatedState.valueOr(state.withError)
  }
}

case object EXTCODECOPY extends OpCode(0x3c) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(4)
      (Seq(address, memOffset, codeOffset, size), stack1) = popped
      //TODO: copy the code
      mem1 = (0 until size.intValue).foldLeft(state.memory)((mem, i) => mem.store(i, 0.toByte))
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

      //FIXME: handle errors
      word = state.memory.load(addr.intValue)
      stack2 <- stack1.push(word)
    } yield state.withStack(stack2).step()

    updatedState.valueOr(state.withError)
  }
}

case object MSTORE extends OpCode(0x52) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(2)
      (Seq(addr, value), stack1) = popped
      //TODO: handle invalid address
      updatedMem = state.memory.store(addr.intValue, value)
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

sealed trait PushOp {
  def code: Byte

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

case object PUSH1  extends OpCode(0x60) with PushOp
case object PUSH2  extends OpCode(0x61) with PushOp
case object PUSH3  extends OpCode(0x62) with PushOp
case object PUSH4  extends OpCode(0x63) with PushOp
case object PUSH5  extends OpCode(0x64) with PushOp
case object PUSH6  extends OpCode(0x65) with PushOp
case object PUSH7  extends OpCode(0x66) with PushOp
case object PUSH8  extends OpCode(0x67) with PushOp
case object PUSH9  extends OpCode(0x68) with PushOp
case object PUSH10 extends OpCode(0x69) with PushOp
case object PUSH11 extends OpCode(0x6a) with PushOp
case object PUSH12 extends OpCode(0x6b) with PushOp
case object PUSH13 extends OpCode(0x6c) with PushOp
case object PUSH14 extends OpCode(0x6d) with PushOp
case object PUSH15 extends OpCode(0x6e) with PushOp
case object PUSH16 extends OpCode(0x6f) with PushOp
case object PUSH17 extends OpCode(0x70) with PushOp
case object PUSH18 extends OpCode(0x71) with PushOp
case object PUSH19 extends OpCode(0x72) with PushOp
case object PUSH20 extends OpCode(0x73) with PushOp
case object PUSH21 extends OpCode(0x74) with PushOp
case object PUSH22 extends OpCode(0x75) with PushOp
case object PUSH23 extends OpCode(0x76) with PushOp
case object PUSH24 extends OpCode(0x77) with PushOp
case object PUSH25 extends OpCode(0x78) with PushOp
case object PUSH26 extends OpCode(0x79) with PushOp
case object PUSH27 extends OpCode(0x7a) with PushOp
case object PUSH28 extends OpCode(0x7b) with PushOp
case object PUSH29 extends OpCode(0x7c) with PushOp
case object PUSH30 extends OpCode(0x7d) with PushOp
case object PUSH31 extends OpCode(0x7e) with PushOp
case object PUSH32 extends OpCode(0x7f) with PushOp


sealed trait DupOp {
  def code: Byte

  def execute(state: ProgramState): ProgramState = {
    val i = code - DUP1.code
    val updatedState = state.stack.dup(i).map(state.withStack(_).step())
    updatedState.valueOr(state.withError)
  }
}

case object DUP1  extends OpCode(0x80) with DupOp
case object DUP2  extends OpCode(0x81) with DupOp
case object DUP3  extends OpCode(0x82) with DupOp
case object DUP4  extends OpCode(0x83) with DupOp
case object DUP5  extends OpCode(0x84) with DupOp
case object DUP6  extends OpCode(0x85) with DupOp
case object DUP7  extends OpCode(0x86) with DupOp
case object DUP8  extends OpCode(0x87) with DupOp
case object DUP9  extends OpCode(0x88) with DupOp
case object DUP10 extends OpCode(0x89) with DupOp
case object DUP11 extends OpCode(0x8a) with DupOp
case object DUP12 extends OpCode(0x8b) with DupOp
case object DUP13 extends OpCode(0x8c) with DupOp
case object DUP14 extends OpCode(0x8d) with DupOp
case object DUP15 extends OpCode(0x8e) with DupOp
case object DUP16 extends OpCode(0x8f) with DupOp


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
      //FIXME: use Memory functions with proper error handling
      ret = state.memory.buffer.slice(offset.intValue, offset.intValue + size.intValue)
    } yield state.withStack(stack1).withReturnData(ret).halt

    updatedState.valueOr(state.withError)
  }
}
