package io.iohk.ethereum.vm

import cats.syntax.either._

// scalastyle:off magic.number
object OpCode {

  val opcodes: List[OpCode] = List(
    STOP,
    ADD,
    DIV,
    EQ,
    AND,
    CALLVALUE,
    CALLDATALOAD,
    EXTCODECOPY,
    MSTORE,
    SSTORE,
    JUMP,
    JUMPI,
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
    RETURN
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

case object ADD extends OpCode(0x01) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(2)
      (Seq(a, b), stack1) = popped
      res = a + b
      stack2 <- stack1.push(res)
    } yield state.withStack(stack2).step()

    updatedState.valueOr(state.withError)
  }
}

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

case object EQ extends OpCode(0x14) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(2)
      (Seq(a, b), stack1) = popped
      res = if (a == b) 1 else 0
      stack2 <- stack1.push(DataWord(res))
    } yield state.withStack(stack2).step()

    updatedState.valueOr(state.withError)
  }
}

case object AND extends OpCode(0x16) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(2)
      (Seq(a, b), stack1) = popped
      res = a & b
      stack2 <- stack1.push(res)
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
      //TODO: handle invalid offset
      data = DataWord(state.invoke.getCallData(offset.intValue))
      stack2 <- stack1.push(data)
    } yield state.withStack(stack2).step()

    updatedState.valueOr(state.withError)
  }
}

case object EXTCODECOPY extends OpCode(0x39) {
  def execute(state: ProgramState): ProgramState = ???
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
    val bytes = state.program.getBytes(state.pc + 1, n)
    val word = DataWord(bytes)

    val updatedState = for {
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
    val i = code - DUP1.code
    val updatedState = state.stack.dup(i).map(state.withStack(_).step())
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

case object RETURN extends OpCode(0xf3) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(2)
      (Seq(offset, size), stack1) = popped
      //FIXME: use Memory functions with proper error handling
      ret = state.memory.buffer.slice(offset.intValue, size.intValue)
    } yield state.withStack(stack1).withReturnData(ret).halt

    updatedState.valueOr(state.withError)
  }
}
