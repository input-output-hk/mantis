package io.iohk.ethereum.vm

import cats.syntax.either._

// scalastyle:off magic.number
object OpCode {

  val opcodes: List[OpCode] = List(
    STOP,
    ADD,
    DIV,
    CALLDATALOAD,
    MSTORE,
    SSTORE,
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
    //...
    PUSH28,
    PUSH29,
    PUSH30,
    PUSH31,
    PUSH32
   )

  val byteToOpCode: Map[Byte, OpCode] =
    opcodes.map(op => op.code -> op).toMap
}

//TODO: do we really need delta an alpha?
/**
  * @param code Opcode byte representation
  * @param delta arguments - number of items to pop from the stack
  * @param alpha return value - number of items to push to the stack
  */
sealed abstract class OpCode(val code: Byte, val delta: Int, val alpha: Int) {
  def execute(state: ProgramState): ProgramState
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


case object STOP extends OpCode(0, 0, 0) {
  def execute(state: ProgramState): ProgramState =
    state.copy(halt = true)
}

case object ADD extends OpCode(0x01, 2, 1) {
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

case object DIV extends OpCode(0x04, 2, 1) {
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

case object CALLDATALOAD extends OpCode(0x34, 1, 1) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop
      (offset, stack1) = popped
      //TODO: handle invalid offset
      data = DataWord(state.program.getCallData(offset.intValue))
      stack2 <- stack1.push(data)
    } yield state.withStack(stack2).step()

    updatedState.valueOr(state.withError)
  }
}

case object MSTORE extends OpCode(0x52, 2, 0) {
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


case object SSTORE extends OpCode(0x55, 2, 0) {
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

case object JUMPI extends OpCode(0x57, 2, 0) {
  def execute(state: ProgramState): ProgramState = {
    val updatedState = for {
      popped <- state.stack.pop(2)
      (Seq(pos, cond), stack1) = popped
      nextPos = if (cond != 0) pos.intValue else state.pc + 1
    } yield state.withStack(stack1).step()

    updatedState.valueOr(state.withError)
  }
}

case object JUMPDEST extends OpCode(0x5b, 0, 0) {
  def execute(state: ProgramState): ProgramState = {
    //TODO: what is it for, really?
    state.step()
  }
}

case object PUSH1  extends OpCode(0x60, 0, 1) with PushOp
case object PUSH2  extends OpCode(0x61, 0, 1) with PushOp
case object PUSH3  extends OpCode(0x62, 0, 1) with PushOp
case object PUSH4  extends OpCode(0x63, 0, 1) with PushOp
case object PUSH5  extends OpCode(0x64, 0, 1) with PushOp
case object PUSH6  extends OpCode(0x65, 0, 1) with PushOp
case object PUSH7  extends OpCode(0x66, 0, 1) with PushOp
case object PUSH8  extends OpCode(0x67, 0, 1) with PushOp
case object PUSH9  extends OpCode(0x68, 0, 1) with PushOp
case object PUSH10 extends OpCode(0x69, 0, 1) with PushOp
case object PUSH28 extends OpCode(0x7b, 0, 1) with PushOp
case object PUSH29 extends OpCode(0x7c, 0, 1) with PushOp
case object PUSH30 extends OpCode(0x7d, 0, 1) with PushOp
case object PUSH31 extends OpCode(0x7e, 0, 1) with PushOp
case object PUSH32 extends OpCode(0x7f, 0, 1) with PushOp
