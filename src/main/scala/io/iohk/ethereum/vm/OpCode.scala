package io.iohk.ethereum.vm

// scalastyle:off magic.number
object OpCode {

  val opcodes: List[OpCode] = List(
    STOP,
    ADD,
    DIV,
    PUSH1,
    PUSH2,
    PUSH3,
    PUSH4,
    PUSH5,
    PUSH6,
    PUSH7,
    PUSH8,
    PUSH9,
    PUSH10
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
    val stack = state.stack.push(word)
    state.copy(stack = stack, pc = state.pc + n + 1)
  }
}


case object STOP extends OpCode(0, 0, 0) {
  def execute(state: ProgramState): ProgramState =
    state.copy(halt = true)
}

case object ADD extends OpCode(0x01, 2, 1) {
  def execute(state: ProgramState): ProgramState = {
    val (Seq(a, b), updatedStack) = state.stack.pop(2)
    val res = a + b
    state.copy(stack = updatedStack.push(res), pc = state.pc + 1)
  }
}

case object DIV extends OpCode(0x04, 2, 1) {
  def execute(state: ProgramState): ProgramState = {
    val (Seq(a, b), updatedStack) = state.stack.pop(2)

    if (b != 0) {
      val res = a / b
      state.copy(stack = updatedStack.push(res), pc = state.pc + 1)
    } else {
      state.copy(halt = true, error = Some(DivisionByZero(state.pc)))
    }
  }
}

case object CALLDATALOAD extends OpCode(0x34, 1, 1) {
  def execute(state: ProgramState): ProgramState = {
    val (offset, stack1) = state.stack.pop
    //TODO: handle invalid offset
    val data = DataWord(state.program.getCallData(offset.intValue))
    val stack2 = stack1.push(data)
    state.copy(stack = stack2, pc = state.pc + 1)
  }
}

case object MSTORE extends OpCode(0x52, 2, 0) {
  def execute(state: ProgramState): ProgramState = {
    val (Seq(addr, value), updateStack) = state.stack.pop(2)
    //TODO: handle invalid address
    val updatedMem = state.memory.save(addr.intValue, value)

    state.copy(stack = updateStack, memory = updatedMem, pc = state.pc + 1)
  }
}


case object SSTORE extends OpCode(0x55, 2, 0) {
  def execute(state: ProgramState): ProgramState = {
    val (Seq(addr, value), updateStack) = state.stack.pop(2)
    //TODO: handle invalid address
    val updatedStorage = state.storage.save(addr.intValue, value)

    state.copy(stack = updateStack, storage = updatedStorage, pc = state.pc + 1)
  }
}

case object JUMPI extends OpCode(0x57, 2, 0) {
  def execute(state: ProgramState): ProgramState = {
    val (Seq(pos, cond), updatedStack) = state.stack.pop(2)
    val nextPos = if (cond != 0) pos.intValue else state.pc + 1

    state.copy(stack = updatedStack, pc = nextPos)
  }
}

case object JUMPDEST extends OpCode(0x5b, 0, 0) {
  def execute(state: ProgramState): ProgramState = {
    //TODO: what is it for, really?
    state.copy(pc = state.pc + 1)
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

