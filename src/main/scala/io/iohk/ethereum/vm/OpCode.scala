package io.iohk.ethereum.vm

// scalastyle:off magic.number
object OpCode {

  val opcodes: List[OpCode] = List(
    STOP,
    ADD
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
    val (Seq(b, a), updatedStack) = state.stack.pop(2)
    val res = a + b
    state.copy(stack = updatedStack.push(res), pc = state.pc + 1)
  }
}

case object DIV extends OpCode(0x04, 2, 1) {
  def execute(state: ProgramState): ProgramState = {
    val (Seq(b, a), updatedStack) = state.stack.pop(2)

    if (b != 0) {
      val res = a / b
      state.copy(stack = updatedStack.push(res), pc = state.pc + 1)
    } else {
      state.copy(halt = true, error = Some(DivisionByZero(state.pc)))
    }
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

