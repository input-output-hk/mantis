package io.iohk.ethereum.vm


object OpCode {
  val byteToOpCode: Map[Byte, OpCode] = Map(
    ADD.code -> ADD
  ).withDefault(InvalidOpCode)
}

/**
  * @param code Opcode byte representation
  * @param delta arguments - number of items to pop from the stack
  * @param alpha return value - number of items to push to the stack
  */
sealed abstract class OpCode(val code: Byte, val delta: Int, val alpha: Int) {
  def execute(state: ProgramState): ProgramState
}


case object STOP extends OpCode(0, 0, 0) {
  def execute(state: ProgramState): ProgramState =
    state.copy(halt = true)
}

case object ADD extends OpCode(0x01, 2, 1) {
  def execute(state: ProgramState): ProgramState = {
    val (a, stack1) = state.stack.pop
    val (b, stack2) = stack1.pop

    val res = DataWord(a.value + b.value)
    val stack3 = stack2.push(res)

    state.copy(stack = stack3, pc = state.pc + 1)
  }
}

case class InvalidOpCode(c: Byte) extends OpCode(c, -1, -1) {
  def execute(state: ProgramState): ProgramState =
    throw new IllegalArgumentException(s"Invalid opcode: $c")
}
