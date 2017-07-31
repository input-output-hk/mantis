package io.iohk.ethereum.ets.vm

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.vm._

case object TestCREATE extends CreateOp {
  override protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(endowment, inOffset, inSize), stack1) = state.stack.pop(3)
    val validCall = state.env.callDepth < EvmConfig.MaxCallDepth && endowment <= state.ownBalance

    if (!validCall) {
      val stack2 = stack1.push(UInt256.Zero)
      state.withStack(stack2).step()
    } else {
      val (newAddress, world1) = state.world.createAddressWithOpCode(state.env.ownerAddr)
      //val world2 = world1.transfer(state.env.ownerAddr, newAddress, endowment)
      val availableGas = state.gas - (constGasFn(state.config.feeSchedule) + varGas(state))
      val startGas = state.config.gasCap(availableGas)
      val (initCode, memory1) = state.memory.load(inOffset, inSize)
      val internalTx = InternalTransaction(CREATE, state.env.ownerAddr, None, startGas, initCode, endowment)
      val stack2 = stack1.push(newAddress.toUInt256)

      state
        .withStack(stack2)
        .withWorld(world1)
        .withMemory(memory1)
        .withInternalTxs(internalTx :: Nil)
        .step()
    }
  }
}

case object TestCALL extends CallOp(0xf1, 7, 1) {
  override protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(gas, to, endowment, inOffset, inSize, outOffset, outSize), stack1) = getParams(state)

    val validCall = state.env.callDepth < EvmConfig.MaxCallDepth && endowment <= state.ownBalance

    if (!validCall) {
      val stack2 = stack1.push(UInt256.Zero)
      state.withStack(stack2).step()

    } else {
      val stack2 = stack1.push(UInt256.One)
      val startGas = calcStartGas(state, gas, endowment, to)
      val (inputData, mem1) = state.memory.load(inOffset, inSize)
      val internalTx = InternalTransaction(this, state.env.ownerAddr, Some(Address(to)), startGas, inputData, endowment)

      state
        .withStack(stack2)
        .withMemory(mem1)
        .withInternalTxs(internalTx :: Nil)
        .step()
    }
  }
}
