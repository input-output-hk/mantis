package io.iohk.ethereum.ets.vm

import akka.util.ByteString
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
      val (newAddress, _) = state.world.createAddressWithOpCode(state.env.ownerAddr)
      val availableGas = state.gas - (constGasFn(state.config.feeSchedule) + varGas(state))
      val startGas = state.config.gasCap(availableGas)
      val (initCode, memory1) = state.memory.load(inOffset, inSize)
      val internalTx = InternalTransaction(this, state.env.ownerAddr, None, startGas, initCode, endowment)
      val stack2 = stack1.push(newAddress.toUInt256)

      state
        .withStack(stack2)
        .withMemory(memory1)
        .withInternalTxs(internalTx :: Nil)
        .step()
    }
  }
}

abstract class TestCallOp(code: Int) extends CallOp(code, 7, 1) {
  override protected def exec[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val (Seq(gas, to, endowment, inOffset, inSize, outOffset, outSize), stack1) = getParams(state)
    val toAddr = Address(to)

    val gasBack = -gas.toBigInt // not executing any code so all the gas provided is returned
    val validCall = state.env.callDepth < EvmConfig.MaxCallDepth && endowment <= state.ownBalance

    if (!validCall) {
      val stack2 = stack1.push(UInt256.Zero)
      state.withStack(stack2).spendGas(gasBack).step()

    } else {
      val stack2 = stack1.push(UInt256.One)
      val startGas = calcStartGas(state, gas, endowment, to)
      val (inputData, mem1) = state.memory.load(inOffset, inSize)
      val internalTx = internalTransaction(state.env, to, startGas, inputData, endowment)

      state
        .withStack(stack2)
        .withMemory(mem1)
        .withInternalTxs(internalTx :: Nil)
        .spendGas(gasBack)
        .step()
    }
  }

  override protected def internalTransaction(env: ExecEnv, callee: UInt256, startGas: BigInt, inputData: ByteString, endowment: UInt256): InternalTransaction = {
    val from = env.ownerAddr
    val to = if (this == TestCALL) Address(callee) else env.ownerAddr
    InternalTransaction(this, from, Some(to), startGas, inputData, endowment)
  }

  override protected def varGas[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): BigInt = {
    val (Seq(gas, to, endowment, inOffset, inSize, outOffset, outSize), _) = getParams(state)
    val fs = state.config.feeSchedule
    import fs._

    val transferCost: BigInt = if (endowment.isZero) 0 else G_callvalue - G_callstipend
    val newAccountCost: BigInt = if (!state.world.accountExists(Address(to)) && this == TestCALL) G_newaccount else 0
    val memCost = calcMemCost(state, inOffset, inSize, outOffset, outSize)

    G_call + gas + transferCost + newAccountCost + memCost
  }
}

case object TestCALL extends TestCallOp(0xf1)
case object TestCALLCODE extends TestCallOp(0xf2)
