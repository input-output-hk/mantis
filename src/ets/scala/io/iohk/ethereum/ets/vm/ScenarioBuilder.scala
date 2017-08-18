package io.iohk.ethereum.ets.vm

import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.domain.{Account, Address, BlockHeader}
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.vm.MockWorldState._
import io.iohk.ethereum.vm._

object ScenarioBuilder {

  def prepareContext(scenario: Scenario): PC = {
    val blockHeader = prepareHeader(scenario.env)
    val execEnv = prepareExecEnv(scenario.exec, blockHeader)
    val world = prepareWorld(scenario.pre, scenario.env.currentNumber)
    val config = getConfig(blockHeader.number)

    ProgramContext(
      execEnv,
      scenario.exec.address,
      scenario.exec.gas,
      world,
      config
    )
  }

  def prepareHeader(env: Env): BlockHeader =
    BlockHeader(
      env.previousHash.getOrElse(bEmpty),
      bEmpty,
      env.currentCoinbase.bytes,
      bEmpty,
      bEmpty,
      bEmpty,
      bEmpty,
      env.currentDifficulty,
      env.currentNumber,
      env.currentGasLimit,
      0,
      env.currentTimestamp,
      bEmpty,
      bEmpty,
      bEmpty
    )

  def prepareExecEnv(exec: Exec, header: BlockHeader): ExecEnv =
    ExecEnv(
      exec.address,
      exec.caller,
      exec.origin,
      exec.gasPrice.u256,
      exec.data,
      exec.value.u256,
      Program(exec.code),
      header,
      0
    )

  def prepareWorld(accounts: Map[Address, AccountState], blockNumber: BigInt): MockWorldState = {
    val zeroWorld = MockWorldState(numberOfHashes = blockNumber.u256)
    accounts.foldLeft(zeroWorld) { case (world, (address, state)) =>
      val storage = MockStorage(state.storage)
      val account = Account(nonce = state.nonce.u256, balance = state.balance.u256)
      world
        .saveAccount(address, account)
        .saveCode(address, state.code)
        .saveStorage(address, storage)
    }
  }

  def getConfig(blockNumber: BigInt): EvmConfig = {
    val baseConfig = EvmConfig.HomesteadConfig
    val opCodes = baseConfig.opCodes.diff(List(CREATE, CALL, CALLCODE)) ++ List(TestCREATE, TestCALL, TestCALLCODE)
    baseConfig.copy(
      opCodes = opCodes,
      traceInternalTransactions = true
    )

  }
}
