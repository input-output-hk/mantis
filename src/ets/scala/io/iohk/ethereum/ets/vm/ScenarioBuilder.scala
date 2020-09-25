package io.iohk.ethereum.ets.vm

import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.domain.{Account, Address, BlockHeader, UInt256}
import io.iohk.ethereum.ets.common.AccountState
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.vm.MockWorldState._
import io.iohk.ethereum.vm._
import Fixtures.blockchainConfig

object ScenarioBuilder {

  def prepareContext(scenario: VMScenario): PC = {
    val blockHeader = prepareHeader(scenario.env)
    val world = prepareWorld(scenario.pre, scenario.env.currentNumber, scenario.exec)

    import scenario.exec._

    ProgramContext(
      callerAddr = caller,
      originAddr = origin,
      recipientAddr = Some(address),
      gasPrice = UInt256(gasPrice),
      startGas = gas,
      inputData = data,
      value = UInt256(value),
      endowment = 0, // the VM under test is not expected to transfer funds
      doTransfer = false,
      blockHeader = blockHeader,
      callDepth = 0,
      world = world,
      initialAddressesToDelete = Set(),
      evmConfig = evmConfig,
      originalWorld = world
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
      bEmpty,
      None
    )

  def prepareWorld(accounts: Map[Address, AccountState], blockNumber: BigInt, exec: Exec): MockWorldState = {
    val zeroWorld = MockWorldState(numberOfHashes = blockNumber.u256).saveAccount(exec.caller, Account.empty())
    accounts.foldLeft(zeroWorld) { case (world, (address, state)) =>
      val storage = MockStorage(state.storage.map { case (k, v) => (k.toBigInt, v.toBigInt) })
      val account = Account(nonce = state.nonce.u256, balance = state.balance.u256)
      world
        .saveAccount(address, account)
        .saveCode(address, state.code)
        .saveStorage(address, storage)
    }
  }

  val evmConfig: EvmConfig = {
    val baseConfig = EvmConfig.HomesteadConfigBuilder(blockchainConfig)
    val opCodes = baseConfig.opCodes.diff(List(CREATE, CALL, CALLCODE)) ++ List(TestCREATE, TestCALL, TestCALLCODE)
    baseConfig.copy(
      opCodeList = EvmConfig.OpCodeList(opCodes),
      traceInternalTransactions = true
    )
  }
}
