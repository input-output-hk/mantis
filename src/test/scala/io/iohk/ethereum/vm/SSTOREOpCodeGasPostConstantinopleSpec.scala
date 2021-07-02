package io.iohk.ethereum.vm

import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}

import org.bouncycastle.util.encoders.Hex
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.Fixtures.{Blocks => BlockFixtures}
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.vm.Fixtures.blockchainConfig
import io.iohk.ethereum.vm.MockWorldState.PC
import io.iohk.ethereum.vm.MockWorldState.TestVM

class StoreOpCodeGasPostConstantinopleSpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with TestSetup {

  val defaultGaspool = 1000000

  // Spec https://eips.ethereum.org/EIPS/eip-1283
  "Net gas metering for SSTORE after Constantinople hard fork (EIP-1283)" in {
    val eip1283table = Table[String, BigInt, BigInt, BigInt](
      ("code", "original", "gasUsed", "refund"),
      ("60006000556000600055", 0, 412, 0),
      ("60006000556001600055", 0, 20212, 0),
      ("60016000556000600055", 0, 20212, 19800),
      ("60016000556002600055", 0, 20212, 0),
      ("60016000556001600055", 0, 20212, 0),
      ("60006000556000600055", 1, 5212, 15000),
      ("60006000556001600055", 1, 5212, 4800),
      ("60006000556002600055", 1, 5212, 0),
      ("60026000556000600055", 1, 5212, 15000),
      ("60026000556003600055", 1, 5212, 0),
      ("60026000556001600055", 1, 5212, 4800),
      ("60026000556002600055", 1, 5212, 0),
      ("60016000556000600055", 1, 5212, 15000),
      ("60016000556002600055", 1, 5212, 0),
      ("60016000556001600055", 1, 412, 0),
      ("600160005560006000556001600055", 0, 40218, 19800),
      ("600060005560016000556000600055", 1, 10218, 19800)
    )

    forAll(eip1283table) { (code, original, gasUsed, refund) =>
      val result =
        vm.exec(prepareProgramState(ByteString(Hex.decode(code)), original, defaultGaspool, EipToCheck.EIP1283))

      result.gasUsed shouldEqual gasUsed
      result.gasRefund shouldEqual refund
    }
  }

  // Spec https://eips.ethereum.org/EIPS/eip-2200
  "Net gas metering for SSTORE after Phoenix hard fork (EIP-2200)" in {
    val eip2200table = Table[String, BigInt, BigInt, BigInt, BigInt, Option[ProgramError]](
      ("code", "original", "gasUsed", "refund", "gaspool", "error"),
      ("60006000556000600055", 0, 1612, 0, defaultGaspool, None),
      ("60006000556001600055", 0, 20812, 0, defaultGaspool, None),
      ("60016000556000600055", 0, 20812, 19200, defaultGaspool, None),
      ("60016000556002600055", 0, 20812, 0, defaultGaspool, None),
      ("60016000556001600055", 0, 20812, 0, defaultGaspool, None),
      ("60006000556000600055", 1, 5812, 15000, defaultGaspool, None),
      ("60006000556001600055", 1, 5812, 4200, defaultGaspool, None),
      ("60006000556002600055", 1, 5812, 0, defaultGaspool, None),
      ("60026000556000600055", 1, 5812, 15000, defaultGaspool, None),
      ("60026000556003600055", 1, 5812, 0, defaultGaspool, None),
      ("60026000556001600055", 1, 5812, 4200, defaultGaspool, None),
      ("60026000556002600055", 1, 5812, 0, defaultGaspool, None),
      ("60016000556000600055", 1, 5812, 15000, defaultGaspool, None),
      ("60016000556002600055", 1, 5812, 0, defaultGaspool, None),
      ("60016000556001600055", 1, 1612, 0, defaultGaspool, None),
      ("600160005560006000556001600055", 0, 40818, 19200, defaultGaspool, None),
      ("600060005560016000556000600055", 1, 10818, 19200, defaultGaspool, None),
      ("6001600055", 1, 2306, 0, 2306, Some(OutOfGas)),
      ("6001600055", 1, 806, 0, 2307, None)
    )

    forAll(eip2200table) { (code, original, gasUsed, refund, gaspool, maybeError) =>
      val result = vm.exec(prepareProgramState(ByteString(Hex.decode(code)), original, gaspool, EipToCheck.EIP2200))

      result.gasUsed shouldEqual gasUsed
      result.gasRefund shouldEqual refund
      result.error shouldEqual maybeError
    }
  }
}

trait TestSetup {
  val vm = new TestVM

  val senderAddr: Address = Address(0xcafebabeL)
  val senderAcc: Account = Account(nonce = 1, balance = 1000000)

  val accountWithCode: ByteString => Account = code => Account.empty().withCode(kec256(code))

  def defaultWorld: MockWorldState = MockWorldState().saveAccount(senderAddr, senderAcc)

  def prepareBlockHeader(blockNumber: BigInt): BlockHeader = BlockFixtures.ValidBlock.header.copy(
    difficulty = 1000000,
    number = blockNumber,
    gasLimit = 10000000,
    gasUsed = 0,
    unixTimestamp = 0
  )

  def getContext(
      world: MockWorldState = defaultWorld,
      inputData: ByteString = bEmpty,
      eipToCheck: EipToCheck,
      gaspool: BigInt
  ): PC =
    ProgramContext(
      callerAddr = senderAddr,
      originAddr = senderAddr,
      recipientAddr = None,
      gasPrice = 1,
      startGas = gaspool,
      inputData = inputData,
      value = 100,
      endowment = 100,
      doTransfer = true,
      blockHeader = eipToCheck.blockHeader,
      callDepth = 0,
      world = world,
      initialAddressesToDelete = Set(),
      evmConfig = eipToCheck.config,
      originalWorld = world
    )

  def prepareProgramState(
      assemblyCode: ByteString,
      originalValue: BigInt,
      gaspool: BigInt,
      eipToCheck: EipToCheck
  ): ProgramState[MockWorldState, MockStorage] = {
    val newWorld = defaultWorld
      .saveAccount(senderAddr, accountWithCode(assemblyCode))
      .saveCode(senderAddr, assemblyCode)
      .saveStorage(senderAddr, MockStorage(Map(BigInt(0) -> originalValue)))

    val context: PC = getContext(newWorld, eipToCheck = eipToCheck, gaspool = gaspool)
    val env = ExecEnv(context, assemblyCode, context.originAddr)

    ProgramState(vm, context, env)
  }

  sealed trait EipToCheck {
    val blockHeader: BlockHeader
    val config: EvmConfig
  }
  object EipToCheck {
    case object EIP1283 extends EipToCheck {
      override val blockHeader: BlockHeader = prepareBlockHeader(blockchainConfig.constantinopleBlockNumber + 1)
      override val config: EvmConfig = EvmConfig.ConstantinopleConfigBuilder(blockchainConfig)
    }
    case object EIP2200 extends EipToCheck {
      override val blockHeader: BlockHeader = prepareBlockHeader(blockchainConfig.phoenixBlockNumber + 1)
      override val config: EvmConfig = EvmConfig.PhoenixConfigBuilder(blockchainConfig)
    }
  }
}
