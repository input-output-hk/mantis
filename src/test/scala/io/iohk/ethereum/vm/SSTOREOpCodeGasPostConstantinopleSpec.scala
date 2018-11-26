package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.{Account, Address, BlockHeader}
import io.iohk.ethereum.vm.Fixtures.blockchainConfig
import io.iohk.ethereum.vm.MockWorldState.{PC, TestVM}
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.crypto.kec256
import org.bouncycastle.util.encoders.Hex

// EIP-1283
// Spec https://eips.ethereum.org/EIPS/eip-1283
class StoreOpCodeGasPostConstantinopleSpec extends WordSpec with PropertyChecks with Matchers with TestSetup {

  val table =  Table[String, BigInt, BigInt, BigInt](
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


  "Net gas metering for SSTORE after Constantinople hard fork (EIP-1283)" in {
    forAll(table) {
      (code, original, gasUsed, refund) => {
        val result = vm.exec(prepareProgramState(ByteString(Hex.decode(code)), original))

        result.gasUsed shouldEqual gasUsed
        result.gasRefund shouldEqual refund
      }
    }
  }

}

trait TestSetup {
  val config = EvmConfig.ConstantinopleConfigBuilder(blockchainConfig)
  val vm = new TestVM

  val senderAddr = Address(0xcafebabeL)
  val senderAcc = Account(nonce = 1, balance = 1000000)

  val accountWithCode: ByteString => Account = code => Account.empty().withCode(kec256(code))

  def defaultWorld: MockWorldState = MockWorldState().saveAccount(senderAddr, senderAcc)

  val blockHeader = BlockHeader(
    parentHash = bEmpty,
    ommersHash = bEmpty,
    beneficiary = bEmpty,
    stateRoot = bEmpty,
    transactionsRoot = bEmpty,
    receiptsRoot = bEmpty,
    logsBloom = bEmpty,
    difficulty = 1000000,
    number = blockchainConfig.constantinopleBlockNumber + 1,
    gasLimit = 10000000,
    gasUsed = 0,
    unixTimestamp = 0,
    extraData = bEmpty,
    mixHash = bEmpty,
    nonce = bEmpty
  )

  def getContext(world: MockWorldState = defaultWorld, inputData: ByteString = bEmpty): PC =
    ProgramContext(
      callerAddr = senderAddr,
      originAddr = senderAddr,
      recipientAddr = None,
      gasPrice = 1,
      startGas = 1000000,
      inputData = inputData,
      value = 100,
      endowment = 100,
      doTransfer = true,
      blockHeader = blockHeader,
      callDepth = 0,
      world = world,
      initialAddressesToDelete = Set(),
      evmConfig = config,
      originalWorld = world
    )

  def prepareProgramState(assemblyCode: ByteString, originalValue: BigInt): ProgramState[MockWorldState, MockStorage] = {
    val newWorld = defaultWorld
      .saveAccount(senderAddr, accountWithCode(assemblyCode))
      .saveCode(senderAddr, assemblyCode)
      .saveStorage(senderAddr, MockStorage(Map(BigInt(0) -> originalValue)))

    val context: PC = getContext(newWorld)
    val env = ExecEnv(context, assemblyCode, context.originAddr)

    ProgramState(vm, context, env)
  }
}
