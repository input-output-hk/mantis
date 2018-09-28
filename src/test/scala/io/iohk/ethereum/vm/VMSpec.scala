package io.iohk.ethereum.vm

import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.vm.MockWorldState._
import org.scalatest.{WordSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class VMSpec extends WordSpec with PropertyChecks with Matchers {

  "VM" when {

    "executing message call" should {

      "only transfer if recipient's account has no code" in new MessageCall {

        val context = getContext()
        val result = vm.run(context)

        result.world.getBalance(recipientAddr.get) shouldEqual context.value
      }

      "execute recipient's contract" in new MessageCall {
        val inputData = UInt256(42).bytes

        // store first 32 bytes of input data as value at offset 0
        val code = Assembly(
          PUSH1, 0,
          CALLDATALOAD,
          PUSH1, 0,
          SSTORE
        ).code

        val world = defaultWorld.saveCode(recipientAddr.get, code)

        val context = getContext(world = world, inputData = inputData)

        val result = vm.run(context)

        result.world.getBalance(recipientAddr.get) shouldEqual context.value
        result.world.getStorage(recipientAddr.get).load(0) shouldEqual 42
      }
    }

    "executing contract creation" should {

      "create new contract" in new ContractCreation {
        val context1 = getContext()
        val result1 = vm.run(context1)

        result1.world.getCode(expectedNewAddress) shouldEqual defaultContractCode
        result1.world.getBalance(expectedNewAddress) shouldEqual context1.value
        result1.world.getStorage(expectedNewAddress).load(storageOffset) shouldEqual storedValue

        val context2 = getContext(Some(expectedNewAddress), result1.world, bEmpty, homesteadConfig)
        val result2 = vm.run(context2)

        result2.world.getStorage(expectedNewAddress).load(storageOffset) shouldEqual secondStoredValue
      }

      "go OOG if new contract's code size exceeds limit" in new ContractCreation {
        val codeSize = evmBlockchainConfig.maxCodeSize.get.toInt + 1
        val contractCode = ByteString(Array.fill(codeSize)(-1.toByte))

        val context = getContext(inputData = initCode(contractCode))
        val result = vm.run(context)

        result.error shouldBe Some(OutOfGas)
      }

      "fail to create contract in case of address conflict (non-empty code)" in new ContractCreation {
        val nonEmptyCodeHash = ByteString(1)
        val world = defaultWorld.saveAccount(expectedNewAddress, Account(codeHash = nonEmptyCodeHash))

        val context = getContext(world = world)
        val result = vm.run(context)

        result.error shouldBe Some(InvalidOpCode(INVALID.code))
      }

      "fail to create contract in case of address conflict (non-zero nonce)" in new ContractCreation {
        val world = defaultWorld.saveAccount(expectedNewAddress, Account(nonce = 1))

        val context = getContext(world = world)
        val result = vm.run(context)

        result.error shouldBe Some(InvalidOpCode(INVALID.code))
      }

      "create contract if the account already has some balance, but zero nonce and empty code" in new ContractCreation {
        val world = defaultWorld.saveAccount(expectedNewAddress, Account(balance = 1))

        val context = getContext(world = world)
        val result = vm.run(context)

        result.error shouldBe None
        result.world.getBalance(expectedNewAddress) shouldEqual context.value + 1
        result.world.getCode(expectedNewAddress) shouldEqual defaultContractCode
      }

      "initialise a new contract account with zero nonce before EIP-161" in new ContractCreation {
        val context = getContext(evmConfig = homesteadConfig)
        val result = vm.run(context)

        result.world.getAccount(expectedNewAddress).map(_.nonce) shouldEqual Some(0)
      }

      "initialise a new contract account with incremented nonce after EIP-161" in new ContractCreation {
        val world = defaultWorld.copy(noEmptyAccountsCond = true)

        val context = getContext(world = world, evmConfig = eip161Config)
        val result = vm.run(context)

        result.world.getAccount(expectedNewAddress).map(_.nonce) shouldEqual Some(1)
      }
    }
  }

  trait TestSetup {
    val vm = new TestVM

    val blockHeader = BlockHeader(
      parentHash = bEmpty,
      ommersHash = bEmpty,
      beneficiary = bEmpty,
      stateRoot = bEmpty,
      transactionsRoot = bEmpty,
      receiptsRoot = bEmpty,
      logsBloom = bEmpty,
      difficulty = 1000000,
      number = 1,
      gasLimit = 10000000,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = bEmpty,
      mixHash = bEmpty,
      nonce = bEmpty
    )

    val evmBlockchainConfig = BlockchainConfigForEvm(
      frontierBlockNumber = Long.MaxValue,
      homesteadBlockNumber = Long.MaxValue,
      eip150BlockNumber = Long.MaxValue,
      eip160BlockNumber = Long.MaxValue,
      eip161BlockNumber = Long.MaxValue,
      danseBlockNumber = Long.MaxValue,
      maxCodeSize = Some(16),
      accountStartNonce = 0
    )

    val homesteadConfig = EvmConfig.forBlock(0, evmBlockchainConfig.copy(homesteadBlockNumber = 0))
    val eip161Config = EvmConfig.forBlock(0, evmBlockchainConfig.copy(eip161BlockNumber = 0))

    val senderAddr = Address(0xcafebabeL)
    val senderAcc = Account(nonce = 1, balance = 1000000)
    def defaultWorld = MockWorldState().saveAccount(senderAddr, senderAcc)

    def getContext(
        recipientAddr: Option[Address],
        world: MockWorldState,
        inputData: ByteString,
        evmConfig: EvmConfig): PC =
      ProgramContext(
        callerAddr = senderAddr,
        originAddr = senderAddr,
        recipientAddr = recipientAddr,
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
        evmConfig = evmConfig
      )

    def recipientAddr: Option[Address]
  }

  trait MessageCall extends TestSetup {
    val recipientAddr = Some(Address(0xdeadbeefL))
    val recipientAcc = Account(nonce = 1)

    override val defaultWorld = super.defaultWorld.saveAccount(recipientAddr.get, recipientAcc)

    def getContext(world: MockWorldState = defaultWorld, inputData: ByteString = bEmpty): PC =
      getContext(recipientAddr, world, inputData, homesteadConfig)
  }

  trait ContractCreation extends TestSetup {
    val recipientAddr = None

    val expectedNewAddress = defaultWorld.createAddress(senderAddr)

    val storedValue = 42
    val secondStoredValue = 13
    val storageOffset = 0

    val defaultContractCode: ByteString =
      Assembly(
        PUSH1, secondStoredValue,
        PUSH1, storageOffset,
        SSTORE
      ).code

    def initCode(contractCode: ByteString = defaultContractCode): ByteString =
      Assembly(
        PUSH1, storedValue,
        PUSH1, storageOffset,
        SSTORE, //store an arbitrary value
        PUSH1, contractCode.size,
        DUP1,
        PUSH1, 16,
        PUSH1, 0,
        CODECOPY,
        PUSH1, 0,
        RETURN
      ).code ++ contractCode

    def getContext(
        world: MockWorldState = defaultWorld,
        inputData: ByteString = initCode(),
        evmConfig: EvmConfig = homesteadConfig): PC =
      getContext(None, world, inputData, evmConfig)
  }

}
