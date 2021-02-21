package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.{Account, Address, UInt256}
import io.iohk.ethereum.Fixtures.{Blocks => BlockFixtures}
import MockWorldState._
import akka.util.ByteString
import Fixtures.blockchainConfig
import io.iohk.ethereum.crypto.kec256
import org.bouncycastle.util.encoders.Hex
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

// scalastyle:off method.length
class CreateOpcodeSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  val config = EvmConfig.ByzantiumConfigBuilder(blockchainConfig)

  import config.feeSchedule._

  // scalastyle:off
  object fxt {
    val fakeHeader = BlockFixtures.ValidBlock.header.copy(number = blockchainConfig.constantinopleBlockNumber - 1)
    val addresWithRevert = Address(10)
    val creatorAddr = Address(0xcafe)
    val salt = UInt256.Zero

    // doubles the value passed in the input data
    val contractCode = Assembly(
      PUSH1,
      0,
      CALLDATALOAD,
      DUP1,
      ADD,
      PUSH1,
      0,
      MSTORE,
      PUSH1,
      32,
      PUSH1,
      0,
      RETURN
    )

    def initPart(contractCodeSize: Int): Assembly = Assembly(
      PUSH1,
      42,
      PUSH1,
      0,
      SSTORE, //store an arbitrary value
      PUSH1,
      contractCodeSize,
      DUP1,
      PUSH1,
      16,
      PUSH1,
      0,
      CODECOPY,
      PUSH1,
      0,
      RETURN
    )

    val initWithSelfDestruct = Assembly(
      PUSH1,
      creatorAddr.toUInt256.toInt,
      SELFDESTRUCT
    )

    val gas1000 = ByteString(3, -24)

    val initWithSelfDestructAndCall = Assembly(
      PUSH1,
      1,
      PUSH1,
      0,
      PUSH1,
      0,
      PUSH1,
      0,
      PUSH1,
      0,
      PUSH20,
      addresWithRevert.bytes,
      PUSH2,
      gas1000,
      CALL,
      PUSH1,
      creatorAddr.toUInt256.toInt,
      SELFDESTRUCT
    )

    val initWithSstoreWithClear = Assembly(
      //Save a value to the storage
      PUSH1,
      10,
      PUSH1,
      0,
      SSTORE,
      //Clear the store
      PUSH1,
      0,
      PUSH1,
      0,
      SSTORE
    )

    val revertValue = 21
    val initWithRevertProgram = Assembly(
      PUSH1,
      revertValue,
      PUSH1,
      0,
      MSTORE,
      PUSH1,
      1,
      PUSH1,
      31,
      REVERT
    )

    val revertProgram = Assembly(
      PUSH1,
      revertValue,
      PUSH1,
      0,
      MSTORE,
      PUSH1,
      1,
      PUSH1,
      31,
      REVERT
    )

    val accountWithCode: ByteString => Account = code => Account.empty().withCode(kec256(code))

    val endowment: UInt256 = 123
    val initWorld = MockWorldState().saveAccount(creatorAddr, Account.empty().increaseBalance(endowment))
    val newAddr = initWorld.increaseNonce(creatorAddr).createAddress(creatorAddr)

    val worldWithRevertProgram = initWorld
      .saveAccount(addresWithRevert, accountWithCode(revertProgram.code))
      .saveCode(addresWithRevert, revertProgram.code)

    val createCode = Assembly(initPart(contractCode.code.size).byteCode ++ contractCode.byteCode: _*)

    val copyCodeGas = G_copy * wordsForBytes(contractCode.code.size) + config.calcMemCost(0, 0, contractCode.code.size)
    val storeGas = G_sset
    def gasRequiredForInit(withHashCost: Boolean) = initPart(contractCode.code.size).linearConstGas(
      config
    ) + copyCodeGas + storeGas + (if (withHashCost) G_sha3word * wordsForBytes(contractCode.code.size) else 0)
    val depositGas = config.calcCodeDepositCost(contractCode.code)
    def gasRequiredForCreation(withHashCost: Boolean) = gasRequiredForInit(withHashCost) + depositGas + G_create

    val context: PC = ProgramContext(
      callerAddr = Address(0),
      originAddr = Address(0),
      recipientAddr = Some(creatorAddr),
      gasPrice = 1,
      startGas = 2 * gasRequiredForCreation(false),
      inputData = ByteString.empty,
      value = 0,
      endowment = 0,
      doTransfer = true,
      blockHeader = fakeHeader,
      callDepth = 0,
      world = initWorld,
      initialAddressesToDelete = Set(),
      evmConfig = config,
      originalWorld = initWorld
    )
  }

  case class CreateResult(
      context: PC = fxt.context,
      value: UInt256 = fxt.endowment,
      createCode: ByteString = fxt.createCode.code,
      opcode: CreateOp,
      salt: UInt256 = UInt256.Zero,
      ownerAddress: Address = fxt.creatorAddr
  ) {
    val vm = new TestVM
    val env = ExecEnv(context, ByteString.empty, ownerAddress)

    val mem = Memory.empty.store(0, createCode)
    val stack = opcode match {
      case CREATE => Stack.empty().push(Seq[UInt256](createCode.size, 0, value))
      case CREATE2 => Stack.empty().push(Seq[UInt256](salt, createCode.size, 0, value))
    }
    val stateIn: PS = ProgramState(vm, context, env).withStack(stack).withMemory(mem)
    val stateOut: PS = opcode.execute(stateIn)

    val world = stateOut.world
    val returnValue = stateOut.stack.pop._1
  }

  def commonBehaviour(opcode: CreateOp): Unit = {
    def newAccountAddress(code: ByteString = fxt.createCode.code) = opcode match {
      case CREATE => fxt.initWorld.increaseNonce(fxt.creatorAddr).createAddress(fxt.creatorAddr)
      case CREATE2 => fxt.initWorld.create2Address(fxt.creatorAddr, fxt.salt, code)
    }

    val withHashCost = opcode match {
      case CREATE => false
      case CREATE2 => true
    }

    "initialization code executes normally" should {
      val result = CreateResult(opcode = opcode)

      "create a new contract" in {
        val newAccount = result.world.getGuaranteedAccount(newAccountAddress())

        newAccount.balance shouldEqual fxt.endowment
        result.world.getCode(newAccountAddress()) shouldEqual fxt.contractCode.code
        result.world.getStorage(newAccountAddress()).load(0) shouldEqual BigInt(42)
      }

      "update sender (creator) account" in {
        val initialCreator = result.context.world.getGuaranteedAccount(fxt.creatorAddr)
        val updatedCreator = result.world.getGuaranteedAccount(fxt.creatorAddr)

        updatedCreator.balance shouldEqual initialCreator.balance - fxt.endowment
        updatedCreator.nonce shouldEqual initialCreator.nonce + 1
      }

      "return the new contract's address" in {
        Address(result.returnValue) shouldEqual newAccountAddress()
      }

      "consume correct gas" in {
        result.stateOut.gasUsed shouldEqual fxt.gasRequiredForCreation(withHashCost)
      }

      "step forward" in {
        result.stateOut.pc shouldEqual result.stateIn.pc + 1
      }

      "leave return buffer empty" in {
        result.stateOut.returnData shouldEqual ByteString.empty
      }
    }

    "initialization code fails" should {
      val context: PC = fxt.context.copy(startGas = G_create + fxt.gasRequiredForInit(withHashCost) / 2)
      val result = CreateResult(context = context, opcode = opcode)

      "not modify world state except for the creator's nonce" in {
        val creatorsAccount = context.world.getGuaranteedAccount(fxt.creatorAddr)
        val expectedWorld =
          context.world.saveAccount(fxt.creatorAddr, creatorsAccount.copy(nonce = creatorsAccount.nonce + 1))
        result.world shouldEqual expectedWorld
      }

      "return 0" in {
        result.returnValue shouldEqual 0
      }

      "consume correct gas" in {
        val expectedGas = G_create + config.gasCap(context.startGas - G_create)
        result.stateOut.gasUsed shouldEqual expectedGas
      }

      "step forward" in {
        result.stateOut.pc shouldEqual result.stateIn.pc + 1
      }

      "leave return buffer empty" in {
        result.stateOut.returnData shouldEqual ByteString.empty
      }
    }

    "initialization code runs normally but there's not enough gas to deposit code" should {
      val depositGas = fxt.depositGas * 101 / 100
      val availableGasDepth0 = fxt.gasRequiredForInit(withHashCost) + depositGas
      val availableGasDepth1 = config.gasCap(availableGasDepth0)
      val gasUsedInInit = fxt.gasRequiredForInit(withHashCost) + fxt.depositGas

      require(
        gasUsedInInit < availableGasDepth0 && gasUsedInInit > availableGasDepth1,
        "Regression: capped startGas in the VM at depth 1, should be used a base for code deposit gas check"
      )

      val context: PC = fxt.context.copy(startGas = G_create + fxt.gasRequiredForInit(withHashCost) + depositGas)
      val result = CreateResult(context = context, opcode = opcode)

      "consume all gas passed to the init code" in {
        val expectedGas = G_create + config.gasCap(context.startGas - G_create)
        result.stateOut.gasUsed shouldEqual expectedGas
      }

      "not modify world state except for the creator's nonce" in {
        val creatorsAccount = context.world.getGuaranteedAccount(fxt.creatorAddr)
        val expectedWorld =
          context.world.saveAccount(fxt.creatorAddr, creatorsAccount.copy(nonce = creatorsAccount.nonce + 1))
        result.world shouldEqual expectedWorld
      }

      "return 0" in {
        result.returnValue shouldEqual 0
      }
    }

    "call depth limit is reached" should {
      val context: PC = fxt.context.copy(callDepth = EvmConfig.MaxCallDepth)
      val result = CreateResult(context = context, opcode = opcode)

      "not modify world state" in {
        result.world shouldEqual context.world
      }

      "return 0" in {
        result.returnValue shouldEqual 0
      }

      "consume correct gas" in {
        result.stateOut.gasUsed shouldEqual G_create + (if (withHashCost)
                                                          G_sha3word * wordsForBytes(fxt.contractCode.code.size)
                                                        else 0)
      }
    }

    "endowment value is greater than balance" should {
      val result = CreateResult(value = fxt.endowment * 2, opcode = opcode)

      "not modify world state" in {
        result.world shouldEqual result.context.world
      }

      "return 0" in {
        result.returnValue shouldEqual 0
      }

      "consume correct gas" in {
        result.stateOut.gasUsed shouldEqual G_create + (if (withHashCost)
                                                          G_sha3word * wordsForBytes(fxt.contractCode.code.size)
                                                        else 0)
      }
    }

    "initialization includes SELFDESTRUCT opcode" should {
      val gasRequiredForInit = fxt.initWithSelfDestruct.linearConstGas(config) + G_newaccount
      val gasRequiredForCreation = gasRequiredForInit + G_create

      val context: PC = fxt.context.copy(startGas = 2 * gasRequiredForCreation)
      val result = CreateResult(context = context, createCode = fxt.initWithSelfDestruct.code, opcode = opcode)

      "refund the correct amount of gas" in {
        result.stateOut.gasRefund shouldBe result.stateOut.config.feeSchedule.R_selfdestruct
      }
    }

    "initialization includes SELFDESTRUCT opcode and CALL with REVERT" should {

      /*
       * SELFDESTRUCT should clear returnData buffer, if not then leftovers in buffer will lead to additional gas cost
       * for code deployment.
       * In this test case, if we will forget to clear buffer `revertValue` from `revertProgram` will be left in buffer
       * and lead to additional 200 gas cost more for CREATE operation.
       *
       * */
      val expectedGas = 61261
      val gasRequiredForInit = fxt.initWithSelfDestruct.linearConstGas(config) + G_newaccount
      val gasRequiredForCreation =
        gasRequiredForInit + G_create + (if (withHashCost) G_sha3word * wordsForBytes(fxt.contractCode.code.size)
                                         else 0)

      val context: PC = fxt.context.copy(startGas = 2 * gasRequiredForCreation, world = fxt.worldWithRevertProgram)
      val result = CreateResult(context = context, createCode = fxt.initWithSelfDestructAndCall.code, opcode = opcode)

      "clear buffer after SELFDESTRUCT" in {
        result.stateOut.gas shouldBe expectedGas
      }
    }

    "initialization includes REVERT opcode" should {
      val gasRequiredForInit = fxt.initWithRevertProgram.linearConstGas(config) + G_newaccount
      val gasRequiredForCreation = gasRequiredForInit + G_create

      val context: PC = fxt.context.copy(startGas = 2 * gasRequiredForCreation)
      val result = CreateResult(context = context, createCode = fxt.initWithRevertProgram.code, opcode = opcode)

      "return 0" in {
        result.returnValue shouldEqual 0
      }

      "should create an account with empty code" in {
        result.world.getCode(fxt.newAddr) shouldEqual ByteString.empty
      }

      "should fill up data buffer" in {
        result.stateOut.returnData shouldEqual ByteString(fxt.revertValue.toByte)
      }
    }

    "initialization includes a SSTORE opcode that clears the storage" should {

      val codeExecGas = G_sreset + G_sset
      val gasRequiredForInit = fxt.initWithSstoreWithClear.linearConstGas(config) + codeExecGas
      val gasRequiredForCreation = gasRequiredForInit + G_create

      val context: PC = fxt.context.copy(startGas = 2 * gasRequiredForCreation)
      val call = CreateResult(context = context, createCode = fxt.initWithSstoreWithClear.code, opcode = opcode)

      "refund the correct amount of gas" in {
        call.stateOut.gasRefund shouldBe call.stateOut.config.feeSchedule.R_sclear
      }

    }

    "maxCodeSize check is enabled" should {
      val maxCodeSize = 30
      val ethConfig = EvmConfig.PostEIP160ConfigBuilder(blockchainConfig.copy(maxCodeSize = Some(maxCodeSize)))

      val context: PC = fxt.context.copy(startGas = Int.MaxValue, evmConfig = ethConfig)

      val gasConsumedIfError =
        G_create + config.gasCap(context.startGas - G_create) //Gas consumed by CREATE opcode if an error happens

      "result in an out of gas if the code is larger than the limit" in {
        val codeSize = maxCodeSize + 1
        val largeContractCode = Assembly((0 until codeSize).map(_ => Assembly.OpCodeAsByteCode(STOP)): _*)
        val createCode =
          Assembly(fxt.initPart(largeContractCode.code.size).byteCode ++ largeContractCode.byteCode: _*).code
        val call = CreateResult(context = context, createCode = createCode, opcode = opcode)

        call.stateOut.error shouldBe None
        call.stateOut.gasUsed shouldBe gasConsumedIfError
      }

      "not result in an out of gas if the code is smaller than the limit" in {
        val codeSize = maxCodeSize - 1
        val largeContractCode = Assembly((0 until codeSize).map(_ => Assembly.OpCodeAsByteCode(STOP)): _*)
        val createCode =
          Assembly(fxt.initPart(largeContractCode.code.size).byteCode ++ largeContractCode.byteCode: _*).code
        val call = CreateResult(context = context, createCode = createCode, opcode = opcode)

        call.stateOut.error shouldBe None
        call.stateOut.gasUsed shouldNot be(gasConsumedIfError)
      }

    }

    "account with non-empty code already exists" should {

      "fail to create contract" in {
        val accountNonEmptyCode = Account(codeHash = ByteString("abc"))

        val newAddress = newAccountAddress(accountNonEmptyCode.codeHash)

        val world = fxt.initWorld.saveAccount(newAddress, accountNonEmptyCode)
        val context: PC = fxt.context.copy(world = world)
        val result =
          CreateResult(context = context, opcode = opcode, salt = fxt.salt, createCode = accountNonEmptyCode.codeHash)

        result.returnValue shouldEqual UInt256.Zero
        result.world.getGuaranteedAccount(newAddress) shouldEqual accountNonEmptyCode
        result.world.getCode(newAddress) shouldEqual ByteString.empty
      }
    }

    "account with non-zero nonce already exists" should {

      "fail to create contract" in {
        val accountNonZeroNonce = Account(nonce = 1)

        val newAddress = newAccountAddress(accountNonZeroNonce.codeHash)

        val world = fxt.initWorld.saveAccount(newAddress, accountNonZeroNonce)
        val context: PC = fxt.context.copy(world = world)
        val result =
          CreateResult(context = context, opcode = opcode, salt = fxt.salt, createCode = accountNonZeroNonce.codeHash)

        result.returnValue shouldEqual UInt256.Zero
        result.world.getGuaranteedAccount(newAddress) shouldEqual accountNonZeroNonce
        result.world.getCode(newAddress) shouldEqual ByteString.empty
      }
    }

  }

  "CREATE" should {
    behave like commonBehaviour(CREATE)

    "account with non-zero balance, but empty code and zero nonce, already exists" should {

      "succeed in creating new contract" in {
        val accountNonZeroBalance = Account(balance = 1)

        val world = fxt.initWorld.saveAccount(fxt.newAddr, accountNonZeroBalance)
        val context: PC = fxt.context.copy(world = world)
        val result = CreateResult(context = context, opcode = CREATE)

        result.returnValue shouldEqual fxt.newAddr.toUInt256

        val newContract = result.world.getGuaranteedAccount(fxt.newAddr)
        newContract.balance shouldEqual (accountNonZeroBalance.balance + fxt.endowment)
        newContract.nonce shouldEqual accountNonZeroBalance.nonce

        result.world.getCode(fxt.newAddr) shouldEqual fxt.contractCode.code
      }
    }
  }

  "CREATE2" should {
    behave like commonBehaviour(CREATE2)

    "returns correct address and spends correct amount of gas (examples from https://eips.ethereum.org/EIPS/eip-1014)" in {

      val testTable = Table[String, String, String, BigInt, String](
        ("address", "salt", "init_code", "gas", "result"),
        (
          "0x0000000000000000000000000000000000000000",
          "0000000000000000000000000000000000000000000000000000000000000000",
          "00",
          32006,
          "0x4D1A2e2bB4F88F0250f26Ffff098B0b30B26BF38"
        ),
        (
          "0xdeadbeef00000000000000000000000000000000",
          "0000000000000000000000000000000000000000000000000000000000000000",
          "00",
          32006,
          "0xB928f69Bb1D91Cd65274e3c79d8986362984fDA3"
        ),
        (
          "0xdeadbeef00000000000000000000000000000000",
          "000000000000000000000000feed000000000000000000000000000000000000",
          "00",
          32006,
          "0xD04116cDd17beBE565EB2422F2497E06cC1C9833"
        ),
        (
          "0x0000000000000000000000000000000000000000",
          "0000000000000000000000000000000000000000000000000000000000000000",
          "",
          32000,
          "0xE33C0C7F7df4809055C3ebA6c09CFe4BaF1BD9e0"
        )
      )

      forAll(testTable) { (address, salt, init_code, gas, resultAddress) =>
        val add = Address(address)
        val world = fxt.initWorld.saveAccount(add, Account(balance = 100000))

        val result = CreateResult(
          context = fxt.context.copy(callerAddr = add, world = world),
          opcode = CREATE2,
          createCode = ByteString(Hex.decode(init_code)),
          ownerAddress = add,
          salt = UInt256(ByteString(Hex.decode(salt)))
        )

        Address(result.returnValue) shouldBe Address(resultAddress)
        result.stateOut.gasUsed shouldBe gas
      }
    }
  }

}
