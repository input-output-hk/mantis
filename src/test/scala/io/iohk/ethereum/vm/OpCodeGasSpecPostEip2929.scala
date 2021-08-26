package io.iohk.ethereum.vm

import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}

import org.bouncycastle.util.encoders.Hex
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.Fixtures.Blocks
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.domain.UInt256._
import io.iohk.ethereum.vm.Generators._
import io.iohk.ethereum.vm.MockWorldState.PC
import io.iohk.ethereum.vm.MockWorldState.TestVM

import Fixtures.blockchainConfig

class OpCodeGasSpecPostMagneto extends OpCodeGasSpecPostEip2929 {
  override val config: EvmConfig = EvmConfig.MagnetoConfigBuilder(blockchainConfig)
  override val forkBlockHeight = Fixtures.MagnetoBlockNumber
}

class OpCodeGasSpecPostBerlin extends OpCodeGasSpecPostEip2929 {
  override val config: EvmConfig = EvmConfig.BerlinConfigBuilder(blockchainConfig)
  override val forkBlockHeight = Fixtures.BerlinBlockNumber
}

trait OpCodeGasSpecPostEip2929 extends AnyFunSuite with OpCodeTesting with Matchers with ScalaCheckPropertyChecks {

  protected[this] def forkBlockHeight: Int

  import config.feeSchedule._

  test(EXTCODESIZE, EXTCODEHASH, BALANCE) { op =>
    val stateGen = getProgramStateGen(
      evmConfig = config,
      stackGen = getStackGen(elems = 1),
      blockNumberGen = getUInt256Gen(forkBlockHeight)
    )
    val codeGen = getByteStringGen(0, 512)

    forAll(stateGen) { stateIn =>
      val (addrUint, _) = stateIn.stack.pop
      val addr = Address(addrUint)
      stateIn.accessedAddresses shouldNot contain(addr)

      val stateOut = op.execute(stateIn)

      verifyGas(G_cold_account_access, stateIn, stateOut)
      stateOut.accessedAddresses should contain(addr)
    }

    forAll(stateGen, codeGen) { (stateIn, extCode) =>
      val (addrUint, _) = stateIn.stack.pop
      val addr = Address(addrUint)
      val program = Program(extCode)
      val world1 = stateIn.world.saveCode(addr, program.code)
      val stateInWithExtCode = stateIn.withWorld(world1).addAccessedAddress(addr)

      val stateOut = op.execute(stateInWithExtCode)

      verifyGas(G_warm_storage_read, stateIn, stateOut)
      stateOut.accessedAddresses should contain(addr)
    }
  }

  test(EXTCODECOPY) { op =>
    val table = Table[UInt256, Boolean, BigInt](
      ("size", "accessed", "expectedGas"),
      (0, false, G_cold_account_access),
      (0, true, G_warm_storage_read),
      (1, false, G_cold_account_access + G_copy * 1),
      (1, true, G_warm_storage_read + G_copy * 1),
      (32, false, G_cold_account_access + G_copy * 1),
      (32, true, G_warm_storage_read + G_copy * 1),
      (33, false, G_cold_account_access + G_copy * 2),
      (33, true, G_warm_storage_read + G_copy * 2),
      (Two ** 16, false, G_cold_account_access + G_copy * 2048),
      (Two ** 16, true, G_warm_storage_read + G_copy * 2048),
      (Two ** 16 + 1, false, G_cold_account_access + G_copy * 2049),
      (Two ** 16 + 1, true, G_warm_storage_read + G_copy * 2049)
    )

    forAll(table) { (size, accessed, expectedGas) =>
      val initState = getProgramStateGen(
        evmConfig = config,
        blockNumberGen = getUInt256Gen(forkBlockHeight)
      ).sample.get
      // Pick an address (small, so it fits into memory) that is not on the precompiles list
      val addr =
        getUInt256Gen(max = 1000).map(Address(_)).retryUntil(!initState.accessedAddresses.contains(_)).sample.get
      val stackIn = Stack.empty().push(Seq(size, Zero, Zero, addr.toUInt256))
      val memIn = Memory.empty.store(addr.toUInt256, Array.fill[Byte](size.toInt)(-1))
      val stateIn = initState.withStack(stackIn).withMemory(memIn).copy(gas = expectedGas)

      val stateOut = if (accessed) op.execute(stateIn.addAccessedAddress(addr)) else op.execute(stateIn)

      verifyGas(expectedGas, stateIn, stateOut, allowOOG = false)
      stateOut.accessedAddresses should contain(addr)
    }

    val maxGas = 2 * (G_cold_account_access + G_copy * 8)
    val stateGen = getProgramStateGen(
      evmConfig = config,
      blockNumberGen = getUInt256Gen(forkBlockHeight),
      stackGen = getStackGen(elems = 4, maxUInt = UInt256(256)),
      gasGen = getBigIntGen(max = maxGas),
      memGen = getMemoryGen(256)
    )

    forAll(stateGen) { stateIn =>
      val stateOut = op.execute(stateIn)

      val (Seq(address, offset, _, size), _) = stateIn.stack.pop(4)
      val addr = Address(address)
      val memCost = config.calcMemCost(stateIn.memory.size, offset, size)
      val copyCost = G_copy * wordsForBytes(size)
      val expectedGas =
        if (stateIn.accessedAddresses.contains(addr))
          G_warm_storage_read + memCost + copyCost
        else G_cold_account_access + memCost + copyCost

      verifyGas(expectedGas, stateIn, stateOut)
    }
  }

  test(SLOAD) { op =>
    val stateGen = getProgramStateGen(
      evmConfig = config,
      stackGen = getStackGen(elems = 1),
      blockNumberGen = getUInt256Gen(forkBlockHeight)
    )

    forAll(stateGen) { stateIn =>
      stateIn.accessedStorageKeys shouldBe empty
      val (offset, _) = stateIn.stack.pop

      val stateOut = op.execute(stateIn)

      verifyGas(G_cold_sload, stateIn, stateOut)
      assert(stateOut.accessedStorageKeys.contains((stateIn.ownAddress, offset)))
    }

    forAll(stateGen) { stateIn =>
      val (offset, _) = stateIn.stack.pop

      val stateOut = op.execute(stateIn.addAccessedStorageKey(stateIn.ownAddress, offset))

      verifyGas(G_warm_storage_read, stateIn, stateOut)
      assert(stateOut.accessedStorageKeys.contains((stateIn.ownAddress, offset)))
    }
  }

  test(SELFDESTRUCT) { op =>
    val stateGen = getProgramStateGen(
      evmConfig = config,
      stackGen = getStackGen(elems = 1),
      blockNumberGen = getUInt256Gen(forkBlockHeight)
    )

    val addressAlreadyAccessedGen = Arbitrary.arbitrary[Boolean]

    // Sending refund to a non-existent account
    forAll(stateGen, addressAlreadyAccessedGen) { (stateIn, addressAlreadyAccessed) =>
      val (refund, _) = stateIn.stack.pop
      val refundAddress = Address(refund)
      whenever(stateIn.world.getAccount(refundAddress).isEmpty && stateIn.ownBalance > 0) {
        val stateOut =
          if (addressAlreadyAccessed) op.execute(stateIn.addAccessedAddress(refundAddress)) else op.execute(stateIn)
        stateOut.gasRefund shouldEqual R_selfdestruct
        if (addressAlreadyAccessed)
          verifyGas(G_selfdestruct + G_newaccount, stateIn, stateOut)
        else
          verifyGas(G_selfdestruct + G_newaccount + G_cold_account_access, stateIn, stateOut)
      }
    }

    // Sending refund to an already existing account not dead account
    forAll(stateGen, addressAlreadyAccessedGen) { (stateIn, addressAlreadyAccessed) =>
      val (refund, _) = stateIn.stack.pop
      val refundAddress = Address(refund)
      val world = stateIn.world.saveAccount(refundAddress, Account.empty().increaseNonce())
      val updatedStateIn = stateIn.withWorld(world)
      val stateOut =
        if (addressAlreadyAccessed) op.execute(updatedStateIn.addAccessedAddress(refundAddress))
        else op.execute(updatedStateIn)
      if (addressAlreadyAccessed)
        verifyGas(G_selfdestruct, updatedStateIn, stateOut)
      else
        verifyGas(G_selfdestruct + G_cold_account_access, updatedStateIn, stateOut)
      stateOut.gasRefund shouldEqual R_selfdestruct
    }

    // Owner account was already selfdestructed
    forAll(stateGen, addressAlreadyAccessedGen) { (stateIn, addressAlreadyAccessed) =>
      val (refund, _) = stateIn.stack.pop
      val refundAddress = Address(refund)
      whenever(stateIn.world.getAccount(refundAddress).isEmpty && stateIn.ownBalance > 0) {
        val updatedStateIn = stateIn.withAddressToDelete(stateIn.env.ownerAddr)
        val stateOut =
          if (addressAlreadyAccessed) op.execute(updatedStateIn.addAccessedAddress(refundAddress))
          else op.execute(updatedStateIn)
        if (addressAlreadyAccessed)
          verifyGas(G_selfdestruct + G_newaccount, updatedStateIn, stateOut)
        else
          verifyGas(G_selfdestruct + G_newaccount + G_cold_account_access, updatedStateIn, stateOut)
        stateOut.gasRefund shouldEqual 0
      }
    }
  }

  test(SSTORE) { op =>
    val storage = MockStorage.Empty.store(Zero, One)
    val table = Table[UInt256, UInt256, Boolean, BigInt, BigInt](
      ("offset", "value", "alreadyAccessed", "startGas", "expectedGasConsumption"),
      (0, 1, true, G_callstipend + 1, G_sload),
      (0, 1, false, G_callstipend + 1, G_sload + G_cold_sload),
      (0, 0, true, G_callstipend + 1, G_sload),
      (0, 0, false, G_callstipend + 1, G_sload + G_cold_sload),
      (1, 0, true, G_callstipend + 1, G_sload),
      (1, 0, false, G_callstipend + 1, G_sload + G_cold_sload),
      (1, 1, true, G_sset, G_sset),
      (1, 1, false, G_sset + G_cold_sload, G_sset + G_cold_sload)
    )

    forAll(table) { (offset, value, alreadyAccessed, startGas, expectedGasConsumption) =>
      val stackIn = Stack.empty().push(value).push(offset)
      val stateIn = getProgramStateGen(
        blockNumberGen = getUInt256Gen(forkBlockHeight),
        evmConfig = config
      ).sample.get.withStack(stackIn).withStorage(storage).copy(gas = startGas)

      val stateOut =
        if (alreadyAccessed) op.execute(stateIn.addAccessedStorageKey(stateIn.ownAddress, offset))
        else op.execute(stateIn)
      verifyGas(expectedGasConsumption, stateIn, stateOut, allowOOG = false)
    }

    // test G_sreset
    forAll(Arbitrary.arbitrary[Boolean]) { alreadyAccessed =>
      val offset = 0
      val value = 0
      val expectedGasConsumption = if (alreadyAccessed) G_sreset else G_sreset + G_cold_sload

      val stackIn = Stack.empty().push(value).push(offset)
      val stateIn = getProgramStateGen(
        blockNumberGen = getUInt256Gen(forkBlockHeight),
        evmConfig = config,
        storageGen = Gen.const(storage)
      ).sample.get.withStack(stackIn).copy(gas = expectedGasConsumption)

      val stateOut =
        if (alreadyAccessed) op.execute(stateIn.addAccessedStorageKey(stateIn.ownAddress, offset))
        else op.execute(stateIn)
      verifyGas(expectedGasConsumption, stateIn, stateOut, allowOOG = false)
    }
  }

  // Testcases described in https://gist.github.com/holiman/174548cad102096858583c6fbbb0649a
  test("Gas metering after Magneto/Berlin hard fork (EIP-2929)") {
    val eip2929Table = Table[String, BigInt](
      ("code", "gasUsed"),
      ("60013f5060023b506003315060f13f5060f23b5060f3315060f23f5060f33b5060f1315032315030315000", 8653),
      ("60006000600060ff3c60006000600060ff3c600060006000303c00", 2835),
      ("60015450601160015560116002556011600255600254600154", 44529),
      ("60008080808060046000f15060008080808060ff6000f15060008080808060ff6000fa50", 2869)
    )

    forAll(eip2929Table) { (code, gasUsed) =>
      val defaultGaspool = 1000000
      val senderAddr: Address = Address(0xcafebabeL)
      val senderAcc: Account = Account(nonce = 1, balance = 1000000)
      val defaultWorld: MockWorldState = MockWorldState().saveAccount(senderAddr, senderAcc)

      val blockHeader = Blocks.ValidBlock.header.copy(
        number = forkBlockHeight
      )

      val vm = new TestVM
      val context: PC = ProgramContext(
        callerAddr = senderAddr,
        originAddr = senderAddr,
        recipientAddr = None,
        gasPrice = 1,
        startGas = defaultGaspool,
        inputData = bEmpty,
        value = 100,
        endowment = 100,
        doTransfer = true,
        blockHeader = blockHeader,
        callDepth = 0,
        world = defaultWorld,
        initialAddressesToDelete = Set(),
        evmConfig = config,
        originalWorld = defaultWorld
      )

      val env = ExecEnv(context, ByteString(Hex.decode(code)), context.originAddr)
      val result = vm.exec(ProgramState(vm, context, env))

      result.gasUsed shouldEqual gasUsed
    }
  }
}
