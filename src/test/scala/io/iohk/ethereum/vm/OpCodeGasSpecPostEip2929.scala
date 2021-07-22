package io.iohk.ethereum.vm

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.vm.Generators._

import Fixtures.blockchainConfig

class OpCodeGasSpecPostEip2929 extends AnyFunSuite with OpCodeTesting with Matchers with ScalaCheckPropertyChecks {

  override val config: EvmConfig = EvmConfig.MagnetoConfigBuilder(blockchainConfig)

  import config.feeSchedule._

  test(EXTCODESIZE) { op =>
    val stateGen = getProgramStateGen(
      evmConfig = config,
      stackGen = getStackGen(elems = 1),
      blockNumberGen = getUInt256Gen(Fixtures.MagnetoBlockNumber)
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
}
