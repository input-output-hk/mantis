package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.Address
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import MockWorldState._
import io.iohk.ethereum.utils.ByteUtils

class PrecompiledContractsSpec extends FunSuite with Matchers with PropertyChecks {

  def buildContext(recipient: Address, inputData: ByteString, gas: UInt256 = 1000000): PC = {
    val origin = Address(0xcafebabe)
    val env = ExecEnv(recipient, origin, origin, 1000, inputData, 0, Program(ByteString.empty), null, 0)
    ProgramContext(env, recipient, gas, MockWorldState(), EvmConfig.PostEIP160Config)
  }

  test("ECDSARECOVER") {
    val keyPair = generateKeyPair()
    val bytesGen = Generators.getByteStringGen(1, 128)

    forAll(bytesGen) { bytes =>
      val hash = kec256(bytes)
      val validSig = ECDSASignature.sign(hash.toArray, keyPair)
      //byte 0 indicates that this is uncompressed point and is not part of the point itself but is part of spongycastle encoding
      val recoveredPub = ByteUtils.padLeft(kec256(validSig.publicKey(hash).get.tail).slice(12, 32), 32, 0)

      val inputData = hash ++ UInt256(validSig.v).bytes ++ UInt256(validSig.r).bytes ++ UInt256(validSig.s).bytes

      val context = buildContext(PrecompiledContracts.EcDsaRecAddr, inputData)
      val result = VM.run(context)
      result.returnData shouldEqual recoveredPub

      val gasUsed = context.startGas - result.gasRemaining
      gasUsed shouldEqual 3000
    }

    // invalid input - recovery failed
    val invalidInput = ByteString(Array.fill[Byte](128)(0))
    val contextWithWrongData = buildContext(PrecompiledContracts.EcDsaRecAddr, invalidInput)
    val resultFailedRecovery = VM.run(contextWithWrongData)
    resultFailedRecovery.returnData shouldEqual ByteString.empty

    val gasUsedFailedRecover = contextWithWrongData.startGas - resultFailedRecovery.gasRemaining
    gasUsedFailedRecover shouldEqual 3000
  }

  test("SHA256") {
    val bytesGen = Generators.getByteStringGen(0, 256)
    forAll(bytesGen) { bytes =>
      val context = buildContext(PrecompiledContracts.Sha256Addr, bytes)
      val result = VM.run(context)
      result.returnData shouldEqual kec256(bytes)

      val gasUsed = context.startGas - result.gasRemaining
      val expectedGas = 60 + 12 * wordsForBytes(bytes.size)
      gasUsed shouldEqual expectedGas
    }
  }

  test("RIPEMD160") {
    val bytesGen = Generators.getByteStringGen(0, 256)
    forAll(bytesGen) { bytes =>
      val context = buildContext(PrecompiledContracts.Rip160Addr, bytes)
      val result = VM.run(context)
      result.returnData shouldEqual ByteUtils.padLeft(ripemd160(bytes), 32)

      val gasUsed = context.startGas - result.gasRemaining
      val expectedGas = 600 + 120 * wordsForBytes(bytes.size)
      gasUsed shouldEqual expectedGas
    }
  }

  test("IDENTITY") {
    val bytesGen = Generators.getByteStringGen(0, 256)
    forAll(bytesGen) { bytes =>
      val context = buildContext(PrecompiledContracts.IdAddr, bytes)
      val result = VM.run(context)
      result.returnData shouldEqual bytes

      val gasUsed = context.startGas - result.gasRemaining
      val expectedGas = 15 + 3 * wordsForBytes(bytes.size)
      gasUsed shouldEqual expectedGas
    }
  }

}
