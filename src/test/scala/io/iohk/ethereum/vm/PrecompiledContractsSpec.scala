package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.Address
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import MockWorldState._
import GasFee.wordsForBytes
import io.iohk.ethereum.utils.ByteUtils

class PrecompiledContractsSpec extends FunSuite with Matchers with PropertyChecks {

  def buildContext(recipient: Address, inputData: ByteString, gas: UInt256 = 1000000): PC = {
    val origin = Address(0xcafebabe)
    val env = ExecEnv(recipient, origin, origin, 1000, inputData, 0, Program(ByteString.empty), null, 0)
    ProgramContext(env, recipient, gas, MockWorldState())
  }

  test("ECDSARECOVER") {
    val keyPair = generateKeyPair()
    val bytesGen = Generators.getByteStringGen(32, 32)

    forAll(bytesGen) { bytes =>
      val validSig = ECDSASignature.sign(bytes.toArray, keyPair)
      val recoveredPub = ByteUtils.padLeft(kec256(validSig.recoverPubBytes(bytes).get).slice(12, 32), 32, 0)
      val context = buildContext(PrecompiledContracts.EcDsaRecAddr, bytes)
      val result = VM.run(context)
      result.returnData shouldEqual recoveredPub

      val gasUsed = context.startGas - result.gasRemaining
      gasUsed shouldEqual 3000

      // wrong message - recovery failed
      val wrongMessage = bytes
      val contextWithWrongData = buildContext(PrecompiledContracts.EcDsaRecAddr, wrongMessage)
      val resultFailedRecovery = VM.run(contextWithWrongData)
      resultFailedRecovery.returnData shouldEqual ByteString.empty

      val gasUsedFailedRecover = contextWithWrongData.startGas - resultFailedRecovery.gasRemaining
      gasUsedFailedRecover shouldEqual 3000
    }
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
      result.returnData shouldEqual ripemd160(bytes)

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
