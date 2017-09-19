package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.{Address, UInt256}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import MockWorldState._
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.utils.ByteUtils
import org.spongycastle.util.encoders.Hex

class PrecompiledContractsSpec extends FunSuite with Matchers with PropertyChecks with SecureRandomBuilder {

  def buildContext(recipient: Address, inputData: ByteString, gas: UInt256 = 1000000): PC = {
    val origin = Address(0xcafebabe)
    val env = ExecEnv(recipient, origin, origin, 1000, inputData, 0, Program(ByteString.empty), null, 0)
    ProgramContext(env, recipient, gas, MockWorldState(), EvmConfig.PostEIP160Config)
  }

  test("ECDSARECOVER") {
    val keyPair = generateKeyPair(secureRandom)
    val bytesGen = Generators.getByteStringGen(1, 128)

    forAll(bytesGen) { bytes =>
      val hash = kec256(bytes)
      val validSig = ECDSASignature.sign(hash.toArray, keyPair)
      val recoveredPub = ByteUtils.padLeft(kec256(validSig.publicKey(hash).get).slice(12, 32), 32, 0)

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

  test("ECDSARECOVER_Malformed_Recovery_ID_V") {
    val validAddress = ByteString(Hex.decode("000000000000000000000000a94f5374fce5edbc8e2a8697c15331677e6ebf0b"))

    val validH = ByteString(Hex.decode("18c547e4f7b0f325ad1e56f57e26c745b09a3e503d86e00e5255ff7f715d3d1c"))
    val validR = ByteString(Hex.decode("73b1693892219d736caba55bdb67216e485557ea6b6af75f37096c9aa6a5a75f"))
    val validS = ByteString(Hex.decode("eeb940b1d03b21e36b0e47e79769f095fe2ab855bd91e3a38756b7d75a9c4549"))

    val validV =  ByteString(Hex.decode("000000000000000000000000000000000000000000000000000000000000001c"))
    val invalidV= ByteString(Hex.decode("000000000000000000000000000000000000000000000000000000000000f01c"))

    val invalidInput = validH ++ invalidV ++ validR ++ validS
    val validInput = validH ++ validV ++ validR ++ validS

    // Valid Input
    val context = buildContext(PrecompiledContracts.EcDsaRecAddr, validInput)
    val result = VM.run(context)
    result.returnData shouldEqual validAddress

    // InvalidInput
    val invalidContext = buildContext(PrecompiledContracts.EcDsaRecAddr, invalidInput)
    val invalidResult = VM.run(invalidContext)
    invalidResult.returnData shouldEqual  ByteString.empty
  }

  test("SHA256") {
    val bytesGen = Generators.getByteStringGen(0, 256)
    forAll(bytesGen) { bytes =>
      val context = buildContext(PrecompiledContracts.Sha256Addr, bytes)
      val result = VM.run(context)
      result.returnData shouldEqual sha256(bytes)

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
