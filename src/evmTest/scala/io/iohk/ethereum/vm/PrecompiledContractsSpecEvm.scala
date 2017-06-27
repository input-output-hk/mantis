package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.{SecureRandomProvider, crypto}
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.SignedTransaction.{FirstByteOfAddress, LastByteOfAddress}
import io.iohk.ethereum.vm.utils.EvmTestEnv
import org.scalatest.{FunSuite, Matchers}
import org.spongycastle.crypto.params.ECPublicKeyParameters

class PrecompiledContractsSpecEvm extends FunSuite with Matchers with SecureRandomProvider {

  test("Precompiled Contracts") {
    val keyPair = generateKeyPair(secureRandom)
    val bytes: Array[Byte] = ByteString("aabbccdd").toArray[Byte]
    val signature = ECDSASignature.sign(bytes, keyPair)
    val pubKey = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false)
    val address = crypto.kec256(pubKey.tail).slice(FirstByteOfAddress, LastByteOfAddress)
    val expectedOutput = ripemd160(sha256(address))

    new EvmTestEnv {
      val (_, contract) = deployContract("PrecompiledContracts")
      val result = contract.usePrecompiledContracts(bytes, signature.v, signature.r, signature.s).call()

      // even though contract specifies bytes20 as the return type, 32-byte (right zero padded) value is returned
      result.returnData.take(20) shouldEqual expectedOutput
    }
  }
}
