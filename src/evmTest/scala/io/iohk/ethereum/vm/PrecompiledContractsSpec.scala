package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.{Address, SignedTransaction, Transaction}
import io.iohk.ethereum.vm.utils.EvmTestEnv
import org.scalatest.{FunSuite, Matchers}

class PrecompiledContractsSpec extends FunSuite with Matchers {

  test("Precompiled Contracts") {
    val keyPair = generateKeyPair()
    val tx = Transaction(42, 10, 1000, Address(123), 0, ByteString.empty)
    val stx = SignedTransaction(tx, keyPair)

    val addressBytes = stx.recoveredSenderAddress.get.bytes
    val expectedOutput = ripemd160(kec256(addressBytes))

    new EvmTestEnv {
      val (_, contract) = deployContract("PrecompiledContracts")
      //FIXME: stx should use BigInts?
      val result = contract.usePrecompiledContracts(stx.bytesToSign, stx.recoveredPointSign.get,
        BigInt(stx.signatureRandom.toArray), BigInt(stx.signature.toArray)).call()

      // even though contract specifies bytes20 as the return type, 32-byte (right zero padded) value is returned
      result.returnData.take(20) shouldEqual expectedOutput
    }
  }
}
