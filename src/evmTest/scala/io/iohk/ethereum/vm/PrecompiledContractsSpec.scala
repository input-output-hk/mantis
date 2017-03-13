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

    val expectedOutput = kec256(ripemd160(stx.recoveredSenderAddress.get.bytes))

    new EvmTestEnv {
      val (_, contract) = deployContract("PrecompiledContracts")
      //FIXME: stx should use BigInts?
      val result = contract.usePrecompiledContracts(stx.bytesToSign, stx.pointSign, BigInt(stx.signatureRandom.toArray), BigInt(stx.signature.toArray)).call()

      result.returnData shouldEqual expectedOutput
    }
  }
}
