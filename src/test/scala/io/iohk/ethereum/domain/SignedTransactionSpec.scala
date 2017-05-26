package io.iohk.ethereum.domain

import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.domain.SignedTransaction.FirstByteOfAddress
import io.iohk.ethereum.vm.Generators
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.crypto.params.ECPublicKeyParameters

class SignedTransactionSpec extends FlatSpec with Matchers with PropertyChecks {
  "SignedTransaction" should "correctly set pointSign for chainId with chain specific signing schema" in {
    forAll(Generators.transactionGen(), Arbitrary.arbitrary[Unit].map(_ => generateKeyPair())) {
      (tx, key) =>
        val chainId: Byte = 0x3d
        val allowedPointSigns = Set((chainId * 2 + 35).toByte, (chainId * 2 + 36).toByte)
        //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of spongycastle encoding
        val address = Address(crypto.kec256(key.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail).drop(FirstByteOfAddress))
        val result = SignedTransaction.sign(tx, key, Some(chainId))

        allowedPointSigns should contain(result.signature.v)
        address shouldEqual result.senderAddress
    }
  }
}
