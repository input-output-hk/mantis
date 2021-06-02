package io.iohk.ethereum.domain

import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.domain.SignedTransaction.FirstByteOfAddress
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.vm.Generators
import org.scalacheck.Arbitrary
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SignedTransactionSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks with SecureRandomBuilder {
  "SignedTransaction" should "correctly set pointSign for chainId with chain specific signing schema" in {
    forAll(Generators.transactionGen(), Arbitrary.arbitrary[Unit].map(_ => generateKeyPair(secureRandom))) {
      (tx, key) =>
        val chainId: BigInt = 0x3d
        val allowedPointSigns = Set(chainId * 2 + 35, chainId * 2 + 36)
        //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of bouncycastle encoding
        val address = Address(
          crypto
            .kec256(key.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail)
            .drop(FirstByteOfAddress)
        )
        val result = SignedTransaction.sign(tx, key, Some(chainId))

        allowedPointSigns should contain(result.tx.signature.v)
        address shouldEqual result.senderAddress
    }
  }
}
