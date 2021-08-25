package io.iohk.ethereum.domain

import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.domain.SignedTransaction.FirstByteOfAddress
import io.iohk.ethereum.security.SecureRandomBuilder
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait SignedTransactionBehavior
extends Matchers
with ScalaCheckPropertyChecks
with SecureRandomBuilder
{
  this: AnyFlatSpec =>

  def SignedTransactionBehavior(signedTransactionGenerator: Gen[Transaction], allowedPointSigns: Byte => Set[Byte]): Unit = {

    it should "correctly set pointSign for chainId with chain specific signing schema" in {
      forAll(signedTransactionGenerator, Arbitrary.arbitrary[Unit].map(_ => generateKeyPair(secureRandom))) {
        (tx, key) =>
          val chainId: Byte = 0x3d
          //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of bouncycastle encoding
          val address = Address(
            crypto
              .kec256(key.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail)
              .drop(FirstByteOfAddress)
          )
          val signedTransaction = SignedTransaction.sign(tx, key, Some(chainId))
          val result = SignedTransactionWithSender(signedTransaction, Address(key))

          allowedPointSigns(chainId) should contain(result.tx.signature.v)
          address shouldEqual result.senderAddress
      }
    }
  }
}
