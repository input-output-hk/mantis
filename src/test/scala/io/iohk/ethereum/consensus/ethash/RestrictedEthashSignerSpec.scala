package io.iohk.ethereum.consensus.ethash

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RestrictedEthashSignerSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with ObjectGenerators
    with SecureRandomBuilder {
  "RestrictedEthashSigner" should "sign and validate correct header" in {
    forAll(blockHeaderGen, genKey(secureRandom)) { (header, key) =>
      val signedHeader = RestrictedEthashSigner.signHeader(header, key)
      val keyAsBytes = ByteString.fromArrayUnsafe(
        key.getPublic
          .asInstanceOf[ECPublicKeyParameters]
          .getQ
          .getEncoded(false)
          .drop(1)
      )
      assert(RestrictedEthashSigner.validateSignature(signedHeader, Set(keyAsBytes)))
    }
  }

  it should "fail to validate header signed with wrong key" in {
    forAll(blockHeaderGen, genKey(secureRandom), genKey(secureRandom)) { (header, correctKey, wrongKey) =>
      val signedHeader = RestrictedEthashSigner.signHeader(header, correctKey)
      val wrongKeyAsBytes = ByteString.fromArrayUnsafe(
        wrongKey.getPublic
          .asInstanceOf[ECPublicKeyParameters]
          .getQ
          .getEncoded(false)
          .drop(1)
      )
      assert(!RestrictedEthashSigner.validateSignature(signedHeader, Set(wrongKeyAsBytes)))
    }
  }

}
