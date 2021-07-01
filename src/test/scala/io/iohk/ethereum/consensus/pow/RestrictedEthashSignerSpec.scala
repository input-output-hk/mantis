package io.iohk.ethereum.consensus.pow

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.crypto
import io.iohk.ethereum.security.SecureRandomBuilder

class RestrictedEthashSignerSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with ObjectGenerators
    with SecureRandomBuilder {
  "RestrictedEthashSigner" should "sign and validate correct header" in {
    forAll(blockHeaderGen, genKey(secureRandom)) { (header, key) =>
      val signedHeader = RestrictedPoWSigner.signHeader(header, key)
      val keyAsBytes = crypto.keyPairToByteStrings(key)._2
      assert(RestrictedPoWSigner.validateSignature(signedHeader, Set(keyAsBytes)))
    }
  }

  it should "fail to validate header signed with wrong key" in {
    forAll(blockHeaderGen, genKey(secureRandom), genKey(secureRandom)) { (header, correctKey, wrongKey) =>
      val signedHeader = RestrictedPoWSigner.signHeader(header, correctKey)
      val wrongKeyAsBytes = crypto.keyPairToByteStrings(wrongKey)._2
      assert(!RestrictedPoWSigner.validateSignature(signedHeader, Set(wrongKeyAsBytes)))
    }
  }

}
