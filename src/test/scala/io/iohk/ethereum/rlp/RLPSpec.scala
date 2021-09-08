package io.iohk.ethereum.rlp

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.domain.Transaction

class RLPSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {

  "PrefixedRLPEncodable" should "reject invalid transaction type outside of [0, 0x7f]" in {
    forAll(Arbitrary.arbitrary[Byte].suchThat(b => b < Transaction.MinAllowedType || b > Transaction.MaxAllowedType)) {
      transactionType =>
        an[RuntimeException] shouldBe thrownBy(PrefixedRLPEncodable(transactionType, RLPList()))
    }

  }

  "PrefixedRLPEncodable" should "accept valid transaction type [0, 0x7f]" in {
    forAll(Gen.choose[Byte](Transaction.MinAllowedType, Transaction.MaxAllowedType)) { transactionType =>
      PrefixedRLPEncodable(transactionType, RLPList())
    }
  }
}
