package io.iohk.ethereum.pow

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.pow.PowCache._
import io.iohk.ethereum.utils.Logger
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class PowCacheSpec extends FlatSpec with Matchers with PropertyChecks with Logger {

  def seedForBlockReference(blockNumber: BigInt): ByteString = {
    if (blockNumber < JEpoch) {
      ByteString(kec256(Hex.decode("00" * 32)))
    } else {
      kec256(seedForBlockReference(blockNumber - JEpoch))
    }
  }

  "PowCache" should "generate correct hash" in {
    forAll(Arbitrary.arbitrary[BigInt].filter(_ < 15000000)) { blockNumber =>
      seedForBlock(blockNumber) shouldBe seedForBlockReference(blockNumber)
    }
  }
}
