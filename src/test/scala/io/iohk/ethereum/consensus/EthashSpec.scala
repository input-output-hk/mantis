package io.iohk.ethereum.consensus

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class EthashSpec extends FlatSpec with Matchers with PropertyChecks {

  "Ethash" should "generate correct hash" in {
    forAll(Arbitrary.arbitrary[Long].filter(_ < 15000000)) { blockNumber =>
      Ethash.seed(blockNumber) shouldBe seedForBlockReference(blockNumber)
    }
  }

  it should "calculate cache size" in {
    val cacheSizes = Seq(16776896, 16907456, 17039296, 17170112, 17301056, 17432512, 17563072)
    cacheSizes.zipWithIndex.foreach { case (referenceSize, epoch) =>
      val blockNumber = epoch * Ethash.EPOCH_LENGTH
      Ethash.cacheSize(blockNumber) shouldBe referenceSize
    }
  }

  it should "compute hash" in {
    val hash = Array(0xf5, 0x7e, 0x6f, 0x3a, 0xcf, 0xc0, 0xdd, 0x4b, 0x5b, 0xf2, 0xbe, 0xe4, 0x0a, 0xb3, 0x35, 0x8a, 0xa6, 0x87, 0x73, 0xa8, 0xd0, 0x9f, 0x5e, 0x59, 0x5e, 0xab, 0x55, 0x94, 0x05, 0x52, 0x7d, 0x72).map(_.toByte)
    val nonce = Array(0xd7, 0xb3, 0xac, 0x70, 0xa3, 0x01, 0xa2, 0x49).map(_.toByte)

    val mixHash = Array(0x1f, 0xff, 0x04, 0xce, 0xc9, 0x41, 0x73, 0xfd, 0x59, 0x1e, 0x3d, 0x89, 0x60, 0xce, 0x6b, 0xdf, 0x8b, 0x19, 0x71, 0x04, 0x8c, 0x71, 0xff, 0x93, 0x7b, 0xb2, 0xd3, 0x2a, 0x64, 0x31, 0xab, 0x6d).map(_.toByte)
    val boundary = Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x3e, 0x9b, 0x6c, 0x69, 0xbc, 0x2c, 0xe2, 0xa2, 0x4a, 0x8e, 0x95, 0x69, 0xef, 0xc7, 0xd7, 0x1b, 0x33, 0x35, 0xdf, 0x36, 0x8c, 0x9a, 0xe9, 0x7e, 0x53, 0x84).map(_.toByte)

    val blockNumber = 486382
    val cache = Ethash.makeCache(blockNumber)
    val proofOfWork = Ethash.hashimotoLight(hash, nonce, Ethash.dagSize(blockNumber), cache)

    proofOfWork.mixHash shouldBe ByteString(mixHash)
    proofOfWork.difficultyBoundary shouldBe ByteString(boundary)
  }

  def seedForBlockReference(blockNumber: BigInt): ByteString = {
    if (blockNumber < Ethash.EPOCH_LENGTH) {
      //wrong version from YP:
      //ByteString(kec256(Hex.decode("00" * 32)))
      //working version:
      ByteString(Hex.decode("00" * 32))
    } else {
      kec256(seedForBlockReference(blockNumber - Ethash.EPOCH_LENGTH))
    }
  }

}
