package io.iohk.ethereum.consensus.ethash

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class EthashUtilsSpec extends FlatSpec with Matchers with PropertyChecks {

  import io.iohk.ethereum.consensus.ethash.EthashUtils._

  "Ethash" should "generate correct hash" in {
    forAll(Arbitrary.arbitrary[Long].filter(_ < 15000000)) { blockNumber =>
      seed(epoch(blockNumber)) shouldBe seedForBlockReference(blockNumber)
    }
  }

  it should "calculate cache size" in {
    val cacheSizes = Seq(16776896, 16907456, 17039296, 17170112, 17301056, 17432512, 17563072)
    cacheSizes.zipWithIndex.foreach { case (referenceSize, epoch) =>
      cacheSize(epoch) shouldBe referenceSize
    }
  }

  it should "compute proof of work using cache" in {
    val hash = Array(0xf5, 0x7e, 0x6f, 0x3a, 0xcf, 0xc0, 0xdd, 0x4b, 0x5b, 0xf2, 0xbe, 0xe4, 0x0a, 0xb3, 0x35, 0x8a, 0xa6, 0x87, 0x73, 0xa8, 0xd0, 0x9f, 0x5e, 0x59, 0x5e, 0xab, 0x55, 0x94, 0x05, 0x52, 0x7d, 0x72).map(_.toByte)
    val nonce = Array(0xd7, 0xb3, 0xac, 0x70, 0xa3, 0x01, 0xa2, 0x49).map(_.toByte)

    val mixHash = Array(0x1f, 0xff, 0x04, 0xce, 0xc9, 0x41, 0x73, 0xfd, 0x59, 0x1e, 0x3d, 0x89, 0x60, 0xce, 0x6b, 0xdf, 0x8b, 0x19, 0x71, 0x04, 0x8c, 0x71, 0xff, 0x93, 0x7b, 0xb2, 0xd3, 0x2a, 0x64, 0x31, 0xab, 0x6d).map(_.toByte)
    val boundary = Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x3e, 0x9b, 0x6c, 0x69, 0xbc, 0x2c, 0xe2, 0xa2, 0x4a, 0x8e, 0x95, 0x69, 0xef, 0xc7, 0xd7, 0x1b, 0x33, 0x35, 0xdf, 0x36, 0x8c, 0x9a, 0xe9, 0x7e, 0x53, 0x84).map(_.toByte)

    val blockNumber = 486382
    val cache = makeCache(epoch(blockNumber))
    val proofOfWork = hashimotoLight(hash, nonce, dagSize(epoch(blockNumber)), cache)

    proofOfWork.mixHash shouldBe ByteString(mixHash)
    proofOfWork.difficultyBoundary shouldBe ByteString(boundary)

    val table = Table(
      ("blockNumber", "hashWithoutNonce", "nonce", "mixHash"),
      (3521,"269d13f7ca546dced28ee26071dcb61085b7c54dfc5f93808b94885e136cd616","534ab630b9aa1f68","c6913517d1dc7544febde9f17e65d6ae4fa380d4a2a2d31305b0043caf95e717"),
      (5021,"7bd6c3c49a0627712c51f1abf0a7828bb25ebb8679d2584385a191db955667da","413dc4ec1a0df7c4","35890608f8867402052b2ce55a694b86a44ce87e7fb5412a77a025b184f25883"),
      (5091,"5b27820bfa3a059274ce17db0beea90ba0b6fbe6b49d2a23cbf972e8cde79319","59225875d18ad606","46f72f8b269461078e9d1cf4edf1b608f9d101e0f335ea59568c3436f291d01b"),
      (3091,"c37d980124cf83a4de4d9600f5bb6d3883797b84b7ec472feff6ca855c01d245","745609efa9c4eef3","c647fec06481b9f3f74cd771968d6d630aa11bf75ebd9e3c55ccfbae0fbad4da"),
      (1091,"c1c1efb8fdd4241a55db39e092fedae3df6d4abc13133778810027ade6557bc6","4d9ddadaea6c20b2","53624a7faac2ec82208348f7a11e3b38c880a2fec76dd8b47e434fe641eeacde"),
      (109,"aa234d4bcee14e93d127275dcc83504b6e730a14e9110bd09b68e1964f0daad3","388e6b37c22147b7","df14701b1ad6d3d5639956e463250960de3189a726cb38d71a6f6042f45dea72"),
      (1009,"3259779f9d2c477d29e18ead0ccc829bf2146723563c3e81e5e4886673d93bfb","5faa044b70ccdf6b","a1f1af0c2ca3e1d8e69da59fefbfeb4d0d172ec96bdbdac71b2cde49ddb3a828"),
      (1001,"028cc9a70d6db52c2a2606f04392e9a323d0370291d6c6d78bc8ce54acf1d761","a54b5b31ce3de766","819c26573f1a9cd6c4b9a399b72fbfb0084a104b25b62083533e114ee98a4831"),
      (1000,"15c5729eb017a703c13d00752338f6b55e2d2551b380706f0486f2ccca57ae1e","eb610e766452a801","a369e2fd5c4e357cf9f60ba063ae0baf32075b0d7ed80cd78134bc401db8f1bf"),
      (100,"41944a94a42695180b1ca231720a87825f17d36475112b659c23dea1542e0977","37129c7f29a9364b","5bb43c0772e58084b221c8e0c859a45950c103c712c5b8f11d9566ee078a4501"))

    forAll(table) { (blockNumber, hashWithoutNonce, nonce, mixHash) =>
      val cache = makeCache(epoch(blockNumber))
      val proofOfWork = hashimotoLight(Hex.decode(hashWithoutNonce), Hex.decode(nonce), dagSize(epoch(blockNumber)), cache)
      proofOfWork.mixHash shouldBe ByteString(Hex.decode(mixHash))
    }
  }

  def seedForBlockReference(blockNumber: BigInt): ByteString = {
    if (blockNumber < EPOCH_LENGTH) {
      //wrong version from YP:
      //ByteString(kec256(Hex.decode("00" * 32)))
      //working version:
      ByteString(Hex.decode("00" * 32))
    } else {
      kec256(seedForBlockReference(blockNumber - EPOCH_LENGTH))
    }
  }
}
