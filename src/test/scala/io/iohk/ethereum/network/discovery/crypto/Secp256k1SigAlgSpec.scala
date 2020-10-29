package io.iohk.ethereum.network.discovery.crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scodec.bits.BitVector
import scala.util.Random

class Secp256k1SigAlgSpec extends AnyFlatSpec with Matchers {
  behavior of "Secp256k1SigAlg"

  def randomData: BitVector = {
    val size = Random.nextInt(1000)
    val bytes = Array.ofDim[Byte](size)
    Random.nextBytes(bytes)
    BitVector(bytes)
  }

  it should "generate new keypairs" in {
    val (publicKey, privateKey) = Secp256k1SigAlg.newKeyPair
    publicKey.toByteVector should have size 64
    privateKey.toByteVector should have size 32
  }

  it should "compress a public key" in {
    val (publicKey, _) = Secp256k1SigAlg.newKeyPair
    val compressedPublicKey = Secp256k1SigAlg.compressPublicKey(publicKey)
    compressedPublicKey.toByteVector should have size 33

    Secp256k1SigAlg.compressPublicKey(compressedPublicKey) shouldBe compressedPublicKey
  }

  it should "turn a private key into a public key" in {
    val (publicKey, privateKey) = Secp256k1SigAlg.newKeyPair
    Secp256k1SigAlg.toPublicKey(privateKey) shouldBe publicKey
  }

  it should "sign some data" in {
    val (_, privateKey) = Secp256k1SigAlg.newKeyPair
    val data = randomData
    val signature = Secp256k1SigAlg.sign(privateKey, data)

    signature.toByteVector should have size 65
  }
}
