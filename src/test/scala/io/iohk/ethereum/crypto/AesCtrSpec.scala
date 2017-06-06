package io.iohk.ethereum.crypto

import java.nio.charset.StandardCharsets

import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.spongycastle.util.encoders.Hex

class AesCtrSpec extends FlatSpec with Matchers with PropertyChecks {

  "AES_CTR" should "correctly evaluate for the test vectors" in pendingUntilFixed {

    fail("no test vectors")

    val testVectors = Table[String, String, String, String](
      ("key", "iv", "plaintext", "ciphertext")
      // TODO: find test vectors
    )

    forAll(testVectors) { (k, i, plain, c) =>
      val key = ByteString(Hex.decode(k))
      val iv = ByteString(Hex.decode(i))
      val plaintext = ByteString(plain.getBytes(StandardCharsets.US_ASCII))
      val ciphertext = ByteString(Hex.decode(c))

      val encrypted = AES_CTR.encrypt(key, iv, plaintext)

      encrypted shouldEqual ciphertext
      AES_CTR.decrypt(key, iv, encrypted) shouldEqual Some(plaintext)
    }
  }

  it should "decrypt encrypted random values" in {
    val keyGen = Generators.getByteStringGen(16, 16)
    val ivGen = Generators.getByteStringGen(16, 16)
    val plaintextGen = Generators.getByteStringGen(1, 256)

    forAll(keyGen, ivGen, plaintextGen) { (key, iv, plaintext) =>
      val encrypted = AES_CTR.encrypt(key, iv, plaintext)
      val decrypted = AES_CTR.decrypt(key, iv, encrypted)
      decrypted shouldEqual Some(plaintext)
    }
  }
}
