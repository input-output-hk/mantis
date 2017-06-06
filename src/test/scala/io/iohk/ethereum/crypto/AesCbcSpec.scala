package io.iohk.ethereum.crypto

import java.nio.charset.StandardCharsets

import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.spongycastle.util.encoders.Hex

class AesCbcSpec extends FlatSpec with Matchers with PropertyChecks {

  "AES_CBC" should "correctly evaluate for the test vectors" in pendingUntilFixed {

    // https://tools.ietf.org/html/rfc3602#section-4
    val testVectors = Table[String, String, String, String](
      ("key", "iv", "plaintext", "ciphertext"),

      ("06a9214036b8a15b512e03d534120006",
        "3dafba429d9eb430b422da802c9fac41",
        "Single block msg",
        "e353779c1079aeb82708942dbe77181a"),

      ("c286696d887c9aa0611bbb3e2025a45a",
        "562e17996d093d28ddb3ba695a2e6f58",
        new String(Hex.decode("000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"), StandardCharsets.US_ASCII),
        "d296cd94c2cccf8a3a863028b5e1dc0a7586602d253cfff91b8266bea6d61ab1"),

      ("6c3ea0477630ce21a2ce334aa746c2cd",
        "c782dc4c098c66cbd9cd27d825682c81",
        "This is a 48-byte message (exactly 3 AES blocks)",
        "d0a02b3836451753d493665d33f0e8862dea54cdb293abc7506939276772f8d5021c19216bad525c8579695d83ba2684"),

      ("56e47a38c5598974bc46903dba290349",
        "8ce82eefbea0da3c44699ed7db51b7d9",
        new String(
          Hex.decode("a0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedf"),
          StandardCharsets.US_ASCII),
        "c30e32ffedc0774e6aff6af0869f71aa0f3af07a9a31a9c684db207eb0ef8e4e35907aa632c3ffdf868bb7b29d3d46ad83ce9f9a102ee99d49a53e87f4c3da55")
    )

    forAll(testVectors) { (k, i, plain, c) =>
      val key = ByteString(Hex.decode(k))
      val iv = ByteString(Hex.decode(i))
      val plaintext = ByteString(plain.getBytes(StandardCharsets.US_ASCII))
      val ciphertext = ByteString(Hex.decode(c))

      val encrypted = AES_CBC.encrypt(key, iv, plaintext)

      encrypted.dropRight(key.length) shouldEqual ciphertext
      AES_CBC.decrypt(key, iv, encrypted) shouldEqual Some(plaintext)
    }
  }

  it should "decrypt encrypted random values" in {
    val keyGen = Generators.getByteStringGen(16, 16)
    val ivGen = Generators.getByteStringGen(16, 16)
    val plaintextGen = Generators.getByteStringGen(1, 256)

    forAll(keyGen, ivGen, plaintextGen) { (key, iv, plaintext) =>
      val encrypted = AES_CBC.encrypt(key, iv, plaintext)
      val decrypted = AES_CBC.decrypt(key, iv, encrypted)
      decrypted shouldEqual Some(plaintext)
    }
  }
}
