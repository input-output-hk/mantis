package io.iohk.ethereum.crypto

import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.spongycastle.util.encoders.Hex

class AesCtrSpec extends FlatSpec with Matchers with PropertyChecks {

  "AES_CTR" should "correctly evaluate for the test vectors" in {


    // http://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-38a.pdf Appendix F.5
    val testVectors = Table[String, String, String, String](
      ("key", "iv", "plaintext", "ciphertext"),

      ("2b7e151628aed2a6abf7158809cf4f3c",
        "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff",
        "6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710",
        "874d6191b620e3261bef6864990db6ce9806f66b7970fdff8617187bb9fffdff5ae4df3edbd5d35e5b4f09020db03eab1e031dda2fbe03d1792170a0f3009cee"),

      ("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b",
        "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff",
        "6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710",
        "1abc932417521ca24f2b0459fe7e6e0b090339ec0aa6faefd5ccc2c6f4ce8e941e36b26bd1ebc670d1bd1d665620abf74f78a7f6d29809585a97daec58c6b050"),

      ("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4",
        "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff",
        "6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710",
        "601ec313775789a5b7a7f504bbf3d228f443e3ca4d62b59aca84e990cacaf5c52b0930daa23de94ce87017ba2d84988ddfc9c58db67aada613c2dd08457941a6")
    )

    forAll(testVectors) { (k, i, p, c) =>
      val key = ByteString(Hex.decode(k))
      val iv = ByteString(Hex.decode(i))
      val plaintext = ByteString(Hex.decode(p))
      val ciphertext = ByteString(Hex.decode(c))

      AES_CTR.encrypt(key, iv, plaintext) shouldEqual ciphertext
      AES_CTR.decrypt(key, iv, ciphertext) shouldEqual Some(plaintext)
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
