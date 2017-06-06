package io.iohk.ethereum.crypto

import java.nio.charset.StandardCharsets

import akka.util.ByteString
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class Pbkdf2HMacSha256Spec extends FlatSpec with Matchers with PropertyChecks {

  "pbkdf2HMacSha256" should "correctly evaluate for the test vectors" in {

    // https://stackoverflow.com/a/5136918
    val testVectors = Table[String, String, Int, Int, String](
      ("passphrase", "salt", "c", "dklen", "derivedKey"),
      ("password", "salt", 1, 32,
        "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b"),
      ("password", "salt", 2, 32,
        "ae4d0c95af6b46d32d0adff928f06dd02a303f8ef3c251dfd6e2d85a95474c43"),
      ("password", "salt", 4096, 32,
        "c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a"),
      // takes to long:
      // ("password", "salt", 16777216, 32,
      //  "cf81c66fe8cfc04d1f31ecb65dab4089f7f179e89b3b0bcb17ad10e3ac6eba46"),
      ("passwordPASSWORDpassword", "saltSALTsaltSALTsaltSALTsaltSALTsalt", 4096, 40,
        "348c89dbcbd32b2f32d814b8116e84cf2b17347ebc1800181c4e2a1fb8dd53e1c635518c7dac47e9"),
      ("pass\u0000word", "sa\u0000lt", 4096, 16,
        "89b69d0516f829893c696226650a8687")
    )

    forAll(testVectors) { (pass, s, c, dklen, dk) =>
      val salt = ByteString(s.getBytes(StandardCharsets.US_ASCII))
      val derivedKey = ByteString(Hex.decode(dk))

      pbkdf2HMacSha256(pass, salt, c, dklen) shouldEqual derivedKey
    }
  }
}
