package io.iohk.ethereum.crypto

import java.nio.charset.StandardCharsets

import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.spongycastle.util.encoders.Hex

class ScryptSpec extends FlatSpec with Matchers with PropertyChecks {

  "scrypt" should "correctly evaluate for the test vectors" in {

    // https://datatracker.ietf.org/doc/rfc7914/?include_text=1
    val testVectors = Table[String, String, Int, Int, Int, Int, String](
      ("passphrase", "salt", "n", "r", "p", "dklen", "derivedKey"),
      ("", "", 16, 1, 1, 64,
        "77d6576238657b203b19ca42c18a0497f16b4844e3074ae8dfdffa3fede21442fcd0069ded0948f8326a753a0fc81f17e8d3e0fb2e0d3628cf35e20c38d18906"),

      ("password", "NaCl", 1024, 8, 16, 64,
        "fdbabe1c9d3472007856e7190d01e9fe7c6ad7cbc8237830e77376634b3731622eaf30d92e22a3886ff109279d9830dac727afb94a83ee6d8360cbdfa2cc0640"),

      // takes a bit too long to be run on the CI
      // leaving this around as it's a valid test if we want to examine this function in the future
      // ("pleaseletmein", "SodiumChloride", 1048576, 8, 1, 64,
      //   "2101cb9b6a511aaeaddbbe09cf70f881ec568d574a2ffd4dabe5ee9820adaa478e56fd8f4ba5d09ffa1c6d927c40f4c337304049e8a952fbcbf45c6fa77a41a4"),

      ("pleaseletmein", "SodiumChloride", 16384, 8, 1, 64,
        "7023bdcb3afd7348461c06cd81fd38ebfda8fbba904f8e3ea9b543f6545da1f2d5432955613f0fcf62d49705242a9af9e61e85dc0d651e40dfcf017b45575887")
    )

    forAll(testVectors) { (pass, s, n, r, p, dklen, dk) =>
      val salt = ByteString(s.getBytes(StandardCharsets.US_ASCII))
      val derivedKey = ByteString(Hex.decode(dk))

      scrypt(pass, salt, n, r, p, dklen) shouldEqual derivedKey
    }
  }
}
