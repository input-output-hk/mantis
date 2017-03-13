package io.iohk.ethereum.crypto

import java.nio.charset.StandardCharsets

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import org.spongycastle.util.encoders.Hex

class Ripemd160Spec extends FunSuite with PropertyChecks with Matchers {

  val examples = Table[String, String](("input", "result"),
    ("", "9c1185a5c5e9fc54612808977ee8f548b2258d31"),
    ("a", "0bdc9d2d256b3ee9daae347be6f4dc835a467ffe"),
    ("abc", "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc"),
    ("message digest", "5d0689ef49d2fae572b881b123a85ffa21595f36"),
    ("abcdefghijklmnopqrstuvwxyz", "f71c27109c692c1b56bbdceb5b9d2865b3708dbc"),
    ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq", "12a053384a9c0c88e405a06c27dcf49ada62eb2b"),
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", "b0e20b6e3116640286ed3a87a5713079b21f5189"),
    ("1234567890" * 8, "9b752e45573d4b39f4dbd3323cab82bf63326bfb"),
    ("a" * 1000000, "52783243c1697bdbe16d37f97f68f08325dc1528")
  )

  test("RIPEMD-160") {
    forAll(examples) { (input, result) =>
      val inBytes = input.getBytes(StandardCharsets.US_ASCII)
      val hash = ripemd160(inBytes)
      val encodedHash = Hex.toHexString(hash)

      encodedHash shouldEqual result
    }
  }

}
