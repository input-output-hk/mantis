package io.iohk.ethereum.keystore

import org.scalatest.{FlatSpec, Matchers}
import io.iohk.ethereum.{SecureRandomProvider, crypto}
import io.iohk.ethereum.domain.Address

class EncryptedKeySpec extends FlatSpec with Matchers with SecureRandomProvider {

  val gethKey =
    """{
      |  "id": "033b7a63-30f2-47fc-bbbe-d22925a14ab3",
      |  "address": "932245e1c40ec2026a2c7acc80befb68816cdba4",
      |  "crypto": {
      |    "cipher": "aes-128-ctr",
      |    "ciphertext": "8fb53f8695795d1f0480cad7954bd7a888392bb24c414b9895b4cb288b4897dc",
      |    "cipherparams": {
      |      "iv": "7a754cfd548a351aed270f6b1bfd306d"
      |    },
      |    "kdf": "scrypt",
      |    "kdfparams": {
      |      "dklen": 32,
      |      "n": 262144,
      |      "p": 1,
      |      "r": 8,
      |      "salt": "2321125eff8c3172a05a5947726004075b30e0a01534061fa5c13fb4e5e32465"
      |    },
      |    "mac": "6383677d3b0f34b1dcb9e1c767f8130daf6233266e35f28e00467af97bf2fbfa"
      |  },
      |  "version": 3
      |}
    """.stripMargin

  val parityKey =
    """{
      |  "id": "20909a42-09c4-0740-02dc-a0b8cbaea688",
      |  "version": 3,
      |  "crypto": {
      |    "cipher": "aes-128-ctr",
      |    "cipherparams": {
      |      "iv": "68521b4d5fc5ecf83bbe24768a321fe5"
      |    },
      |    "ciphertext": "235ce2efb355a963eb838f42e3c30e59a00ab14030e66202d729f60fc7af57b3",
      |    "kdf": "pbkdf2",
      |    "kdfparams": {
      |      "c": 10240,
      |      "dklen": 32,
      |      "prf": "hmac-sha256",
      |      "salt": "b720278006c39e3ed01903421aad4ca3d3267f40ddc48bf4ec06429eb1c10fc5"
      |    },
      |    "mac": "f52a2c40173dea137695f40f0f6bed67fc7814e16639acdbff11092cc9f563d0"
      |  },
      |  "address": "04fecb5c49ee66fdbda1f196c120225bdd1ac35c",
      |  "name": "20909a42-09c4-0740-02dc-a0b8cbaea688",
      |  "meta": "{}"
      |}""".stripMargin


  "EncryptedKey" should "securely store private keys" in {
    val prvKey = crypto.secureRandomByteString(secureRandom, 32)
    val passphrase = "P4S5W0rd"
    val encKey = EncryptedKey(prvKey, passphrase, secureRandom)

    val json = EncryptedKeyJsonCodec.toJson(encKey)
    val decoded = EncryptedKeyJsonCodec.fromJson(json)

    decoded shouldEqual Right(encKey)
    decoded.flatMap(_.decrypt(passphrase)) shouldEqual Right(prvKey)
  }

  it should "decrypt a key encrypted by Geth" in {
    val encKey = EncryptedKeyJsonCodec.fromJson(gethKey)
    val prvKey = encKey.flatMap(_.decrypt("qwerty"))
    val address = prvKey.map(k => Address(crypto.kec256(crypto.pubKeyFromPrvKey(k))))
    address shouldEqual Right(Address("932245e1c40ec2026a2c7acc80befb68816cdba4"))
  }

  it should "decrypt a key encrypted by Parity" in {
    val encKey = EncryptedKeyJsonCodec.fromJson(parityKey)
    val prvKey = encKey.flatMap(_.decrypt("qwerty"))
    val address = prvKey.map(k => Address(crypto.kec256(crypto.pubKeyFromPrvKey(k))))
    address shouldEqual Right(Address("04fecb5c49ee66fdbda1f196c120225bdd1ac35c"))
  }
}
