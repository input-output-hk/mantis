package io.iohk.ethereum.crypto

import java.math.BigInteger
import java.security.SecureRandom

import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params.{ECKeyGenerationParameters, ECPrivateKeyParameters, ECPublicKeyParameters}
import org.spongycastle.util.encoders.Hex

class ECIESCoderSpec extends FlatSpec with Matchers {

  "ECIESCoder" should "decrypt encrypted message" in {

    val generator = new ECKeyPairGenerator
    generator.init(new ECKeyGenerationParameters(curve, new SecureRandom))

    val keyPair = generator.generateKeyPair()
    val prv = keyPair.getPrivate.asInstanceOf[ECPrivateKeyParameters].getD
    val pub = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ

    val charSet = "utf-8"
    val plainText = "some test message".getBytes(charSet)
    val cryptogram = ECIESCoder.encrypt(pub, plainText)
    val result = ECIESCoder.decrypt(prv, cryptogram)

    plainText shouldBe result
  }

  "ECIESCoder" should "decryptSimple encryptSimple message" in {

    val generator = new ECKeyPairGenerator
    generator.init(new ECKeyGenerationParameters(curve, new SecureRandom))

    val keyPair = generator.generateKeyPair()
    val prv = keyPair.getPrivate.asInstanceOf[ECPrivateKeyParameters].getD
    val pub = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ

    val charSet = "utf-8"
    val plainText = "some test message".getBytes(charSet)
    val cryptogram = ECIESCoder.encryptSimple(pub, plainText)
    val result = ECIESCoder.decryptSimple(prv, cryptogram)

    plainText shouldBe result
  }

  "ECIESCoder" should "pass test for simple encoding from ethereumJ" in {

    val cipherText1 = Hex.decode("0469e324b8ab4a8e2bf0440548498226c9864d1210248ebf76c3396dd1748f0b04d347728b683993e4061998390c2cc8d6d09611da6df9769ebec888295f9be99e86ddad866f994a494361a5658d2b48d1140d73f71a382a4dc7ee2b0b5487091b0c25a3f0e6")
    val priv = new BigInteger(1, Hex.decode("d0b043b4c5d657670778242d82d68a29d25d7d711127d17b8e299f156dad361a"))
    val pub = curve.getCurve.decodePoint(Hex.decode("04bd27a63c91fe3233c5777e6d3d7b39204d398c8f92655947eb5a373d46e1688f022a1632d264725cbc7dc43ee1cfebde42fa0a86d08b55d2acfbb5e9b3b48dc5"))
    val plain1 = ECIESCoder.decryptSimple(priv, cipherText1)
    val cipherText2 = ECIESCoder.encryptSimple(pub, plain1)
    val plain2 = ECIESCoder.decryptSimple(priv, cipherText2)

    plain1 shouldBe plain2
  }

  "ECIESCoder" should "past tests from ethereumJ - test1" in {

    val privKey = new BigInteger("5e173f6ac3c669587538e7727cf19b782a4f2fda07c1eaa662c593e5e85e3051", 16)
    val cipher = Hex.decode("049934a7b2d7f9af8fd9db941d9da281ac9381b5740e1f64f7092f3588d4f87f5ce55191a6653e5e80c1c5dd538169aa123e70dc6ffc5af1827e546c0e958e42dad355bcc1fcb9cdf2cf47ff524d2ad98cbf275e661bf4cf00960e74b5956b799771334f426df007350b46049adb21a6e78ab1408d5e6ccde6fb5e69f0f4c92bb9c725c02f99fa72b9cdc8dd53cff089e0e73317f61cc5abf6152513cb7d833f09d2851603919bf0fbe44d79a09245c6e8338eb502083dc84b846f2fee1cc310d2cc8b1b9334728f97220bb799376233e113")
    val payload = ECIESCoder.decrypt(privKey, cipher)

    val expectedPayload = "802b052f8b066640bba94a4fc39d63815c377fced6fcb84d27f791c9921ddf3e9bf0108e298f490812847109cbd778fae393e80323fd643209841a3b7f110397f37ec61d84cea03dcc5e8385db93248584e8af4b4d1c832d8c7453c0089687a700"
    expectedPayload shouldBe Hex.toHexString(payload)
  }

  "ECIESCoder" should "past tests from ethereumJ - test2" in {

    val privKey = new BigInteger("5e173f6ac3c669587538e7727cf19b782a4f2fda07c1eaa662c593e5e85e3051", 16)
    val payload = Hex.decode("1122334455")
    val pubKeyPoint = curve.getG.multiply(privKey)
    val cipher = ECIESCoder.encrypt(pubKeyPoint, payload)
    val decrypted_payload = ECIESCoder.decrypt(privKey, cipher)

    decrypted_payload shouldBe payload
  }
}
