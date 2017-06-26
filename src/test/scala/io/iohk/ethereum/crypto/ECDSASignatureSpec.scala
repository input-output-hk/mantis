package io.iohk.ethereum.crypto

import akka.util.ByteString
import io.iohk.ethereum.SecureRandomProvider
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.util.encoders.Hex

class ECDSASignatureSpec extends FlatSpec with Matchers with PropertyChecks with SecureRandomProvider {
  "ECDSASignature" should "recover public key correctly for go ethereum transaction" in {
    val bytesToSign = Hex.decode("5a1465f4683bf2c18fc72c0789239c0f52b3ceac666ca9551cf265a11abe912c")
    val signatureRandom = ByteString(Hex.decode("f3af65a23fbf207b933d3c962381aa50e0ac19649c59c1af1655e592a8d95401"))
    val signature = ByteString(Hex.decode("53629a403579f5ce57bcbefba2616b1c6156d308ddcd37372c94943fdabeda97"))
    val pointSign = 28

    val sig = ECDSASignature(BigInt(1, signatureRandom.toArray[Byte]), BigInt(1, signature.toArray[Byte]), pointSign.toByte)

    sig.publicKey(bytesToSign).isEmpty shouldBe false
  }

  it should "fail on case from transaction 74c45d0cf2332cc021bebdfee6b1c1da0b58e8f4154537adb79b025f722920a4" in {
    val bytesToSign = Hex.decode("2bb3925f178aa22c11435c61899e134fb7b1227016274b5f7b9d85c4469130ba")
    val signatureRandom = ByteString(Hex.decode("fbe3df0cf030655d817a89936850d1cc00c07c35d3b21be73cfe9a730ea8b753"))
    val signature = ByteString(Hex.decode("62d73b6a92ac23ff514315fad795bbac6d485481d356329d71467e93c87dfa42"))
    val pointSign = 0x1f

    val sig = ECDSASignature(BigInt(1, signatureRandom.toArray[Byte]), BigInt(1, signature.toArray[Byte]), pointSign.toByte)

    sig.publicKey(bytesToSign).isEmpty shouldBe true
  }

  it should "sign message and recover public key" in {
    forAll(arbitrary[Array[Byte]], Arbitrary.arbitrary[Unit].map(_ => generateKeyPair(secureRandom))) {
      (message, keys) =>

        val pubKey = keys.getPublic.asInstanceOf[ECPublicKeyParameters].getQ
        val msg = kec256(message)

        val signature = ECDSASignature.sign(msg, keys)
        val recPubKey = signature.publicKey(msg)

        val result = recPubKey.map(a => ECDSASignature.uncompressedIndicator +: a).map(curve.getCurve.decodePoint).map(_.getEncoded(true)).map(ByteString(_))
        val expected = Some(pubKey.getEncoded(true)).map(ByteString(_))

        result shouldBe expected
    }
  }
}
