package io.iohk.ethereum.crypto

import java.math.BigInteger

import akka.util.ByteString
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.util.encoders.Hex

class ECDSASignatureSpec extends FlatSpec with Matchers with PropertyChecks {
  "ECDSASignature" should "recoverPubBytes correctly fro go ethereum transaction" in {
    val bytesToSign = Hex.decode("5a1465f4683bf2c18fc72c0789239c0f52b3ceac666ca9551cf265a11abe912c")
    val signatureRandom = ByteString(Hex.decode("f3af65a23fbf207b933d3c962381aa50e0ac19649c59c1af1655e592a8d95401"))
    val signature = ByteString(Hex.decode("53629a403579f5ce57bcbefba2616b1c6156d308ddcd37372c94943fdabeda97"))
    val pointSign = 28

    val sig = ECDSASignature(new BigInteger(1, signatureRandom.toArray[Byte]), new BigInteger(1, signature.toArray[Byte]), pointSign.toByte)

    sig.publicKey(bytesToSign).isEmpty shouldBe false
  }

  it should "sign message and recover public key" in {
    forAll(arbitrary[Array[Byte]], Arbitrary.arbitrary[Unit].map(_ => generateKeyPair())) {
      (message, keys) =>

        val pubKey = keys.getPublic.asInstanceOf[ECPublicKeyParameters].getQ
        val msg = kec256(message)

        val signature = ECDSASignature.sign(msg, keys)
        val recPubKey = signature.publicKey(msg)

        val result = recPubKey.map(curve.getCurve.decodePoint).map(_.getEncoded(true)).map(ByteString(_))
        val expected = Some(pubKey.getEncoded(true)).map(ByteString(_))

        result shouldBe expected
    }
  }
}
