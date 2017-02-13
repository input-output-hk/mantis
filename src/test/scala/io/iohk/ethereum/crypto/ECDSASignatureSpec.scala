package io.iohk.ethereum.crypto

import java.math.BigInteger

import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class ECDSASignatureSpec extends FlatSpec with Matchers {
  "ECDSASignature" should "recoverPubBytes correctly fro go ethereum transaction" in {
    val bytesToSign = Hex.decode("5a1465f4683bf2c18fc72c0789239c0f52b3ceac666ca9551cf265a11abe912c")
    val signatureRandom = ByteString(Hex.decode("f3af65a23fbf207b933d3c962381aa50e0ac19649c59c1af1655e592a8d95401"))
    val signature = ByteString(Hex.decode("53629a403579f5ce57bcbefba2616b1c6156d308ddcd37372c94943fdabeda97"))
    val pointSign = 28

    val result = ECDSASignature.recoverPubBytes(
      new BigInteger(1, signatureRandom.toArray[Byte]),
      new BigInteger(1, signature.toArray[Byte]),
      ECDSASignature.recIdFromSignatureV(pointSign),
      bytesToSign
    )

    result.isEmpty shouldBe false
  }
}
