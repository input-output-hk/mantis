package io.iohk.ethereum.crypto

import java.security.SecureRandom

import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.asn1.sec.SECNamedCurves
import org.spongycastle.asn1.x9.X9ECParameters
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params.{ECDomainParameters, ECKeyGenerationParameters, ECPrivateKeyParameters, ECPublicKeyParameters}

class ECIESCoderSpec extends FlatSpec with Matchers {

  val params: X9ECParameters = SECNamedCurves.getByName("secp256k1")
  val curve = new ECDomainParameters(params.getCurve, params.getG, params.getN, params.getH)


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

}
