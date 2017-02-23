package io.iohk.ethereum.network

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.crypto.params.{ECPrivateKeyParameters, ECPublicKeyParameters}

class AsymmetricCipherKeyPairSerializableSpec extends FlatSpec with Matchers {

  "AsymmetricCipherKeyPairSerializable" should "properly serialize and deserialize the private and public keys" in {
    val keysValuePair = generateKeyPair()

    //Deserizaling
    val (pub, priv) = AsymmetricCipherKeyPairSerializable.toBytes(keysValuePair)
    val keysValuePairObtained = AsymmetricCipherKeyPairSerializable.fromBytes(pub, priv)

    val publicKeyParam = keysValuePair.getPublic.asInstanceOf[ECPublicKeyParameters]
    val publicKeyParamObtained = keysValuePairObtained.getPublic.asInstanceOf[ECPublicKeyParameters]
    publicKeyParam.getQ shouldBe publicKeyParamObtained.getQ
    publicKeyParam.getParameters shouldBe publicKeyParamObtained.getParameters
    publicKeyParam.isPrivate shouldBe publicKeyParamObtained.isPrivate

    val privateKeyParam = keysValuePair.getPrivate.asInstanceOf[ECPrivateKeyParameters]
    val privateKeyParamObtained = keysValuePairObtained.getPrivate.asInstanceOf[ECPrivateKeyParameters]
    privateKeyParam.getD shouldBe privateKeyParamObtained.getD
    privateKeyParam.getParameters shouldBe privateKeyParamObtained.getParameters
    privateKeyParam.isPrivate shouldBe privateKeyParamObtained.isPrivate

    //Serializing
    val (pub2, priv2) = AsymmetricCipherKeyPairSerializable.toBytes(keysValuePairObtained)
    ByteString(pub2) shouldBe ByteString(pub)
    ByteString(priv2) shouldBe ByteString(priv)
  }
}
