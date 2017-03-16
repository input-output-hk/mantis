package io.iohk.ethereum.utils

import java.math.BigInteger

import io.iohk.ethereum.crypto.curve
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.{ECPrivateKeyParameters, ECPublicKeyParameters}
import org.spongycastle.util.encoders.Hex

import io.iohk.ethereum.network._

object AsymmetricCipherKeyPairSerializable {

  def toHexStrings(keysValuePair: AsymmetricCipherKeyPair): (String, String) = {
    val pubKey = keysValuePair.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId
    val privKey = keysValuePair.getPrivate.asInstanceOf[ECPrivateKeyParameters].getD.toByteArray
    (Hex.toHexString(pubKey), Hex.toHexString(privKey))
  }

  def fromHexStrings(publicKey: String, privateKey: String): AsymmetricCipherKeyPair = {
    val pub = new ECPublicKeyParameters(publicKeyFromNodeId(publicKey), curve)
    val priv = new ECPrivateKeyParameters(new BigInteger(Hex.decode(privateKey)), curve)
    new AsymmetricCipherKeyPair(pub, priv)
  }

}
