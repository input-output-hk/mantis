package io.iohk.ethereum

import java.math.BigInteger
import java.security.SecureRandom

import org.spongycastle.asn1.sec.SECNamedCurves
import org.spongycastle.crypto.BufferedBlockCipher
import org.spongycastle.crypto.agreement.ECDHBasicAgreement
import org.spongycastle.crypto.digests.SHA256Digest
import org.spongycastle.crypto.engines.AESFastEngine
import org.spongycastle.crypto.generators.{ECKeyPairGenerator, KDF2BytesGenerator}
import org.spongycastle.crypto.macs.HMac
import org.spongycastle.crypto.modes.SICBlockCipher
import org.spongycastle.crypto.params.{ECDomainParameters, ECKeyGenerationParameters, ECPrivateKeyParameters, ECPublicKeyParameters}
import org.spongycastle.math.ec.ECPoint

class Handshake {

  val algorithm = "EC"
  val curve = "secp256k1"

  def handshake(host: String, port: Int, nodePubKey: String): Unit = {


    val kdf: KDF2BytesGenerator = new KDF2BytesGenerator(new SHA256Digest)
    val mac: HMac = new HMac(new SHA256Digest)
    val hash: SHA256Digest = new SHA256Digest
    val aes: BufferedBlockCipher = new BufferedBlockCipher(new SICBlockCipher(new AESFastEngine))


    val params = SECNamedCurves.getByName("secp256k1")
    val curve = new ECDomainParameters(params.getCurve, params.getG, params.getN, params.getH)
    val prvKey = new ECPrivateKeyParameters(new BigInteger("123"), curve)


    val point: ECPoint = curve.getCurve.decodePoint("example".getBytes)
    val pubKey = new ECPublicKeyParameters(point, curve)


    val agreement: ECDHBasicAgreement = new ECDHBasicAgreement
    agreement.init(prvKey)
    val agree = agreement.calculateAgreement(pubKey)



    //ephemeral keys generation
    val generator = new ECKeyPairGenerator
    generator.init(new ECKeyGenerationParameters(curve, new SecureRandom))

    val keyPair = generator.generateKeyPair()

    keyPair.getPrivate
    keyPair.getPublic

  }
}
