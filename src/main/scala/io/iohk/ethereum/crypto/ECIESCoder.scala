package io.iohk.ethereum.crypto

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.math.BigInteger
import java.security.SecureRandom

import com.google.common.base.Throwables
import org.spongycastle.asn1.sec.SECNamedCurves
import org.spongycastle.asn1.x9.X9ECParameters
import org.spongycastle.crypto.{BufferedBlockCipher, InvalidCipherTextException}
import org.spongycastle.crypto.agreement.ECDHBasicAgreement
import org.spongycastle.crypto.digests.{SHA1Digest, SHA256Digest}
import org.spongycastle.crypto.engines.AESFastEngine
import org.spongycastle.crypto.generators.{ECKeyPairGenerator, EphemeralKeyPairGenerator}
import org.spongycastle.crypto.macs.HMac
import org.spongycastle.crypto.modes.SICBlockCipher
import org.spongycastle.crypto.params._
import org.spongycastle.crypto.parsers.ECIESPublicKeyParser
import org.spongycastle.math.ec.ECPoint
import org.spongycastle.util.BigIntegers

object ECIESCoder {
  val KEY_SIZE = 128
  val params: X9ECParameters = SECNamedCurves.getByName("secp256k1")
  val CURVE = new ECDomainParameters(params.getCurve, params.getG, params.getN, params.getH)

  @throws[IOException]
  @throws[InvalidCipherTextException]
  def decrypt(privKey: BigInteger, cipher: Array[Byte], macData: Option[Array[Byte]] = None): Array[Byte] = {
    var plaintext: Array[Byte] = null
    val is = new ByteArrayInputStream(cipher)
    val ephemBytes = new Array[Byte](2 * ((CURVE.getCurve.getFieldSize + 7) / 8) + 1)
    is.read(ephemBytes)
    val ephem = CURVE.getCurve.decodePoint(ephemBytes)
    val IV = new Array[Byte](KEY_SIZE / 8)
    is.read(IV)
    val cipherBody = new Array[Byte](is.available)
    is.read(cipherBody)
    plaintext = decrypt(ephem, privKey, IV, cipherBody, macData)
    plaintext
  }

  @throws[InvalidCipherTextException]
  def decrypt(ephem: ECPoint, prv: BigInteger, IV: Array[Byte], cipher: Array[Byte], macData: Option[Array[Byte]]): Array[Byte] = {
    val aesFastEngine = new AESFastEngine
    val d = new Array[Byte](0)
    val e = new Array[Byte](0)
    val p = new IESWithCipherParameters(d, e, KEY_SIZE, KEY_SIZE)
    val parametersWithIV = new ParametersWithIV(p, IV)

    val agree = new ECDHBasicAgreement
    val privParam = new ECPrivateKeyParameters(prv, CURVE)
    val pubParam = new ECPublicKeyParameters(ephem, CURVE)
    agree.init(privParam)
    val z = agree.calculateAgreement(pubParam)
    val VZ = BigIntegers.asUnsignedByteArray(agree.getFieldSize, z)


    val iesEngine = new EthereumIESEngine(
      agree,
      kdf = Left(new ConcatKDFBytesGenerator(new SHA256Digest, new KDFParameters(VZ, p.getDerivationV))),
      mac = new HMac(new SHA256Digest),
      hash = new SHA256Digest,
      cipher = Some(new BufferedBlockCipher(new SICBlockCipher(aesFastEngine))))

    iesEngine.init(
      forEncryption = false,
      privParam = new ECPrivateKeyParameters(prv, CURVE),
      pubParam = new ECPublicKeyParameters(ephem, CURVE),
      params = parametersWithIV)

    iesEngine.processBlock(cipher, 0, cipher.length, macData)
  }

  /**
    * Encryption equivalent to the Crypto++ default ECIES<ECP> settings:
    *
    * DL_KeyAgreementAlgorithm:        DL_KeyAgreementAlgorithm_DH<struct ECPPoint,struct EnumToType<enum CofactorMultiplicationOption,0> >
    * DL_KeyDerivationAlgorithm:       DL_KeyDerivationAlgorithm_P1363<struct ECPPoint,0,class P1363_KDF2<class SHA1> >
    * DL_SymmetricEncryptionAlgorithm: DL_EncryptionAlgorithm_Xor<class HMAC<class SHA1>,0>
    * DL_PrivateKey:                   DL_Key<ECPPoint>
    * DL_PrivateKey_EC<class ECP>
    *
    * Used for Whisper V3
    */
  @throws[IOException]
  @throws[InvalidCipherTextException]
  def decryptSimple(privKey: BigInteger, cipher: Array[Byte]): Array[Byte] = {
    val iesEngine = new EthereumIESEngine(new ECDHBasicAgreement, Right(new MGF1BytesGeneratorExt(new SHA1Digest, 1)), new HMac(new SHA1Digest), new SHA1Digest, None)
    val p = new IESParameters(null, null, KEY_SIZE)
    val parametersWithIV = new ParametersWithIV(p, new Array[Byte](0))
    iesEngine.setHashMacKey(false)
    iesEngine.init(new ECPrivateKeyParameters(privKey, CURVE), parametersWithIV, new ECIESPublicKeyParser(CURVE))
    iesEngine.processBlock(cipher, 0, cipher.length)
  }

  def encrypt(toPub: ECPoint, plaintext: Array[Byte], macData: Option[Array[Byte]] = None): Array[Byte] = {
    val eGen = new ECKeyPairGenerator
    val random = new SecureRandom
    val gParam = new ECKeyGenerationParameters(CURVE, random)
    eGen.init(gParam)
    val IV = new Array[Byte](KEY_SIZE / 8)
    new SecureRandom().nextBytes(IV)
    val ephemPair = eGen.generateKeyPair
    val prv = ephemPair.getPrivate.asInstanceOf[ECPrivateKeyParameters].getD
    val pub = ephemPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ
    val iesEngine = makeIESEngine(isEncrypt = true, toPub, prv, IV)
    val keygenParams = new ECKeyGenerationParameters(CURVE, random)
    val generator = new ECKeyPairGenerator
    generator.init(keygenParams)
    val gen = new ECKeyPairGenerator
    gen.init(new ECKeyGenerationParameters(CURVE, random))
    var cipher: Array[Byte] = null
    try {
      cipher = iesEngine.processBlock(plaintext, 0, plaintext.length, macData)
      val bos = new ByteArrayOutputStream
      bos.write(pub.getEncoded(false))
      bos.write(IV)
      bos.write(cipher)
      bos.toByteArray

    }catch {
      case e: InvalidCipherTextException => {
        throw Throwables.propagate(e)
      }
      case e: IOException => {
        throw Throwables.propagate(e)
      }
    }
  }

  /**
    * Encryption equivalent to the Crypto++ default ECIES<ECP> settings:
    *
    * DL_KeyAgreementAlgorithm:        DL_KeyAgreementAlgorithm_DH<struct ECPPoint,struct EnumToType<enum CofactorMultiplicationOption,0> >
    * DL_KeyDerivationAlgorithm:       DL_KeyDerivationAlgorithm_P1363<struct ECPPoint,0,class P1363_KDF2<class SHA1> >
    * DL_SymmetricEncryptionAlgorithm: DL_EncryptionAlgorithm_Xor<class HMAC<class SHA1>,0>
    * DL_PrivateKey:                   DL_Key<ECPPoint>
    * DL_PrivateKey_EC<class ECP>
    *
    * Used for Whisper V3
    */
  @throws[IOException]
  @throws[InvalidCipherTextException]
  def encryptSimple(pub: ECPoint, plaintext: Array[Byte]): Array[Byte] = {
    val iesEngine = new EthereumIESEngine(new ECDHBasicAgreement, Right(new MGF1BytesGeneratorExt(new SHA1Digest, 1)), new HMac(new SHA1Digest), new SHA1Digest, None)
    val p = new IESParameters(null, null, KEY_SIZE)
    val parametersWithIV = new ParametersWithIV(p, new Array[Byte](0))
    iesEngine.setHashMacKey(false)
    val eGen = new ECKeyPairGenerator
    val random = new SecureRandom
    val gParam = new ECKeyGenerationParameters(CURVE, random)
    eGen.init(gParam)

    val ephemeralKeyPairGenerator = new EphemeralKeyPairGenerator(/*testGen*/ eGen, new ECIESPublicKeyEncoder)
    iesEngine.init(new ECPublicKeyParameters(pub, CURVE), parametersWithIV, ephemeralKeyPairGenerator)
    iesEngine.processBlock(plaintext, 0, plaintext.length)
  }

  private def makeIESEngine(isEncrypt: Boolean, pub: ECPoint, prv: BigInteger, IV: Array[Byte]) = {
    val aesFastEngine = new AESFastEngine
    val d = new Array[Byte](0)
    val e = new Array[Byte](0)
    val p = new IESWithCipherParameters(d, e, KEY_SIZE, KEY_SIZE)
    val parametersWithIV = new ParametersWithIV(p, IV)

    val agree = new ECDHBasicAgreement
    val privParam = new ECPrivateKeyParameters(prv, CURVE)
    val pubParam = new ECPublicKeyParameters(pub, CURVE)
    agree.init(privParam)
    val z = agree.calculateAgreement(pubParam)
    val VZ = BigIntegers.asUnsignedByteArray(agree.getFieldSize, z)

    val iesEngine = new EthereumIESEngine(
      agree,
      kdf = Left(new ConcatKDFBytesGenerator(new SHA256Digest, new KDFParameters(VZ, p.getDerivationV))),
      mac = new HMac(new SHA256Digest),
      hash = new SHA256Digest,
      cipher = Some(new BufferedBlockCipher(new SICBlockCipher(aesFastEngine))))

    iesEngine.init(
      forEncryption = isEncrypt,
      privParam = new ECPrivateKeyParameters(prv, CURVE),
      pubParam = new ECPublicKeyParameters(pub, CURVE),
      params = parametersWithIV)

    iesEngine
  }

  def getOverhead: Int = {
    // 256 bit EC public key, IV, 256 bit MAC
    65 + KEY_SIZE / 8 + 32
  }
}
