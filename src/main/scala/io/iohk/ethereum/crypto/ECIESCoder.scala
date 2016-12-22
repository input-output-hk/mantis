package io.iohk.ethereum.crypto

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.math.BigInteger
import java.security.SecureRandom

import org.spongycastle.asn1.sec.SECNamedCurves
import org.spongycastle.asn1.x9.X9ECParameters
import org.spongycastle.crypto.digests.{SHA1Digest, SHA256Digest}
import org.spongycastle.crypto.engines.AESFastEngine
import org.spongycastle.crypto.generators.{ECKeyPairGenerator, EphemeralKeyPairGenerator}
import org.spongycastle.crypto.macs.HMac
import org.spongycastle.crypto.modes.SICBlockCipher
import org.spongycastle.crypto.params._
import org.spongycastle.crypto.parsers.ECIESPublicKeyParser
import org.spongycastle.crypto.{BufferedBlockCipher, InvalidCipherTextException}
import org.spongycastle.math.ec.ECPoint

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

    val iesEngine = new EthereumIESEngine(
      kdf = Left(new ConcatKDFBytesGenerator(new SHA256Digest)),
      mac = new HMac(new SHA256Digest),
      hash = new SHA256Digest,
      cipher = Some(new BufferedBlockCipher(new SICBlockCipher(aesFastEngine))),
      IV = IV,
      prvSrc = Left(new ECPrivateKeyParameters(prv, CURVE)),
      pubSrc = Left(new ECPublicKeyParameters(ephem, CURVE)))


    iesEngine.processBlock(cipher, 0, cipher.length, forEncryption = false, macData)
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
    val iesEngine = new EthereumIESEngine(
      kdf = Right(new MGF1BytesGeneratorExt(new SHA1Digest)),
      mac = new HMac(new SHA1Digest),
      hash = new SHA1Digest,
      cipher = None,
      IV = new Array[Byte](0),
      prvSrc = Left(new ECPrivateKeyParameters(privKey, CURVE)),
      pubSrc = Right(new ECIESPublicKeyParser(CURVE)),
      hashMacKey = false)

    iesEngine.processBlock(cipher, 0, cipher.length, forEncryption = false)
  }

  def encrypt(toPub: ECPoint, plaintext: Array[Byte], macData: Option[Array[Byte]] = None): Array[Byte] = {

    val random = new SecureRandom
    val gParam = new ECKeyGenerationParameters(CURVE, random)

    val IV = new Array[Byte](KEY_SIZE / 8)
    random.nextBytes(IV)

    val eGen = new ECKeyPairGenerator
    eGen.init(gParam)
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

    cipher = iesEngine.processBlock(plaintext, 0, plaintext.length, forEncryption = true, macData)
    val bos = new ByteArrayOutputStream
    bos.write(pub.getEncoded(false))
    bos.write(IV)
    bos.write(cipher)
    bos.toByteArray
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

    val eGen = new ECKeyPairGenerator
    val random = new SecureRandom
    val gParam = new ECKeyGenerationParameters(CURVE, random)
    eGen.init(gParam)

    val ephemeralKeyPairGenerator = new EphemeralKeyPairGenerator(eGen, new ECIESPublicKeyEncoder)

    val iesEngine = new EthereumIESEngine(
      kdf = Right(new MGF1BytesGeneratorExt(new SHA1Digest)),
      mac = new HMac(new SHA1Digest),
      hash = new SHA1Digest,
      cipher = None,
      IV = new Array[Byte](0),
      prvSrc = Right(ephemeralKeyPairGenerator),
      pubSrc = Left(new ECPublicKeyParameters(pub, CURVE)),
      hashMacKey = false)

    iesEngine.processBlock(plaintext, 0, plaintext.length, forEncryption = true)
  }

  private def makeIESEngine(isEncrypt: Boolean, pub: ECPoint, prv: BigInteger, IV: Array[Byte]) = {
    val aesFastEngine = new AESFastEngine

    val iesEngine = new EthereumIESEngine(
      kdf = Left(new ConcatKDFBytesGenerator(new SHA256Digest)),
      mac = new HMac(new SHA256Digest),
      hash = new SHA256Digest,
      cipher = Some(new BufferedBlockCipher(new SICBlockCipher(aesFastEngine))),
      IV = IV,
      prvSrc = Left(new ECPrivateKeyParameters(prv, CURVE)),
      pubSrc = Left(new ECPublicKeyParameters(pub, CURVE)))

    iesEngine
  }
}
