package io.iohk.ethereum.crypto

import java.io.ByteArrayInputStream
import java.io.IOException
import java.math.BigInteger
import java.security.SecureRandom

import org.bouncycastle.crypto.BufferedBlockCipher
import org.bouncycastle.crypto.InvalidCipherTextException
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.engines.AESEngine
import org.bouncycastle.crypto.generators.ECKeyPairGenerator
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.modes.SICBlockCipher
import org.bouncycastle.crypto.params._
import org.bouncycastle.math.ec.ECPoint

object ECIESCoder {

  val KeySize = 128
  val PublicKeyOverheadSize = 65
  val MacOverheadSize = 32
  val OverheadSize: Int = PublicKeyOverheadSize + KeySize / 8 + MacOverheadSize

  @throws[IOException]
  @throws[InvalidCipherTextException]
  def decrypt(privKey: BigInteger, cipher: Array[Byte], macData: Option[Array[Byte]] = None): Array[Byte] = {
    val is = new ByteArrayInputStream(cipher)
    val ephemBytes = new Array[Byte](2 * ((curve.getCurve.getFieldSize + 7) / 8) + 1)
    is.read(ephemBytes)
    val ephem = curve.getCurve.decodePoint(ephemBytes)
    val IV = new Array[Byte](KeySize / 8)
    is.read(IV)
    val cipherBody = new Array[Byte](is.available)
    is.read(cipherBody)
    decrypt(ephem, privKey, Some(IV), cipherBody, macData)
  }

  @throws[InvalidCipherTextException]
  def decrypt(
      ephem: ECPoint,
      prv: BigInteger,
      IV: Option[Array[Byte]],
      cipher: Array[Byte],
      macData: Option[Array[Byte]]
  ): Array[Byte] = {
    val aesEngine = new AESEngine

    val iesEngine = new EthereumIESEngine(
      kdf = Left(new ConcatKDFBytesGenerator(new SHA256Digest)),
      mac = new HMac(new SHA256Digest),
      hash = new SHA256Digest,
      cipher = Some(new BufferedBlockCipher(new SICBlockCipher(aesEngine))),
      IV = IV,
      prvSrc = Left(new ECPrivateKeyParameters(prv, curve)),
      pubSrc = Left(new ECPublicKeyParameters(ephem, curve))
    )

    iesEngine.processBlock(cipher, 0, cipher.length, forEncryption = false, macData)
  }

  def encrypt(
      toPub: ECPoint,
      secureRandom: SecureRandom,
      plaintext: Array[Byte],
      macData: Option[Array[Byte]] = None
  ): Array[Byte] = {

    val gParam = new ECKeyGenerationParameters(curve, secureRandom)

    val IV = secureRandomByteArray(secureRandom, KeySize / 8)

    val eGen = new ECKeyPairGenerator
    eGen.init(gParam)
    val ephemPair = eGen.generateKeyPair

    val prv = ephemPair.getPrivate.asInstanceOf[ECPrivateKeyParameters].getD
    val pub = ephemPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ

    val iesEngine = makeIESEngine(toPub, prv, Some(IV))

    val keygenParams = new ECKeyGenerationParameters(curve, secureRandom)
    val generator = new ECKeyPairGenerator
    generator.init(keygenParams)
    val gen = new ECKeyPairGenerator
    gen.init(new ECKeyGenerationParameters(curve, secureRandom))

    pub.getEncoded(false) ++ IV ++ iesEngine.processBlock(plaintext, 0, plaintext.length, forEncryption = true, macData)
  }

  private def makeIESEngine(pub: ECPoint, prv: BigInteger, IV: Option[Array[Byte]]) = {
    val aesEngine = new AESEngine

    val iesEngine = new EthereumIESEngine(
      kdf = Left(new ConcatKDFBytesGenerator(new SHA256Digest)),
      mac = new HMac(new SHA256Digest),
      hash = new SHA256Digest,
      cipher = Some(new BufferedBlockCipher(new SICBlockCipher(aesEngine))),
      IV = IV,
      prvSrc = Left(new ECPrivateKeyParameters(prv, curve)),
      pubSrc = Left(new ECPublicKeyParameters(pub, curve))
    )

    iesEngine
  }

}
