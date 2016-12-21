package io.iohk.ethereum.crypto

import java.io.ByteArrayInputStream

import org.spongycastle.crypto._
import org.spongycastle.crypto.agreement.ECDHBasicAgreement
import org.spongycastle.crypto.generators.EphemeralKeyPairGenerator
import org.spongycastle.crypto.params._
import org.spongycastle.crypto.parsers.ECIESPublicKeyParser
import org.spongycastle.util.{Arrays, BigIntegers, Pack}

/**
  * Support class for constructing integrated encryption cipher
  * for doing basic message exchanges on top of key agreement ciphers.
  * Follows the description given in IEEE Std 1363a with a couple of changes
  * specific to Ethereum:
  * - Hash the MAC key before use
  * - Include the encryption IV in the MAC computation
  */

/**
  * set up for use with stream mode, where the key derivation function
  * is used to provide a stream of bytes to xor with the message.
  *
  * @param kdf                    the key derivation function used for byte generation
  * @param mac                    the message authentication code generator for the message
  * @param hash                   hash ing function
  * @param cipher                 the actual cipher
  * @param IV                     vector with random values used to initialize cipher
  * @param prvSrc                 private key source
  * @param pubSrc                 public key source
  * @param addLenOfEncodingVector determines if length of encoding vector should be added whne calculating mac
  * @param hashMacKey             determines if for has use mac key value (if false) or hashed mac key value (if true)
  */
class EthereumIESEngine(kdf: Either[ConcatKDFBytesGenerator, MGF1BytesGeneratorExt],
                        mac: Mac,
                        hash: Digest,
                        cipher: Option[BufferedBlockCipher],
                        IV: Array[Byte],
                        prvSrc: Either[ECPrivateKeyParameters, EphemeralKeyPairGenerator],
                        pubSrc: Either[ECPublicKeyParameters, ECIESPublicKeyParser],
                        addLenOfEncodingVector: Boolean,
                        hashMacKey: Boolean = true) {

  @throws[InvalidCipherTextException]
  private def encryptBlock(in: Array[Byte], inOff: Int, inLen: Int, macData: Option[Array[Byte]], V: Array[Byte], fillKDFunction: Int => Array[Byte]) = {
    var C: Array[Byte] = null
    var K: Array[Byte] = null
    var K1: Array[Byte] = null
    var K2: Array[Byte] = null
    var len = 0


    cipher match {
      case Some(cphr) =>
        // Block cipher mode.
        K1 = new Array[Byte](ECIESCoder.KEY_SIZE / 8)
        K2 = new Array[Byte](ECIESCoder.KEY_SIZE / 8)
        K = fillKDFunction(K1.length + K2.length)

        System.arraycopy(K, 0, K1, 0, K1.length)
        System.arraycopy(K, K1.length, K2, 0, K2.length)

        // If iv provided use it to initialise the cipher
        if (IV != null) cphr.init(true, new ParametersWithIV(new KeyParameter(K1), IV))
        else cphr.init(true, new KeyParameter(K1))
        C = new Array[Byte](cphr.getOutputSize(inLen))
        len = cphr.processBytes(in, inOff, inLen, C, 0)
        len += cphr.doFinal(C, len)
      case None =>
        // Streaming mode.
        K1 = new Array[Byte](inLen)
        K2 = new Array[Byte](ECIESCoder.KEY_SIZE / 8)
        K = fillKDFunction(K1.length + K2.length)

        System.arraycopy(K, 0, K1, 0, K1.length)
        System.arraycopy(K, inLen, K2, 0, K2.length)

        C = (0 until inLen).map { i =>
          (in(inOff + i) ^ K1(i)).toByte
        }.toArray

        len = inLen
    }

    // Apply the MAC.
    val T = new Array[Byte](mac.getMacSize)
    var K2a: Array[Byte] = null

    if (hashMacKey) {
      K2a = new Array[Byte](hash.getDigestSize)
      hash.reset()
      hash.update(K2, 0, K2.length)
      hash.doFinal(K2a, 0)
    } else {
      K2a = K2
    }

    mac.init(new KeyParameter(K2a))
    mac.update(IV, 0, IV.length)
    mac.update(C, 0, C.length)

    // Convert the length of the encoding vector into a byte array.
    if (V.length != 0 && addLenOfEncodingVector) {
      val lenOfEncodingVector = 0
      val L2 = new Array[Byte](4)
      Pack.intToBigEndian(lenOfEncodingVector * 8, L2, 0)
      mac.update(L2, 0, L2.length)
    }

    if (macData.isDefined) mac.update(macData.get, 0, macData.get.length)
    mac.doFinal(T, 0)
    // Output the triple (V,C,T).
    val Output = new Array[Byte](V.length + len + T.length)
    System.arraycopy(V, 0, Output, 0, V.length)
    System.arraycopy(C, 0, Output, V.length, len)
    System.arraycopy(T, 0, Output, V.length + len, T.length)
    Output
  }

  @throws[InvalidCipherTextException]
  private def decryptBlock(in_enc: Array[Byte], inOff: Int, inLen: Int, macData: Option[Array[Byte]], V: Array[Byte], fillKDFunction: Int => Array[Byte]) = {
    var M: Array[Byte] = null
    var K: Array[Byte] = null
    var K1: Array[Byte] = null
    var K2: Array[Byte] = null
    var len = 0
    // Ensure that the length of the input is greater than the MAC in bytes
    if (inLen <= (ECIESCoder.KEY_SIZE / 8)) throw new InvalidCipherTextException("Length of input must be greater than the MAC")


    cipher match {
      case Some(cphr) =>
        // Block cipher mode.
        K1 = new Array[Byte](ECIESCoder.KEY_SIZE / 8)
        K2 = new Array[Byte](ECIESCoder.KEY_SIZE / 8)
        K = fillKDFunction(K1.length + K2.length)

        System.arraycopy(K, 0, K1, 0, K1.length)
        System.arraycopy(K, K1.length, K2, 0, K2.length)
        // If IV provide use it to initialize the cipher
        if (IV != null) cphr.init(false, new ParametersWithIV(new KeyParameter(K1), IV))
        else cphr.init(false, new KeyParameter(K1))
        M = new Array[Byte](cphr.getOutputSize(inLen - V.length - mac.getMacSize))
        len = cphr.processBytes(in_enc, inOff + V.length, inLen - V.length - mac.getMacSize, M, 0)
        len += cphr.doFinal(M, len)
      case None =>
        // Streaming mode.
        K1 = new Array[Byte](inLen - V.length - mac.getMacSize)
        K2 = new Array[Byte](ECIESCoder.KEY_SIZE / 8)
        K = fillKDFunction(K1.length + K2.length)

        System.arraycopy(K, 0, K1, 0, K1.length)
        System.arraycopy(K, K1.length, K2, 0, K2.length)

        M = (0 until K1.length).map { i =>
          (in_enc(inOff + V.length + i) ^ K1(i)).toByte
        }.toArray

        len = K1.length
    }


    // Verify the MAC.
    val end = inOff + inLen
    val T1 = Arrays.copyOfRange(in_enc, end - mac.getMacSize, end)
    val T2 = new Array[Byte](T1.length)
    var K2a: Array[Byte] = null

    if (hashMacKey) {
      K2a = new Array[Byte](hash.getDigestSize)
      hash.reset()
      hash.update(K2, 0, K2.length)
      hash.doFinal(K2a, 0)
    } else {
      K2a = K2
    }

    mac.init(new KeyParameter(K2a))
    mac.update(IV, 0, IV.length)
    mac.update(in_enc, inOff + V.length, inLen - V.length - T2.length)

    // Convert the length of the encoding vector into a byte array.
    if (V.length != 0 && addLenOfEncodingVector) {
      val lenOfEncodingVector = 0
      val L2 = new Array[Byte](4)
      Pack.intToBigEndian(lenOfEncodingVector * 8, L2, 0)
      mac.update(L2, 0, L2.length)
    }

    if (macData.isDefined) mac.update(macData.get, 0, macData.get.length)
    mac.doFinal(T2, 0)
    if (!Arrays.constantTimeAreEqual(T1, T2)) throw new InvalidCipherTextException("Invalid MAC.")
    // Output the message.
    Arrays.copyOfRange(M, 0, len)
  }

  @throws[InvalidCipherTextException]
  def processBlock(in: Array[Byte], inOff: Int, inLen: Int, forEncryption: Boolean, macData: Option[Array[Byte]] = None): Array[Byte] = {
    var V: Array[Byte] = new Array[Byte](0)

    val prv = prvSrc.fold(
      key => key,
      keyPairGenerator => {
        val ephKeyPair = keyPairGenerator.generate
        V = ephKeyPair.getEncodedPublicKey
        ephKeyPair.getKeyPair.getPrivate.asInstanceOf[ECPrivateKeyParameters]
      })

    val pub = pubSrc.fold(
      key => key,
      keyParser => {
        val bIn = new ByteArrayInputStream(in, inOff, inLen)
        val result = keyParser.readKey(bIn).asInstanceOf[ECPublicKeyParameters]
        val encLength = inLen - bIn.available
        V = Arrays.copyOfRange(in, inOff, inOff + encLength)
        result
      }
    )
    val agree = new ECDHBasicAgreement
    agree.init(prv)
    val z = agree.calculateAgreement(pub)
    val VZ = BigIntegers.asUnsignedByteArray(agree.getFieldSize, z)


    val fillKDFunction = (outLen: Int) => kdf.fold(
      _.generateBytes(outLen, VZ).toArray,
      _.generateBytes(outLen, VZ).toArray
    )

    if (forEncryption) encryptBlock(in, inOff, inLen, macData, V, fillKDFunction)
    else decryptBlock(in, inOff, inLen, macData, V, fillKDFunction)
  }
}
