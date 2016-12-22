package io.iohk.ethereum.crypto

import java.io.ByteArrayInputStream

import akka.util.ByteString
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
  * @param kdf        the key derivation function used for byte generation
  * @param mac        the message authentication code generator for the message
  * @param hash       hash ing function
  * @param cipher     the actual cipher
  * @param IV         vector with random values used to initialize cipher
  * @param prvSrc     private key source
  * @param pubSrc     public key source
  * @param hashMacKey determines if for mac use kdf value (if false) or hashed kdf value (if true)
  */
class EthereumIESEngine(kdf: Either[ConcatKDFBytesGenerator, MGF1BytesGeneratorExt],
                        mac: Mac,
                        hash: Digest,
                        cipher: Option[BufferedBlockCipher],
                        IV: Array[Byte],
                        prvSrc: Either[ECPrivateKeyParameters, EphemeralKeyPairGenerator],
                        pubSrc: Either[ECPublicKeyParameters, ECIESPublicKeyParser],
                        hashMacKey: Boolean = true) {

  @throws[InvalidCipherTextException]
  private def encryptBlock(plainText: Array[Byte], inOff: Int, inLen: Int, macData: Option[Array[Byte]], encodedPublicKey: Array[Byte], fillKDFunction: Int => ByteString): Array[Byte] = {
    val (derivedKeySecondPart, cryptogram) = cipher match {

      case Some(cphr) =>
        // Block cipher mode.
        val derivedKey = fillKDFunction(ECIESCoder.KEY_SIZE / 8 + ECIESCoder.KEY_SIZE / 8)
        val (firstPart, secondPart) = derivedKey.splitAt(ECIESCoder.KEY_SIZE / 8)

        if (IV != null)
          cphr.init(true, new ParametersWithIV(new KeyParameter(firstPart.toArray), IV))
        else
          cphr.init(true, new KeyParameter(firstPart.toArray))

        val encrypted = new Array[Byte](cphr.getOutputSize(inLen))
        val len = cphr.processBytes(plainText, inOff, inLen, encrypted, 0)
        cphr.doFinal(encrypted, len)

        (secondPart, ByteString(encrypted))

      case None =>
        // Streaming mode.
        val derivedKey = fillKDFunction(inLen + ECIESCoder.KEY_SIZE / 8)
        val (firstPart, secondPart) = derivedKey.splitAt(inLen)

        val encrypted: Seq[Byte] = firstPart.zipWithIndex.map { case (value, idx) =>
          (plainText(inOff + idx) ^ value).toByte
        }

        (secondPart, ByteString(encrypted: _*))
    }

    val kdfPartForMac: Array[Byte] = if (hashMacKey) {
      val hashBuff = new Array[Byte](hash.getDigestSize)
      hash.reset()
      hash.update(derivedKeySecondPart.toArray, 0, derivedKeySecondPart.length)
      hash.doFinal(hashBuff, 0)
      hashBuff
    } else {
      derivedKeySecondPart.toArray
    }

    // calculate mac
    mac.init(new KeyParameter(kdfPartForMac))
    mac.update(IV, 0, IV.length)
    mac.update(cryptogram.toArray, 0, cryptogram.length)

    macData.foreach(data => mac.update(data, 0, data.length))

    val messageAuthenticationCode = new Array[Byte](mac.getMacSize)
    mac.doFinal(messageAuthenticationCode, 0)

    encodedPublicKey ++ cryptogram ++ messageAuthenticationCode
  }

  @throws[InvalidCipherTextException]
  private def decryptBlock(cryptogram: Array[Byte], inOff: Int, inLen: Int, macData: Option[Array[Byte]], V: Array[Byte], fillKDFunction: Int => ByteString): Array[Byte] = {
    // Ensure that the length of the input is greater than the MAC in bytes
    if (inLen <= (ECIESCoder.KEY_SIZE / 8)) throw new InvalidCipherTextException("Length of input must be greater than the MAC")

    val (derivedKeySecondPart, plainText) = cipher match {
      case Some(cphr) =>
        // Block cipher mode.
        val derivedKey: ByteString = fillKDFunction(ECIESCoder.KEY_SIZE / 8 + ECIESCoder.KEY_SIZE / 8)
        val (firstPart, secondPart) = derivedKey.splitAt(ECIESCoder.KEY_SIZE / 8)

        if (IV != null)
          cphr.init(false, new ParametersWithIV(new KeyParameter(firstPart.toArray), IV))
        else
          cphr.init(false, new KeyParameter(firstPart.toArray))

        val decrypted = new Array[Byte](cphr.getOutputSize(inLen - V.length - mac.getMacSize))
        val len = cphr.processBytes(cryptogram, inOff + V.length, inLen - V.length - mac.getMacSize, decrypted, 0)
        cphr.doFinal(decrypted, len)

        (secondPart, ByteString(decrypted))
      case None =>
        // Streaming mode.
        val derivedKey = fillKDFunction((inLen - V.length - mac.getMacSize) + (ECIESCoder.KEY_SIZE / 8))
        val (firstPart, secondPart) = derivedKey.splitAt(inLen - V.length - mac.getMacSize)

        val decrypted: Seq[Byte] = firstPart.zipWithIndex.map { case (value, idx) =>
          (cryptogram(inOff + V.length + idx) ^ value).toByte
        }

        (secondPart, ByteString(decrypted: _*))
    }

    val kdfPartForMac: Array[Byte] = if (hashMacKey) {
      val hashBuff = new Array[Byte](hash.getDigestSize)
      hash.reset()
      hash.update(derivedKeySecondPart.toArray, 0, derivedKeySecondPart.length)
      hash.doFinal(hashBuff, 0)
      hashBuff
    } else {
      derivedKeySecondPart.toArray
    }

    // Verify the MAC.
    val end = inOff + inLen
    val messageAuthenticationCode = Arrays.copyOfRange(cryptogram, end - mac.getMacSize, end)
    val messageAuthenticationCodeCalculated = new Array[Byte](messageAuthenticationCode.length)

    mac.init(new KeyParameter(kdfPartForMac))
    mac.update(IV, 0, IV.length)
    mac.update(cryptogram, inOff + V.length, inLen - V.length - messageAuthenticationCodeCalculated.length)

    if (macData.isDefined)
      mac.update(macData.get, 0, macData.get.length)

    mac.doFinal(messageAuthenticationCodeCalculated, 0)

    if (!Arrays.constantTimeAreEqual(messageAuthenticationCode, messageAuthenticationCodeCalculated))
      throw new InvalidCipherTextException("Invalid MAC.")

    plainText.toArray
  }

  @throws[InvalidCipherTextException]
  def processBlock(in: Array[Byte], inOff: Int, inLen: Int, forEncryption: Boolean, macData: Option[Array[Byte]] = None): Array[Byte] = {
    var encodedPublicKey: Array[Byte] = new Array[Byte](0)

    val prv = prvSrc.fold(
      key => key,
      keyPairGenerator => {
        val ephKeyPair = keyPairGenerator.generate
        encodedPublicKey = ephKeyPair.getEncodedPublicKey
        ephKeyPair.getKeyPair.getPrivate.asInstanceOf[ECPrivateKeyParameters]
      })

    val pub = pubSrc.fold(
      key => key,
      keyParser => {
        val bIn = new ByteArrayInputStream(in, inOff, inLen)
        val result = keyParser.readKey(bIn).asInstanceOf[ECPublicKeyParameters]
        val encLength = inLen - bIn.available
        encodedPublicKey = Arrays.copyOfRange(in, inOff, inOff + encLength)
        result
      }
    )
    val agree = new ECDHBasicAgreement
    agree.init(prv)
    val z = agree.calculateAgreement(pub)
    val VZ = BigIntegers.asUnsignedByteArray(agree.getFieldSize, z)

    val fillKDFunction = (outLen: Int) => kdf.fold(_.generateBytes(outLen, VZ), _.generateBytes(outLen, VZ))

    if (forEncryption) encryptBlock(in, inOff, inLen, macData, encodedPublicKey, fillKDFunction)
    else decryptBlock(in, inOff, inLen, macData, encodedPublicKey, fillKDFunction)
  }
}
