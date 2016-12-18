package io.iohk.ethereum.crypto

import java.io.{ByteArrayInputStream, IOException}

import org.spongycastle.crypto._
import org.spongycastle.crypto.generators.EphemeralKeyPairGenerator
import org.spongycastle.crypto.params._
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
  * @param agree  the key agreement used as the basis for the encryption
  * @param kdf    the key derivation function used for byte generation
  * @param mac    the message authentication code generator for the message
  * @param hash   hash ing function
  * @param cipher the actual cipher
  */
class EthereumIESEngine(var agree: BasicAgreement, var kdf: DerivationFunction, var mac: Mac, val hash: Digest, var cipher: BufferedBlockCipher) {
  private[crypto] var macBuf: Array[Byte] = new Array[Byte](mac.getMacSize)
  private[crypto] var forEncryption = false
  private[crypto] var privParam: CipherParameters = null
  private[crypto] var pubParam: CipherParameters = null
  private[crypto] var param: IESParameters = null
  private[crypto] var V: Array[Byte] = null
  private var keyPairGenerator: EphemeralKeyPairGenerator = null
  private var keyParser: KeyParser = null
  private var IV: Array[Byte] = null
  private[crypto] var hashK2 = true

  def setHashMacKey(hashK2: Boolean) {
    this.hashK2 = hashK2
  }

  /**
    * Initialise the encryptor.
    *
    * @param forEncryption whether or not this is encryption/decryption.
    * @param privParam     our private key parameters
    * @param pubParam      the recipient's/sender's public key parameters
    * @param params        encoding and derivation parameters, may be wrapped to include an IV for an underlying block cipher.
    */
  def init(forEncryption: Boolean, privParam: CipherParameters, pubParam: CipherParameters, params: CipherParameters) {
    this.forEncryption = forEncryption
    this.privParam = privParam
    this.pubParam = pubParam
    this.V = new Array[Byte](0)
    extractParams(params)
  }

  /**
    * Initialise the encryptor.
    *
    * @param publicKey                 the recipient's/sender's public key parameters
    * @param params                    encoding and derivation parameters, may be wrapped to include an IV for an underlying block cipher.
    * @param ephemeralKeyPairGenerator the ephemeral key pair generator to use.
    */
  def init(publicKey: AsymmetricKeyParameter, params: CipherParameters, ephemeralKeyPairGenerator: EphemeralKeyPairGenerator) {
    this.forEncryption = true
    this.pubParam = publicKey
    this.keyPairGenerator = ephemeralKeyPairGenerator
    extractParams(params)
  }

  /**
    * Initialise the encryptor.
    *
    * @param privateKey      the recipient's private key.
    * @param params          encoding and derivation parameters, may be wrapped to include an IV for an underlying block cipher.
    * @param publicKeyParser the parser for reading the ephemeral public key.
    */
  def init(privateKey: AsymmetricKeyParameter, params: CipherParameters, publicKeyParser: KeyParser) {
    this.forEncryption = false
    this.privParam = privateKey
    this.keyParser = publicKeyParser
    extractParams(params)
  }

  private def extractParams(params: CipherParameters) {
    if (params.isInstanceOf[ParametersWithIV]) {
      this.IV = params.asInstanceOf[ParametersWithIV].getIV
      this.param = params.asInstanceOf[ParametersWithIV].getParameters.asInstanceOf[IESParameters]
    }
    else {
      this.IV = null
      this.param = params.asInstanceOf[IESParameters]
    }
  }

  def getCipher: BufferedBlockCipher = cipher

  def getMac: Mac = mac

  @throws[InvalidCipherTextException]
  private def encryptBlock(in: Array[Byte], inOff: Int, inLen: Int, macData: Option[Array[Byte]]) = {
    var C: Array[Byte] = null
    var K: Array[Byte] = null
    var K1: Array[Byte] = null
    var K2: Array[Byte] = null
    var len = 0
    if (cipher == null) {
      // used only for whispers
      // Streaming mode.
      K1 = new Array[Byte](inLen)
      K2 = new Array[Byte](param.getMacKeySize / 8)
      K = new Array[Byte](K1.length + K2.length)
      kdf.generateBytes(K, 0, K.length)

      System.arraycopy(K, 0, K1, 0, K1.length)
      System.arraycopy(K, inLen, K2, 0, K2.length)

      C = new Array[Byte](inLen)
      var i = 0
      while (i != inLen) {
        {
          C(i) = (in(inOff + i) ^ K1(i)).toByte
        }
        {
          i += 1;
          i - 1
        }
      }
      len = inLen
    }
    else {
      // Block cipher mode.
      K1 = new Array[Byte](param.asInstanceOf[IESWithCipherParameters].getCipherKeySize / 8)
      K2 = new Array[Byte](param.getMacKeySize / 8)
      K = new Array[Byte](K1.length + K2.length)
      kdf.generateBytes(K, 0, K.length)
      System.arraycopy(K, 0, K1, 0, K1.length)
      System.arraycopy(K, K1.length, K2, 0, K2.length)
      // If iv provided use it to initialise the cipher
      if (IV != null) cipher.init(true, new ParametersWithIV(new KeyParameter(K1), IV))
      else cipher.init(true, new KeyParameter(K1))
      C = new Array[Byte](cipher.getOutputSize(inLen))
      len = cipher.processBytes(in, inOff, inLen, C, 0)
      len += cipher.doFinal(C, len)
    }
    // Convert the length of the encoding vector into a byte array.
    val P2 = param.getEncodingV
    // Apply the MAC.
    val T = new Array[Byte](mac.getMacSize)
    var K2a: Array[Byte] = null
    if (hashK2) {
      K2a = new Array[Byte](hash.getDigestSize)
      hash.reset()
      hash.update(K2, 0, K2.length)
      hash.doFinal(K2a, 0)
    }
    else K2a = K2
    mac.init(new KeyParameter(K2a))
    mac.update(IV, 0, IV.length)
    mac.update(C, 0, C.length)
    if (P2 != null) mac.update(P2, 0, P2.length)
    if (V.length != 0 && P2 != null) {
      val L2 = new Array[Byte](4)
      Pack.intToBigEndian(P2.length * 8, L2, 0)
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
  private def decryptBlock(in_enc: Array[Byte], inOff: Int, inLen: Int, macData: Option[Array[Byte]]) = {
    var M: Array[Byte] = null
    var K: Array[Byte] = null
    var K1: Array[Byte] = null
    var K2: Array[Byte] = null
    var len = 0
    // Ensure that the length of the input is greater than the MAC in bytes
    if (inLen <= (param.getMacKeySize / 8)) throw new InvalidCipherTextException("Length of input must be greater than the MAC")
    if (cipher == null) {
      // used only for whispers
      // Streaming mode.
      K1 = new Array[Byte](inLen - V.length - mac.getMacSize)
      K2 = new Array[Byte](param.getMacKeySize / 8)
      K = new Array[Byte](K1.length + K2.length)
      kdf.generateBytes(K, 0, K.length)


      System.arraycopy(K, 0, K1, 0, K1.length)
      System.arraycopy(K, K1.length, K2, 0, K2.length)

      M = new Array[Byte](K1.length)
      var i = 0
      while (i != K1.length) {
        {
          M(i) = (in_enc(inOff + V.length + i) ^ K1(i)).toByte
        }
        {
          i += 1;
          i - 1
        }
      }
      len = K1.length
    }
    else {
      // Block cipher mode.
      K1 = new Array[Byte](param.asInstanceOf[IESWithCipherParameters].getCipherKeySize / 8)
      K2 = new Array[Byte](param.getMacKeySize / 8)
      K = new Array[Byte](K1.length + K2.length)
      kdf.generateBytes(K, 0, K.length)
      System.arraycopy(K, 0, K1, 0, K1.length)
      System.arraycopy(K, K1.length, K2, 0, K2.length)
      // If IV provide use it to initialize the cipher
      if (IV != null) cipher.init(false, new ParametersWithIV(new KeyParameter(K1), IV))
      else cipher.init(false, new KeyParameter(K1))
      M = new Array[Byte](cipher.getOutputSize(inLen - V.length - mac.getMacSize))
      len = cipher.processBytes(in_enc, inOff + V.length, inLen - V.length - mac.getMacSize, M, 0)
      len += cipher.doFinal(M, len)
    }
    // Convert the length of the encoding vector into a byte array.
    val P2 = param.getEncodingV
    // Verify the MAC.
    val end = inOff + inLen
    val T1 = Arrays.copyOfRange(in_enc, end - mac.getMacSize, end)
    val T2 = new Array[Byte](T1.length)
    var K2a: Array[Byte] = null
    if (hashK2) {
      K2a = new Array[Byte](hash.getDigestSize)
      hash.reset()
      hash.update(K2, 0, K2.length)
      hash.doFinal(K2a, 0)
    }
    else K2a = K2
    mac.init(new KeyParameter(K2a))
    mac.update(IV, 0, IV.length)
    mac.update(in_enc, inOff + V.length, inLen - V.length - T2.length)
    if (P2 != null) mac.update(P2, 0, P2.length)
    if (V.length != 0 && P2 != null) {
      val L2 = new Array[Byte](4)
      Pack.intToBigEndian(P2.length * 8, L2, 0)
      mac.update(L2, 0, L2.length)
    }
    if (macData.isDefined) mac.update(macData.get, 0, macData.get.length)
    mac.doFinal(T2, 0)
    if (!Arrays.constantTimeAreEqual(T1, T2)) throw new InvalidCipherTextException("Invalid MAC.")
    // Output the message.
    Arrays.copyOfRange(M, 0, len)
  }

  @throws[InvalidCipherTextException]
  def processBlock(in: Array[Byte], inOff: Int, inLen: Int, macData: Option[Array[Byte]] = None): Array[Byte] = {
    if (forEncryption) if (keyPairGenerator != null) {
      val ephKeyPair = keyPairGenerator.generate
      this.privParam = ephKeyPair.getKeyPair.getPrivate
      this.V = ephKeyPair.getEncodedPublicKey
    }
    else if (keyParser != null) {
      val bIn = new ByteArrayInputStream(in, inOff, inLen)
      try
        this.pubParam = keyParser.readKey(bIn)

      catch {
        case e: IOException => {
          throw new InvalidCipherTextException("unable to recover ephemeral public key: " + e.getMessage, e)
        }
      }
      val encLength = inLen - bIn.available
      this.V = Arrays.copyOfRange(in, inOff, inOff + encLength)
    }
    // Compute the common value and convert to byte array.
    agree.init(privParam)
    val z = agree.calculateAgreement(pubParam)
    val Z = BigIntegers.asUnsignedByteArray(agree.getFieldSize, z)
    // Create input to KDF.
    var VZ: Array[Byte] = null

    VZ = Z

    // Initialise the KDF.
    var kdfParam: DerivationParameters = null

    if (kdf.isInstanceOf[MGF1BytesGeneratorExt])
      kdfParam = new MGFParameters(VZ)
    else
      kdfParam = new KDFParameters(VZ, param.getDerivationV)

    kdf.init(kdfParam)
    if (forEncryption) encryptBlock(in, inOff, inLen, macData)
    else decryptBlock(in, inOff, inLen, macData)
  }
}
