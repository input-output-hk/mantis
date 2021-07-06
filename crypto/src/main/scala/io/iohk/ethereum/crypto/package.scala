package io.iohk.ethereum

import java.nio.charset.StandardCharsets
import java.security.SecureRandom

import akka.util.ByteString

import org.bouncycastle.asn1.sec.SECNamedCurves
import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.digests.KeccakDigest
import org.bouncycastle.crypto.digests.RIPEMD160Digest
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.generators.ECKeyPairGenerator
import org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator
import org.bouncycastle.crypto.generators.SCrypt
import org.bouncycastle.crypto.params._
import org.bouncycastle.util.encoders.Hex

import io.iohk.ethereum.utils.ByteUtils

package object crypto {

  val curveParams: X9ECParameters = SECNamedCurves.getByName("secp256k1")
  val curve: ECDomainParameters =
    new ECDomainParameters(curveParams.getCurve, curveParams.getG, curveParams.getN, curveParams.getH)

  private val keccakSize = 512
  val kec512 = new KeccakDigest(keccakSize)

  def kec256(input: Array[Byte], start: Int, length: Int): Array[Byte] = {
    val digest = new KeccakDigest(256)
    val output = Array.ofDim[Byte](digest.getDigestSize)
    digest.update(input, start, length)
    digest.doFinal(output, 0)
    output
  }

  def kec256(input: Array[Byte]*): Array[Byte] = {
    val digest = new KeccakDigest(256)
    val output = Array.ofDim[Byte](digest.getDigestSize)
    input.foreach(i => digest.update(i, 0, i.length))
    digest.doFinal(output, 0)
    output
  }

  def kec256(input: ByteString): ByteString =
    ByteString(kec256(input.toArray))

  def kec256PoW(header: Array[Byte], nonce: Array[Byte]): Array[Byte] = {
    val digest = new KeccakDigest(256)
    digest.update(header, 0, header.length)
    digest.update(nonce, 0, nonce.length)
    val output = Array.ofDim[Byte](32)
    digest.doFinal(output, 0)
    output
  }

  def kec512(input: Array[Byte]): Array[Byte] = synchronized {
    val out = Array.ofDim[Byte](kec512.getDigestSize)
    kec512.update(input, 0, input.length)
    kec512.doFinal(out, 0)
    out
  }

  def generateKeyPair(secureRandom: SecureRandom): AsymmetricCipherKeyPair = {
    val generator = new ECKeyPairGenerator
    generator.init(new ECKeyGenerationParameters(curve, secureRandom))
    generator.generateKeyPair()
  }

  def secureRandomByteString(secureRandom: SecureRandom, length: Int): ByteString =
    ByteString(secureRandomByteArray(secureRandom, length))

  def secureRandomByteArray(secureRandom: SecureRandom, length: Int): Array[Byte] = {
    val bytes = Array.ofDim[Byte](length)
    secureRandom.nextBytes(bytes)
    bytes
  }

  /** @return (privateKey, publicKey) pair.
    * The public key will be uncompressed and have its prefix dropped.
    */
  def keyPairToByteArrays(keyPair: AsymmetricCipherKeyPair): (Array[Byte], Array[Byte]) = {
    val prvKey = ByteUtils.bigIntegerToBytes(keyPair.getPrivate.asInstanceOf[ECPrivateKeyParameters].getD, 32)
    val pubKey = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail
    (prvKey, pubKey)
  }

  def keyPairToByteStrings(keyPair: AsymmetricCipherKeyPair): (ByteString, ByteString) = {
    val (prv, pub) = keyPairToByteArrays(keyPair)
    (ByteString(prv), ByteString(pub))
  }

  def keyPairFromPrvKey(prvKeyBytes: Array[Byte]): AsymmetricCipherKeyPair = {
    val privateKey = BigInt(1, prvKeyBytes)
    keyPairFromPrvKey(privateKey)
  }

  def keyPairFromPrvKey(prvKeyBytes: ByteString): AsymmetricCipherKeyPair = {
    val privateKey = BigInt(1, prvKeyBytes.toArray)
    keyPairFromPrvKey(privateKey)
  }

  def keyPairFromPrvKey(prvKey: BigInt): AsymmetricCipherKeyPair = {
    val publicKey = curve.getG.multiply(prvKey.bigInteger).normalize()
    new AsymmetricCipherKeyPair(
      new ECPublicKeyParameters(publicKey, curve),
      new ECPrivateKeyParameters(prvKey.bigInteger, curve)
    )
  }

  def pubKeyFromKeyPair(keypair: AsymmetricCipherKeyPair): Array[Byte] =
    keyPairToByteArrays(keypair)._2

  def pubKeyFromPrvKey(prvKey: Array[Byte]): Array[Byte] =
    keyPairToByteArrays(keyPairFromPrvKey(prvKey))._2

  def pubKeyFromPrvKey(prvKey: ByteString): ByteString =
    ByteString(pubKeyFromPrvKey(prvKey.toArray))

  def newRandomKeyPairAsStrings(secureRandom: SecureRandom = new SecureRandom): (String, String) = {
    val keyPair = generateKeyPair(secureRandom)
    val (prv, pub) = keyPairToByteArrays(keyPair)
    (Hex.toHexString(prv), Hex.toHexString(pub))
  }

  def ripemd160(input: Array[Byte]): Array[Byte] = {
    val digest = new RIPEMD160Digest
    digest.update(input, 0, input.length)
    val out = Array.ofDim[Byte](digest.getDigestSize)
    digest.doFinal(out, 0)
    out
  }

  def ripemd160(input: ByteString): ByteString =
    ByteString(ripemd160(input.toArray))

  def sha256(input: Array[Byte]): Array[Byte] = {
    val digest = new SHA256Digest()
    val out = Array.ofDim[Byte](digest.getDigestSize)
    digest.update(input, 0, input.size)
    digest.doFinal(out, 0)
    out
  }

  def sha256(input: ByteString): ByteString =
    ByteString(sha256(input.toArray))

  def pbkdf2HMacSha256(passphrase: String, salt: ByteString, c: Int, dklen: Int): ByteString = {
    val generator = new PKCS5S2ParametersGenerator(new SHA256Digest())
    generator.init(passphrase.getBytes(StandardCharsets.UTF_8), salt.toArray, c)
    val key = generator.generateDerivedMacParameters(dklen * 8).asInstanceOf[KeyParameter]
    ByteString(key.getKey)
  }

  def scrypt(passphrase: String, salt: ByteString, n: Int, r: Int, p: Int, dklen: Int): ByteString = {
    val key = SCrypt.generate(passphrase.getBytes(StandardCharsets.UTF_8), salt.toArray, n, r, p, dklen)
    ByteString(key)
  }
}
