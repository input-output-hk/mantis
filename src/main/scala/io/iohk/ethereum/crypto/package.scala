package io.iohk.ethereum

import java.nio.charset.StandardCharsets
import java.security.SecureRandom

import akka.util.ByteString
import fr.cryptohash.{Keccak256, Keccak512}
import org.spongycastle.asn1.sec.SECNamedCurves
import org.spongycastle.asn1.x9.X9ECParameters
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.digests.{RIPEMD160Digest, SHA256Digest}
import org.spongycastle.crypto.generators.{ECKeyPairGenerator, PKCS5S2ParametersGenerator, SCrypt}
import org.spongycastle.crypto.params._

package object crypto {

  val curveParams: X9ECParameters = SECNamedCurves.getByName("secp256k1")
  val curve: ECDomainParameters = new ECDomainParameters(curveParams.getCurve, curveParams.getG, curveParams.getN, curveParams.getH)

  def kec256(input: Array[Byte], start: Int, length: Int): Array[Byte] = {
    val digest = new Keccak256
    digest.update(input, start, length)
    digest.digest
  }

  def kec256(input: Array[Byte]*): Array[Byte] = {
    val digest: Keccak256 = new Keccak256
    input.foreach(i => digest.update(i))
    digest.digest
  }

  def kec256(input: ByteString): ByteString =
    ByteString(kec256(input.toArray))

  def kec512(input: Array[Byte]*): Array[Byte] = {
    val digest = new Keccak512
    input.foreach(i => digest.update(i))
    digest.digest
  }

  def generateKeyPair(secureRandom: SecureRandom): AsymmetricCipherKeyPair = {
    val generator = new ECKeyPairGenerator
    generator.init(new ECKeyGenerationParameters(curve, secureRandom))
    generator.generateKeyPair()
  }

  /** @return (privateKey, publicKey) pair */
  def keyPairToByteArrays(keyPair: AsymmetricCipherKeyPair): (Array[Byte], Array[Byte]) = {
    val prvKey = keyPair.getPrivate.asInstanceOf[ECPrivateKeyParameters].getD.toByteArray
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

  def keyPairFromPrvKey(prvKey: BigInt): AsymmetricCipherKeyPair = {
    val publicKey = curve.getG.multiply(prvKey.bigInteger).normalize()
    new AsymmetricCipherKeyPair(new ECPublicKeyParameters(publicKey, curve), new ECPrivateKeyParameters(prvKey.bigInteger, curve))
  }

  def pubKeyFromPrvKey(prvKey: Array[Byte]): Array[Byte] =
    keyPairToByteArrays(keyPairFromPrvKey(prvKey))._2


  def pubKeyFromPrvKey(prvKey: ByteString): ByteString =
    ByteString(pubKeyFromPrvKey(prvKey.toArray))

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
