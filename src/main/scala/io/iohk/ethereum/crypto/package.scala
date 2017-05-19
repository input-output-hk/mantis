package io.iohk.ethereum

import java.security.{MessageDigest, SecureRandom}

import akka.util.ByteString
import fr.cryptohash.{Keccak256, Keccak512}
import org.spongycastle.asn1.sec.SECNamedCurves
import org.spongycastle.asn1.x9.X9ECParameters
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.digests.RIPEMD160Digest
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params.{ECDomainParameters, ECKeyGenerationParameters, ECPrivateKeyParameters, ECPublicKeyParameters}

package object crypto {

  val curveParams: X9ECParameters = SECNamedCurves.getByName("secp256k1")
  val curve: ECDomainParameters = new ECDomainParameters(curveParams.getCurve, curveParams.getG, curveParams.getN, curveParams.getH)

  val sha256Digest = MessageDigest.getInstance("SHA-256")

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

  def generateKeyPair(secureRandom: SecureRandom = new SecureRandom): AsymmetricCipherKeyPair = {
    val generator = new ECKeyPairGenerator
    generator.init(new ECKeyGenerationParameters(curve, secureRandom))
    generator.generateKeyPair()
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

  def sha256(input: Array[Byte]): Array[Byte] =
    sha256Digest.digest(input)

  def sha256(input: ByteString): ByteString =
    ByteString(sha256(input.toArray))

  def getKeyPair(prvKey: BigInt): AsymmetricCipherKeyPair = {
    val publicKey = curve.getG.multiply(prvKey.bigInteger).normalize()
    new AsymmetricCipherKeyPair(new ECPublicKeyParameters(publicKey, curve), new ECPrivateKeyParameters(prvKey.bigInteger, curve))
  }
}
