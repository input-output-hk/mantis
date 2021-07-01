package io.iohk.ethereum.network.discovery

import akka.util.ByteString

import scala.collection.concurrent.TrieMap

import io.iohk.scalanet.discovery.crypto.PrivateKey
import io.iohk.scalanet.discovery.crypto.PublicKey
import io.iohk.scalanet.discovery.crypto.SigAlg
import io.iohk.scalanet.discovery.crypto.Signature
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import scodec.Attempt
import scodec.Err
import scodec.bits.BitVector

import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.security.SecureRandomBuilder

class Secp256k1SigAlg extends SigAlg with SecureRandomBuilder {
  // We'll be using the same private key over and over to sign messages.
  // To save the time transforming it into a public-private key pair every time, store the results.
  // In the future we might want to not pass around the private key but have it as a constructor argument.
  private val signingKeyPairCache = TrieMap.empty[PrivateKey, AsymmetricCipherKeyPair]

  override val name = "secp256k1"

  override val PrivateKeyBytesSize = 32

  // A Secp256k1 public key is 32 bytes compressed or 64 bytes uncompressed,
  // with a 1 byte prefix showing which version it is.
  // See https://davidederosa.com/basic-blockchain-programming/elliptic-curve-keys
  //
  // However in the discovery v4 protocol the prefix is omitted.
  override val PublicKeyBytesSize = 64

  // A normal Secp256k1 signature consists of 2 bigints `r` and `s` followed by a recovery ID `v`,
  // but it can be just 64 bytes if that's omitted, like in the ENR.
  override val SignatureBytesSize = 65

  val SignatureWithoutRecoveryBytesSize = 64
  val PublicKeyCompressedBytesSize = 33

  override def newKeyPair: (PublicKey, PrivateKey) = {
    val keyPair = crypto.generateKeyPair(secureRandom)
    val (privateKeyBytes, publicKeyBytes) = crypto.keyPairToByteArrays(keyPair)

    val publicKey = toPublicKey(publicKeyBytes)
    val privateKey = toPrivateKey(privateKeyBytes)

    publicKey -> privateKey
  }

  override def sign(privateKey: PrivateKey, data: BitVector): Signature = {
    val message = crypto.kec256(data.toByteArray)
    val keyPair = signingKeyPairCache.getOrElseUpdate(privateKey, crypto.keyPairFromPrvKey(privateKey.toByteArray))
    val sig = ECDSASignature.sign(message, keyPair)
    toSignature(sig)
  }

  // ENR wants the signature without recovery ID, just 64 bytes.
  // The Packet on the other hand has the full 65 bytes.
  override def removeRecoveryId(signature: Signature): Signature =
    signature.size / 8 match {
      case SignatureBytesSize =>
        Signature(signature.dropRight(8))
      case SignatureWithoutRecoveryBytesSize =>
        signature
      case other =>
        throw new IllegalArgumentException(s"Unexpected signature size: $other bytes")
    }

  override def compressPublicKey(publicKey: PublicKey): PublicKey =
    publicKey.size / 8 match {
      case PublicKeyBytesSize =>
        // This is a public key without the prefix, it consists of an x and y bigint.
        // To compress we drop y, and the first byte becomes 02 for even values of y and 03 for odd values.
        val point = crypto.curve.getCurve.decodePoint(ECDSASignature.UncompressedIndicator +: publicKey.toByteArray)
        val key = new ECPublicKeyParameters(point, crypto.curve)
        val bytes = key.getQ.getEncoded(true) // compressed encoding
        val compressed = PublicKey(BitVector(bytes))
        assert(compressed.size == PublicKeyCompressedBytesSize * 8)
        compressed

      case PublicKeyCompressedBytesSize =>
        publicKey

      case other =>
        throw new IllegalArgumentException(s"Unexpected uncompressed public key size: $other bytes")
    }

  // The public key points lie on the curve `y^2 = x^3 + 7`.
  // In the compressed form we have x and a prefix telling us whether y is even or odd.
  // https://bitcoin.stackexchange.com/questions/86234/how-to-uncompress-a-public-key
  // https://bitcoin.stackexchange.com/questions/44024/get-uncompressed-public-key-from-compressed-form
  def decompressPublicKey(publicKey: PublicKey): PublicKey =
    publicKey.size / 8 match {
      case PublicKeyBytesSize =>
        publicKey

      case PublicKeyCompressedBytesSize =>
        val point = crypto.curve.getCurve.decodePoint(publicKey.toByteArray)
        val key = new ECPublicKeyParameters(point, crypto.curve)
        val bytes = key.getQ.getEncoded(false).drop(1) // uncompressed encoding, drop prefix.
        toPublicKey(bytes)

      case other =>
        throw new IllegalArgumentException(s"Unexpected compressed public key size: $other bytes")
    }

  override def verify(publicKey: PublicKey, signature: Signature, data: BitVector): Boolean = {
    val message = crypto.kec256(data.toByteArray)
    val uncompressedPublicKey = decompressPublicKey(publicKey)
    toECDSASignatures(signature).exists { sig =>
      sig.publicKey(message).map(toPublicKey).contains(uncompressedPublicKey)
    }
  }

  override def recoverPublicKey(signature: Signature, data: BitVector): Attempt[PublicKey] = {
    val message = crypto.kec256(data.toByteArray)

    val maybePublicKey = toECDSASignatures(signature).flatMap { sig =>
      sig.publicKey(message).map(toPublicKey)
    }.headOption

    Attempt.fromOption(maybePublicKey, Err("Failed to recover the public key from the signature."))
  }

  override def toPublicKey(privateKey: PrivateKey): PublicKey = {
    val publicKeyBytes = crypto.pubKeyFromPrvKey(privateKey.toByteArray)
    toPublicKey(publicKeyBytes)
  }

  private def toPublicKey(publicKeyBytes: Array[Byte]): PublicKey = {
    // Discovery uses 64 byte keys, without the prefix.
    val publicKey = PublicKey(BitVector(publicKeyBytes))
    assert(publicKey.size == PublicKeyBytesSize * 8, s"Unexpected public key size: ${publicKey.size / 8} bytes")
    publicKey
  }

  private def toPrivateKey(privateKeyBytes: Array[Byte]): PrivateKey = {
    val privateKey = PrivateKey(BitVector(privateKeyBytes))
    assert(privateKey.size == PrivateKeyBytesSize * 8, s"Unexpected private key size: ${privateKey.size / 8} bytes")
    privateKey
  }

  // Apparently the `v` has to adjusted by 27, which is the negative point sign.
  private def vToWire(v: Byte): Byte =
    (v - ECDSASignature.negativePointSign).toByte

  private def wireToV(w: Byte): Byte =
    (w + ECDSASignature.negativePointSign).toByte

  private def adjustV(bytes: Array[Byte], f: Byte => Byte): Unit =
    bytes(bytes.size - 1) = f(bytes(bytes.size - 1))

  private def toSignature(sig: ECDSASignature): Signature = {
    val signatureBytes = sig.toBytes.toArray[Byte]
    assert(signatureBytes.size == SignatureBytesSize)
    adjustV(signatureBytes, vToWire)
    Signature(BitVector(signatureBytes))
  }

  // Based on whether we have the recovery ID in the signature we may have to try 1 or 2 signatures.
  private def toECDSASignatures(signature: Signature): Iterable[ECDSASignature] =
    signature.size / 8 match {
      case SignatureBytesSize =>
        val signatureBytes = signature.toByteArray
        adjustV(signatureBytes, wireToV)
        Iterable(toECDSASignature(signatureBytes))

      case SignatureWithoutRecoveryBytesSize =>
        val signatureBytes = signature.toByteArray
        // Try all allowed points signs.
        ECDSASignature.allowedPointSigns.toIterable.map { v =>
          toECDSASignature(signatureBytes :+ v)
        }

      case other =>
        throw new IllegalArgumentException(s"Unexpected signature size: $other bytes")
    }

  private def toECDSASignature(signatureBytes: Array[Byte]): ECDSASignature =
    ECDSASignature.fromBytes(ByteString(signatureBytes)).getOrElse {
      throw new IllegalArgumentException(s"Could not convert to ECDSA signature.")
    }
}
