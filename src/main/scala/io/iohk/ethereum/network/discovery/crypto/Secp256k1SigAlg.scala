package io.iohk.ethereum.network.discovery.crypto

import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.scalanet.discovery.crypto.{SigAlg, PublicKey, PrivateKey, Signature}
import scodec.bits.BitVector
import scodec.{Attempt, Err}
import scodec.bits.BitVector
import akka.util.ByteString

object Secp256k1SigAlg extends SigAlg with SecureRandomBuilder {
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
    val keyPair = crypto.keyPairFromPrvKey(privateKey.toByteArray)
    val sig = ECDSASignature.sign(message, keyPair)
    toSignature(sig)
  }

  // ENR wants the signature without recovery ID, just 64 bytes.
  // The Packet on the other hand has the full 65 bytes.
  override def removeRecoveryId(signature: Signature): Signature = {
    signature.size / 8 match {
      case SignatureBytesSize =>
        Signature(signature.dropRight(8))
      case SignatureWithoutRecoveryBytesSize =>
        signature
      case other =>
        throw new IllegalArgumentException(s"Unexpected signature size: $other bytes")
    }
  }

  override def compressPublicKey(publicKey: PublicKey): PublicKey = {
    publicKey.size / 8 match {
      case PublicKeyBytesSize =>
        // This is a public key without the prefix, it consists of an x and y bigint.
        // To compress we drop y, and the first byte becomes 02 for even values of y and 03 for odd values.
        val (xbs, ybs) = publicKey.splitAt(publicKey.length / 2)
        val y = BigInt(1, ybs.toByteArray)
        val prefix: Byte =
          if (y.mod(2) == 0) ECDSASignature.CompressedEvenIndicator
          else ECDSASignature.CompressedOddIndicator
        val compressed = PublicKey(BitVector(prefix) ++ xbs)
        assert(compressed.size == PublicKeyCompressedBytesSize * 8)
        compressed

      case PublicKeyCompressedBytesSize =>
        publicKey

      case other =>
        throw new IllegalArgumentException(s"Unexpected public key size: $other bytes")
    }
  }

  override def verify(publicKey: PublicKey, signature: Signature, data: BitVector): Boolean = {
    val message = crypto.kec256(data.toByteArray)
    toECDSASignatures(signature).exists { sig =>
      sig.publicKey(message).map(toPublicKey).contains(publicKey)
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
  private def toECDSASignatures(signature: Signature): Iterable[ECDSASignature] = {
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
  }

  private def toECDSASignature(signatureBytes: Array[Byte]): ECDSASignature =
    ECDSASignature.fromBytes(ByteString(signatureBytes)) getOrElse {
      throw new IllegalArgumentException(s"Could not convert to ECDSA signature.")
    }
}
