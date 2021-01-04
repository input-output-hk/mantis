package io.iohk.ethereum.crypto

import akka.util.ByteString
import io.iohk.ethereum.utils.ByteUtils
import org.bouncycastle.asn1.x9.X9IntegerConverter
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import org.bouncycastle.math.ec.{ECCurve, ECPoint}

import scala.util.Try

object ECDSASignature {

  val SLength = 32
  val RLength = 32
  val VLength = 1
  val EncodedLength: Int = RLength + SLength + VLength

  //byte value that indicates that bytes representing ECC point are in uncompressed format, and should be decoded properly
  val UncompressedIndicator: Byte = 0x04
  val CompressedEvenIndicator: Byte = 0x02
  val CompressedOddIndicator: Byte = 0x03

  //only naming convention
  val negativePointSign: Byte = 27
  val newNegativePointSign: Byte = 35
  val positivePointSign: Byte = 28
  val newPositivePointSign: Byte = 36

  val allowedPointSigns = Set(negativePointSign, positivePointSign)

  def apply(r: ByteString, s: ByteString, v: Byte): ECDSASignature = {
    ECDSASignature(BigInt(1, r.toArray), BigInt(1, s.toArray), v)
  }

  def fromBytes(bytes65: ByteString): Option[ECDSASignature] = {
    if (bytes65.length == EncodedLength)
      Some(apply(bytes65.take(RLength), bytes65.drop(RLength).take(SLength), bytes65.last))
    else
      None
  }

  def sign(messageHash: ByteString, prvKey: ByteString): ECDSASignature =
    sign(messageHash.toArray, keyPairFromPrvKey(prvKey.toArray), None)

  /** Sign a messageHash, expected to be a Keccak256 hash of the original data. */
  def sign(messageHash: Array[Byte], keyPair: AsymmetricCipherKeyPair, chainId: Option[Byte] = None): ECDSASignature = {
    require(
      messageHash.size == 32,
      s"The message should be a hash, expected to be 32 bytes; got ${messageHash.size} bytes."
    )
    val signer = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest))
    signer.init(true, keyPair.getPrivate)
    val components = signer.generateSignature(messageHash)
    val r = components(0)
    val s = ECDSASignature.canonicalise(components(1))
    val v = ECDSASignature
      .calculateV(r, s, keyPair, messageHash)
      .getOrElse(throw new RuntimeException("Failed to calculate signature rec id"))

    val pointSign = chainId match {
      case Some(id) if v == negativePointSign => (id * 2 + newNegativePointSign).toByte
      case Some(id) if v == positivePointSign => (id * 2 + newPositivePointSign).toByte
      case None => v
      case _    => throw new IllegalStateException(s"Unexpected pointSign. ChainId: ${chainId}, v: ${v}")
    }

    ECDSASignature(r, s, pointSign)
  }

  /**
    * new formula for calculating point sign post EIP 155 adoption
    * v = CHAIN_ID * 2 + 35 or v = CHAIN_ID * 2 + 36
    */
  private def getRecoveredPointSign(pointSign: Byte, chainId: Option[Byte]): Option[Byte] = {
    (chainId match {
      case Some(id) =>
        if (pointSign == negativePointSign || pointSign == (id * 2 + newNegativePointSign).toByte) {
          Some(negativePointSign)
        } else if (pointSign == positivePointSign || pointSign == (id * 2 + newPositivePointSign).toByte) {
          Some(positivePointSign)
        } else {
          None
        }
      case None => Some(pointSign)
    }).filter(pointSign => allowedPointSigns.contains(pointSign))
  }

  private def canonicalise(s: BigInt): BigInt = {
    val halfCurveOrder: BigInt = curveParams.getN.shiftRight(1)
    if (s > halfCurveOrder) BigInt(curve.getN) - s
    else s
  }

  private def calculateV(r: BigInt, s: BigInt, key: AsymmetricCipherKeyPair, messageHash: Array[Byte]): Option[Byte] = {
    //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of bouncycastle encoding
    val pubKey = key.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail
    val recIdOpt = Seq(positivePointSign, negativePointSign).find { i =>
      recoverPubBytes(r, s, i, None, messageHash).exists(java.util.Arrays.equals(_, pubKey))
    }
    recIdOpt
  }

  private def recoverPubBytes(
      r: BigInt,
      s: BigInt,
      recId: Byte,
      chainId: Option[Byte],
      messageHash: Array[Byte]
  ): Option[Array[Byte]] = {
    Try {
      val order = curve.getCurve.getOrder
      //ignore case when x = r + order because it is negligibly improbable
      //says: https://github.com/paritytech/rust-secp256k1/blob/f998f9a8c18227af200f0f7fdadf8a6560d391ff/depend/secp256k1/src/ecdsa_impl.h#L282
      val xCoordinate = r
      val curveFp = curve.getCurve.asInstanceOf[ECCurve.Fp]
      val prime = curveFp.getQ

      getRecoveredPointSign(recId, chainId).flatMap { recovery =>
        if (xCoordinate.compareTo(prime) < 0) {
          val R = constructPoint(xCoordinate, recovery)
          if (R.multiply(order).isInfinity) {
            val e = BigInt(1, messageHash)
            val rInv = r.modInverse(order)
            //Q = r^(-1)(sR - eG)
            val q = R.multiply(s.bigInteger).subtract(curve.getG.multiply(e.bigInteger)).multiply(rInv.bigInteger)
            //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of bouncycastle encoding
            Some(q.getEncoded(false).tail)
          } else None
        } else None
      }
    }.toOption.flatten
  }

  private def constructPoint(xCoordinate: BigInt, recId: Int): ECPoint = {
    val x9 = new X9IntegerConverter
    val compEnc = x9.integerToBytes(xCoordinate.bigInteger, 1 + x9.getByteLength(curve.getCurve))
    compEnc(0) = if (recId == ECDSASignature.positivePointSign) 3.toByte else 2.toByte
    curve.getCurve.decodePoint(compEnc)
  }
}

/**
  * ECDSASignature r and s are same as in documentation where signature is represented by tuple (r, s)
  *
  * The `publicKey` method is also the way to verify the signature: if the key can be retrieved based
  * on the signed message, the signature is correct, otherwise it isn't.
  *
  * @param r - x coordinate of ephemeral public key modulo curve order N
  * @param s - part of the signature calculated with signer private key
  * @param v - public key recovery id
  */
case class ECDSASignature(r: BigInt, s: BigInt, v: Byte) {

  /**
    * returns ECC point encoded with on compression and without leading byte indicating compression
    * @param messageHash message to be signed; should be a hash of the actual data.
    * @param chainId optional value if you want new signing schema with recovery id calculated with chain id
    * @return
    */
  def publicKey(messageHash: Array[Byte], chainId: Option[Byte] = None): Option[Array[Byte]] =
    ECDSASignature.recoverPubBytes(r, s, v, chainId, messageHash)

  /**
    * returns ECC point encoded with on compression and without leading byte indicating compression
    * @param messageHash message to be signed; should be a hash of the actual data.
    * @return
    */
  def publicKey(messageHash: ByteString): Option[ByteString] =
    ECDSASignature.recoverPubBytes(r, s, v, None, messageHash.toArray[Byte]).map(ByteString(_))

  def toBytes: ByteString = {
    import ECDSASignature.RLength

    def bigInt2Bytes(b: BigInt) =
      ByteUtils.padLeft(ByteString(b.toByteArray).takeRight(RLength), RLength, 0)


    bigInt2Bytes(r) ++ bigInt2Bytes(s) ++ ByteString(v)
  }
}
