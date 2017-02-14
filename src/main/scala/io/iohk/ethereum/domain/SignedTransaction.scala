package io.iohk.ethereum.domain

import java.math.BigInteger

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Config.Blockchain
import org.spongycastle.util.encoders.Hex


object SignedTransaction {

  val FirstByteOfAddress = 12
  val AddressLength = 20
  val LastByteOfAddress: Int = FirstByteOfAddress + AddressLength
  val negativePointSign = 27
  val newNegativePointSign = 35
  val positivePointSign = 28
  val newPositivePointSign = 36
}

case class SignedTransaction(
  tx: Transaction,
  pointSign: Byte, //v
  signatureRandom: ByteString, //r
  signature: ByteString /*s*/) {

  import SignedTransaction._

  lazy val bytesToSign: Array[Byte] = if (pointSign == negativePointSign || pointSign == positivePointSign) {
    //global transaction
    crypto.sha3(
      rlpEncode(RLPList(
        tx.nonce,
        tx.gasPrice,
        tx.gasLimit,
        tx.receivingAddress.toArray,
        tx.value,
        tx.payload)))
  } else {
    //chain specific transaction
    crypto.sha3(
      rlpEncode(RLPList(
        tx.nonce,
        tx.gasPrice,
        tx.gasLimit,
        tx.receivingAddress.toArray,
        tx.value,
        tx.payload,
        Config.Blockchain.chainId,
        0,
        0)))
  }


  lazy val recoveredAddress: Option[Array[Byte]] =
    recoveredPublicKey.map(key => crypto.sha3(key).slice(FirstByteOfAddress, LastByteOfAddress))

  /**
    * formula for calculating pointt sing new way
    * v = CHAIN_ID * 2 + 35 or v = CHAIN_ID * 2 + 36
    */
  lazy val recoveredPointSign: Option[Int] = pointSign match {
    case p if p == negativePointSign || p == (Blockchain.chainId * 2 + newNegativePointSign).toByte =>
      Some(negativePointSign)
    case p if p == positivePointSign || p == (Blockchain.chainId * 2 + newPositivePointSign).toByte =>
      Some(positivePointSign)
    case _ => None
  }

  lazy val recoveredPublicKey: Option[Array[Byte]] = recoveredPointSign.flatMap { p =>
    ECDSASignature.recoverPubBytes(
      new BigInteger(1, signatureRandom.toArray[Byte]),
      new BigInteger(1, signature.toArray[Byte]),
      ECDSASignature.recIdFromSignatureV(p),
      bytesToSign
    )
  }

  lazy val syntacticValidity: Boolean = {

    import tx._
    import Transaction._

    def byteLength(b: BigInt): Int = b.toByteArray.length

    byteLength(nonce) <= NonceLength &&
    (receivingAddress.bytes.isEmpty || receivingAddress.bytes.length == AddressLength) &&
    byteLength(gasLimit) <= GasLength &&
    byteLength(gasPrice) <= GasLength &&
    byteLength(value) <= ValueLength &&
    signatureRandom.length <= ECDSASignature.RLength &&
    signature.length <= ECDSASignature.SLength &&
    recoveredAddress.isDefined && recoveredAddress.get.length == AddressLength
  }

  override def toString: String = {
    s"""SignedTransaction {
         |tx: $tx
         |pointSign: $pointSign
         |signatureRandom: ${Hex.toHexString(signatureRandom.toArray[Byte])}
         |signature: ${Hex.toHexString(signatureRandom.toArray[Byte])}
         |bytesToSign: ${Hex.toHexString(bytesToSign)}
         |recoveredPublicKey: ${recoveredPublicKey.map(Hex.toHexString)}
         |recoveredAddress: ${recoveredAddress.map(Hex.toHexString)}
         |}""".stripMargin
  }
}
