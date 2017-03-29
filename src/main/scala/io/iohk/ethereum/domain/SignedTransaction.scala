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
  val LastByteOfAddress: Int = FirstByteOfAddress + Address.Length
  val negativePointSign = 27
  val newNegativePointSign = 35
  val positivePointSign = 28
  val newPositivePointSign = 36
  val valueForEmptyR = 0
  val valueForEmptyS = 0

  /**
    * new formula for calculating point sign post EIP 155 adoption
    * v = CHAIN_ID * 2 + 35 or v = CHAIN_ID * 2 + 36
    */
  def getRecoveredPointSign(pointSign: Byte): Option[Int] = {
    if (pointSign == negativePointSign || pointSign == (Blockchain.chainId * 2 + newNegativePointSign).toByte) {
      Some(negativePointSign)
    } else if (pointSign == positivePointSign || pointSign == (Blockchain.chainId * 2 + newPositivePointSign).toByte) {
      Some(positivePointSign)
    } else {
      None
    }
  }

  def getSender(tx: Transaction, pointSign: Byte, signatureRandom: ByteString, signature: ByteString, recoveredPointSign: Option[Int]): Option[Address] = {
    val bytesToSign: Array[Byte] = if (pointSign == negativePointSign || pointSign == positivePointSign) {
      //global transaction
      crypto.kec256(
        rlpEncode(RLPList(
          tx.nonce,
          tx.gasPrice,
          tx.gasLimit,
          tx.receivingAddress.toArray,
          tx.value,
          tx.payload)))
    } else {
      //chain specific transaction
      crypto.kec256(
        rlpEncode(RLPList(
          tx.nonce,
          tx.gasPrice,
          tx.gasLimit,
          tx.receivingAddress.toArray,
          tx.value,
          tx.payload,
          Config.Blockchain.chainId,
          valueForEmptyR,
          valueForEmptyS)))
    }

    val recoveredPublicKey: Option[Array[Byte]] = recoveredPointSign.flatMap { p =>
      ECDSASignature.recoverPubBytes(
        new BigInteger(1, signatureRandom.toArray[Byte]),
        new BigInteger(1, signature.toArray[Byte]),
        ECDSASignature.recIdFromSignatureV(p),
        bytesToSign
      )
    }

    for {
      key <- recoveredPublicKey
      addrBytes = crypto.kec256(key).slice(FirstByteOfAddress, LastByteOfAddress)
      if addrBytes.length == Address.Length
    } yield Address(addrBytes)
  }

  def apply(tx: Transaction, pointSign: Byte, signatureRandom: ByteString, signature: ByteString): Option[SignedTransaction] = {
    for {
      recoveredPointSign <- SignedTransaction.getRecoveredPointSign(pointSign)
      sender <- SignedTransaction.getSender(tx, pointSign, signatureRandom, signature, Some(recoveredPointSign))
    } yield SignedTransaction(tx, pointSign, signatureRandom, signature, sender, recoveredPointSign)
  }
}

case class SignedTransaction private (
  tx: Transaction,
  pointSign: Byte, //v
  signatureRandom: ByteString, //r
  signature: ByteString, //s
  sender: Address,
  recoveredPointSign: Int) {

  override def toString: String = {
    s"""SignedTransaction {
         |tx: $tx
         |pointSign: $pointSign
         |signatureRandom: ${Hex.toHexString(signatureRandom.toArray[Byte])}
         |signature: ${Hex.toHexString(signature.toArray[Byte])}
         |sender: ${Hex.toHexString(sender.bytes.toArray)}
         |recoveredPointSign: ${recoveredPointSign}
         |}""".stripMargin
  }
}
