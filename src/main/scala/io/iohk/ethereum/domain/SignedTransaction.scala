package io.iohk.ethereum.domain

import java.math.BigInteger

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters
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

  private def getSender(tx: Transaction, signature: ECDSASignature, chainId: Byte): Option[Address] = {
    val ECDSASignature(_, _, v) = signature
    val receivingAddressAsArray: Array[Byte] = tx.receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray)
    val bytesToSign: Array[Byte] = if (v == ECDSASignature.negativePointSign || v == ECDSASignature.positivePointSign) {
      //global transaction
      crypto.kec256(
        rlpEncode(RLPList(
          tx.nonce,
          tx.gasPrice,
          tx.gasLimit,
          receivingAddressAsArray,
          tx.value,
          tx.payload)))
    } else {
      //chain specific transaction
      crypto.kec256(
        rlpEncode(RLPList(
          tx.nonce,
          tx.gasPrice,
          tx.gasLimit,
          receivingAddressAsArray,
          tx.value,
          tx.payload,
          chainId,
          valueForEmptyR,
          valueForEmptyS)))
    }

    val recoveredPublicKey: Option[Array[Byte]] = signature.publicKey(bytesToSign, Some(chainId))

    for {
      key <- recoveredPublicKey
      addrBytes = crypto.kec256(key).slice(FirstByteOfAddress, LastByteOfAddress)
      if addrBytes.length == Address.Length
    } yield Address(addrBytes)
  }

  def apply(tx: Transaction, pointSign: Byte, signatureRandom: ByteString, signature: ByteString, chainId: Byte): Option[SignedTransaction] = {
    val txSignature = ECDSASignature(r = new BigInteger(1, signatureRandom.toArray), s = new BigInteger(1, signature.toArray), v = pointSign)
    SignedTransaction(tx, txSignature, chainId)
  }

  def apply(tx: Transaction, pointSign: Int, signatureRandom: ByteString, signature: ByteString): Option[SignedTransaction] =
    SignedTransaction(tx, pointSign.toByte, signatureRandom, signature)

  def apply(tx: Transaction, signature: ECDSASignature, chainId: Byte): Option[SignedTransaction] = {
    for {
      sender <- SignedTransaction.getSender(tx, signature, chainId)
    } yield SignedTransaction(tx, signature, sender)
  }

  def sign(tx: Transaction, keyPair: AsymmetricCipherKeyPair, chainId: Byte): SignedTransaction = {
    val bytes = bytesToSign(tx, chainId)
    val sig = ECDSASignature.sign(bytes, keyPair, Some(chainId))
    //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of spongycastle encoding
    val pub = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail
    val address = Address(crypto.kec256(pub).drop(FirstByteOfAddress))
    SignedTransaction(tx, sig, address)
  }

  def bytesToSign(tx: Transaction, chainId: Byte): Array[Byte] = {
    crypto.kec256(
      rlpEncode(RLPList(
        tx.nonce,
        tx.gasPrice,
        tx.gasLimit,
        tx.receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray): Array[Byte],
        tx.value,
        tx.payload,
        chainId,
        valueForEmptyR,
        valueForEmptyS)))
  }
}

case class SignedTransaction (
  tx: Transaction,
  signature: ECDSASignature,
  senderAddress: Address) {

  override def toString: String = {
    s"""SignedTransaction {
         |tx: $tx
         |signature: $signature
         |sender: ${Hex.toHexString(senderAddress.bytes.toArray)}
         |}""".stripMargin
  }
}
