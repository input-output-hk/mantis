package io.iohk.ethereum.domain

import java.math.BigInteger

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.{ECDSASignature, kec256}
import io.iohk.ethereum.mpt.ByteArraySerializable
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._

object SignedTransaction {

  val FirstByteOfAddress = 12
  val LastByteOfAddress: Int = FirstByteOfAddress + Address.Length
  val negativePointSign = 27
  val newNegativePointSign = 35
  val positivePointSign = 28
  val newPositivePointSign = 36
  val valueForEmptyR = 0
  val valueForEmptyS = 0

  def apply(tx: Transaction, pointSign: Byte, signatureRandom: ByteString, signature: ByteString, chainId: Byte): Option[SignedTransaction] = {
    val txSignature = ECDSASignature(r = new BigInteger(1, signatureRandom.toArray), s = new BigInteger(1, signature.toArray), v = pointSign)
    for {
      sender <- SignedTransaction.getSender(tx, txSignature, chainId)
    } yield SignedTransaction(tx, txSignature, sender)
  }

  def sign(tx: Transaction, keyPair: AsymmetricCipherKeyPair, chainId: Option[Byte]): SignedTransaction = {
    val bytes = bytesToSign(tx, chainId)
    val sig = ECDSASignature.sign(bytes, keyPair, chainId)
    //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of spongycastle encoding
    val pub = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail
    val address = Address(crypto.kec256(pub).drop(FirstByteOfAddress))
    SignedTransaction(tx, sig, address)
  }

  private def bytesToSign(tx: Transaction, chainId: Option[Byte]): Array[Byte] = {
    chainId match {
      case Some(id) =>
        chainSpecificTransactionBytes(tx, id)
      case None =>
        generalTransactionBytes(tx)
    }
  }

  private def getSender(tx: Transaction, signature: ECDSASignature, chainId: Byte): Option[Address] = {
    val ECDSASignature(_, _, v) = signature
    val bytesToSign: Array[Byte] = if (v == ECDSASignature.negativePointSign || v == ECDSASignature.positivePointSign) {
      generalTransactionBytes(tx)
    } else {
      chainSpecificTransactionBytes(tx, chainId)
    }

    val recoveredPublicKey: Option[Array[Byte]] = signature.publicKey(bytesToSign, Some(chainId))

    for {
      key <- recoveredPublicKey
      addrBytes = crypto.kec256(key).slice(FirstByteOfAddress, LastByteOfAddress)
      if addrBytes.length == Address.Length
    } yield Address(addrBytes)
  }

  private def generalTransactionBytes(tx: Transaction): Array[Byte] = {
    val receivingAddressAsArray: Array[Byte] = tx.receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray)
    crypto.kec256(
      rlpEncode(RLPList(
        tx.nonce,
        tx.gasPrice,
        tx.gasLimit,
        receivingAddressAsArray,
        tx.value,
        tx.payload)))
  }

  private def chainSpecificTransactionBytes(tx: Transaction, chainId: Byte): Array[Byte] = {
    val receivingAddressAsArray: Array[Byte] = tx.receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray)
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

  val byteArraySerializable = new ByteArraySerializable[SignedTransaction] {

    override def fromBytes(bytes: Array[Byte]): SignedTransaction = bytes.toSignedTransaction

    override def toBytes(input: SignedTransaction): Array[Byte] = input.toBytes
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

  def isChainSpecific: Boolean =
    signature.v != ECDSASignature.negativePointSign && signature.v != ECDSASignature.positivePointSign

  lazy val hash: ByteString = ByteString(kec256(this.toBytes : Array[Byte]))
  lazy val hashAsHexString: String = Hex.toHexString(hash.toArray[Byte])
}
