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

  /**
    * new formula for calculating point sign post EIP 155 adoption
    * v = CHAIN_ID * 2 + 35 or v = CHAIN_ID * 2 + 36
    */
  private def getRecoveredPointSign(pointSign: Byte, chainId: Byte): Option[Int] = {
    if (pointSign == negativePointSign || pointSign == (chainId * 2 + newNegativePointSign).toByte) {
      Some(negativePointSign)
    } else if (pointSign == positivePointSign || pointSign == (chainId * 2 + newPositivePointSign).toByte) {
      Some(positivePointSign)
    } else {
      None
    }
  }

  private def getSender(tx: Transaction, signature: ECDSASignature, recoveredPointSign: Int, chainId: Byte): Option[Address] = {
    val ECDSASignature(r, s, v) = signature
    val receivingAddressAsArray: Array[Byte] = tx.receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray)
    val bytesToSign: Array[Byte] = if (v == negativePointSign || v == positivePointSign) {
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

    val recoveredPublicKey: Option[Array[Byte]] =
      ECDSASignature.recoverPubBytes(
        new BigInteger(1, r.toByteArray),
        new BigInteger(1, s.toByteArray),
        ECDSASignature.recIdFromSignatureV(recoveredPointSign),
        bytesToSign
      )

    for {
      key <- recoveredPublicKey

      //TODO: Parity doesn't do this (https://github.com/paritytech/parity/blob/master/ethkey/src/keypair.rs#L23-L28)
      //      more research is needed on why `key.tail` is needed
      addrBytes = crypto.kec256(key.tail).slice(FirstByteOfAddress, LastByteOfAddress)
      if addrBytes.length == Address.Length
    } yield Address(addrBytes)
  }

  def apply(tx: Transaction, pointSign: Byte, signatureRandom: ByteString, signature: ByteString, chainId: Byte): Option[SignedTransaction] = {
    val txSignature = ECDSASignature(r = new BigInteger(1, signatureRandom.toArray), s = new BigInteger(1, signature.toArray), v = pointSign)
    SignedTransaction(tx, txSignature, chainId)
  }

  def apply(tx: Transaction, signature: ECDSASignature, chainId: Byte): Option[SignedTransaction] = {
    for {
      recoveredPointSign <- SignedTransaction.getRecoveredPointSign(signature.v, chainId)
      sender <- SignedTransaction.getSender(tx, signature, recoveredPointSign, chainId)
    } yield SignedTransaction(tx, signature, sender)
  }

  def sign(tx: Transaction, keyPair: AsymmetricCipherKeyPair, chainId: Byte): SignedTransaction = {
    val bytes = crypto.kec256(
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

    val sig = ECDSASignature.sign(bytes, keyPair)
    val pub = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false)
    val address = Address(crypto.kec256(pub).drop(FirstByteOfAddress))
    SignedTransaction(tx, sig, address)
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
