package io.iohk.ethereum.validators

import java.math.BigInteger

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Address, SignedTransaction, Transaction}
import io.iohk.ethereum.validators.SignedTransactionError.{TransactionSignatureError, TransactionSyntaxError}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class SignedTransactionValidatorSpec extends FlatSpec with Matchers {

  //From block 0x228943f4ef720ac91ca09c08056d7764c2a1650181925dfaeb484f27e544404e with number 1100000 (tx index 0)
  val txBeforeHomestead = Transaction(
    nonce = 81,
    gasPrice = BigInt("60000000000"),
    gasLimit = 21000,
    receivingAddress = Address(Hex.decode("32be343b94f860124dc4fee278fdcbd38c102d88")),
    value = BigInt("1143962220000000000"),
    payload = ByteString.empty)
  val signedTxBeforeHomestead = SignedTransaction(
    txBeforeHomestead,
    pointSign = 0x1b,
    signatureRandom = ByteString(Hex.decode("12bfc6e767e518c50f59006556ecc9911593094cfb6f6ef78c9959e3327137a3")),
    signature = ByteString(Hex.decode("13696dc6b5b601d19960a4f764416d36b271fc292bb87e2c36aea25d52f49064"))).get

  //From block 0xdc7874d8ea90b63aa0ba122055e514db8bb75c0e7d51a448abd12a31ca3370cf with number 1200003 (tx index 0)
  val txAfterHomestead = Transaction(
    nonce = 1631,
    gasPrice = BigInt("30000000000"),
    gasLimit = 21000,
    receivingAddress = Address(Hex.decode("1e0cf4971f42462823b122a9a0a2206902b51132")),
    value = BigInt("1050230460000000000"),
    payload = ByteString.empty)
  val signedTxAfterHomestead = SignedTransaction(
    txBeforeHomestead,
    pointSign = 0x1c,
    signatureRandom = ByteString(Hex.decode("f337e8ca3306c131eabb756aa3701ec7b00bef0d6cc21fbf6a6f291463d58baf")),
    signature = ByteString(Hex.decode("72216654137b4b58a4ece0a6df87aa1a4faf18ec4091839dd1c722fa9604fd09"))).get

  it should "report as valid a tx from before homestead" in {
    SignedTransactionValidator.validateTransaction(signedTxBeforeHomestead, fromBeforeHomestead = true) match {
      case Right(validated) if validated == signedTxBeforeHomestead => succeed
      case _ => fail
    }
  }

  it should "report as valid a tx from after homestead" in {
    SignedTransactionValidator.validateTransaction(signedTxAfterHomestead, fromBeforeHomestead = false) match {
      case Right(validated) if validated == signedTxAfterHomestead => succeed
      case _ => fail
    }
  }

  it should "report as syntactic invalid a tx with long nonce" in {
    val invalidNonce = (0 until Transaction.NonceLength + 1).map(_ => 1.toByte).toArray
    val signedTxWithInvalidNonce = signedTxBeforeHomestead.copy(tx = txBeforeHomestead.copy(nonce = BigInt(invalidNonce)))
    SignedTransactionValidator.validateTransaction(signedTxWithInvalidNonce, fromBeforeHomestead = true) match {
      case Left(_ :TransactionSyntaxError) => succeed
      case _ => fail
    }
  }

  it should "report as syntactic invalid a tx with long gas limit" in {
    val invalidGasLimit = (0 until Transaction.GasLength + 1).map(_ => 1.toByte).toArray
    val signedTxWithInvalidGasLimit = signedTxBeforeHomestead.copy(tx = txBeforeHomestead.copy(gasLimit = BigInt(invalidGasLimit)))
    SignedTransactionValidator.validateTransaction(signedTxWithInvalidGasLimit, fromBeforeHomestead = true) match {
      case Left(_: TransactionSyntaxError) => succeed
      case _ => fail
    }
  }

  it should "report as syntactic invalid a tx with long gas price" in {
    val invalidGasPrice = (0 until Transaction.GasLength + 1).map(_ => 1.toByte).toArray
    val signedTxWithInvalidGasPrice = signedTxBeforeHomestead.copy(tx = txBeforeHomestead.copy(gasPrice = BigInt(invalidGasPrice)))
    SignedTransactionValidator.validateTransaction(signedTxWithInvalidGasPrice, fromBeforeHomestead = true) match {
      case Left(_: TransactionSyntaxError) => succeed
      case _ => fail
    }
  }

  it should "report as syntactic invalid a tx with long value" in {
    val invalidValue = (0 until Transaction.ValueLength + 1).map(_ => 1.toByte).toArray
    val signedTxWithInvalidValue = signedTxBeforeHomestead.copy(tx = txBeforeHomestead.copy(value = BigInt(invalidValue)))
    SignedTransactionValidator.validateTransaction(signedTxWithInvalidValue, fromBeforeHomestead = true) match {
      case Left(_: TransactionSyntaxError) => succeed
      case _ => fail
    }
  }

  it should "report as syntactic invalid a tx with long s" in {
    val signatureWithInvalidS = signedTxBeforeHomestead.signature.copy(s = new BigInteger(1, (0 until ECDSASignature.SLength + 1).map(_ => 1.toByte).toArray))
    val signedTxWithInvalidSignatureLength = signedTxBeforeHomestead.copy(signature = signatureWithInvalidS)
    SignedTransactionValidator.validateTransaction(signedTxWithInvalidSignatureLength, fromBeforeHomestead = true) match {
      case Left(_: TransactionSyntaxError) => succeed
      case _ => fail
    }
  }

  it should "report as syntactic invalid a tx with long r" in {
    val signatureWithInvalidR = signedTxBeforeHomestead.signature.copy(r = new BigInteger(1, (0 until ECDSASignature.RLength + 1).map(_ => 1.toByte).toArray))
    val signedTxWithInvalidSignatureLength = signedTxBeforeHomestead.copy(signature = signatureWithInvalidR)
    SignedTransactionValidator.validateTransaction(signedTxWithInvalidSignatureLength, fromBeforeHomestead = true) match {
      case Left(_: TransactionSyntaxError) => succeed
      case _ => fail
    }
  }

  it should "report a tx with invalid r as having invalid signature" in {
    val signatureWithInvalidR = signedTxBeforeHomestead.signature.copy(r = new BigInteger("0"))
    val signedTxWithInvalidSignatureRandom = signedTxAfterHomestead.copy(signature = signatureWithInvalidR)
    SignedTransactionValidator.validateTransaction(signedTxWithInvalidSignatureRandom, fromBeforeHomestead = false) match {
      case Left(TransactionSignatureError) => succeed
      case _ => fail
    }
  }

  it should "report a tx with invalid s as having invalid signature" in {
    val signatureWithInvalidS = signedTxAfterHomestead.signature.copy(s = (SignedTransactionValidator.secp256k1n / 2 + 1).bigInteger)
    val signedTxWithInvalidSignature = signedTxAfterHomestead.copy(signature = signatureWithInvalidS)
    SignedTransactionValidator.validateTransaction(signedTxWithInvalidSignature, fromBeforeHomestead = false) match {
      case Left(TransactionSignatureError) => succeed
      case _ => fail
    }
  }
}
