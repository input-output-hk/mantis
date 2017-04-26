package io.iohk.ethereum.validators

import java.math.BigInteger

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Account, Address, SignedTransaction, Transaction}
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import io.iohk.ethereum.validators.SignedTransactionError._
import io.iohk.ethereum.vm.{EvmConfig, UInt256}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class SignedTransactionValidatorSpec extends FlatSpec with Matchers {

  val blockchainConfig = BlockchainConfig(Config.config)

  val signedTransactionValidator = new SignedTransactionValidatorImpl(blockchainConfig)

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
    signature = ByteString(Hex.decode("13696dc6b5b601d19960a4f764416d36b271fc292bb87e2c36aea25d52f49064")),
    chainId = 0x3d.toByte).get

  //From block 0xdc7874d8ea90b63aa0ba122055e514db8bb75c0e7d51a448abd12a31ca3370cf with number 1200003 (tx index 0)
  val txAfterHomestead = Transaction(
    nonce = 1631,
    gasPrice = BigInt("30000000000"),
    gasLimit = 21000,
    receivingAddress = Address(Hex.decode("1e0cf4971f42462823b122a9a0a2206902b51132")),
    value = BigInt("1050230460000000000"),
    payload = ByteString.empty)
  val signedTxAfterHomestead = SignedTransaction(
    txAfterHomestead,
    pointSign = 0x1c,
    signatureRandom = ByteString(Hex.decode("f337e8ca3306c131eabb756aa3701ec7b00bef0d6cc21fbf6a6f291463d58baf")),
    signature = ByteString(Hex.decode("72216654137b4b58a4ece0a6df87aa1a4faf18ec4091839dd1c722fa9604fd09")),
    chainId = 0x3d.toByte).get

  val senderBalance = 100

  val senderAccountBeforeHomestead = Account.Empty
    .copy(nonce = UInt256(txBeforeHomestead.nonce), balance = senderBalance)

  val senderAccountAfterHomestead = Account.Empty
    .copy(nonce = UInt256(txAfterHomestead.nonce), balance = senderBalance)

  val blockHeaderBeforeHomestead = Fixtures.Blocks.Block3125369.header.copy(number = 1100000, gasLimit = 4700000)

  val blockHeaderAfterHomestead = Fixtures.Blocks.Block3125369.header.copy(number = 1200003, gasLimit = 4710000)

  val accumGasUsed = 0 //Both are the first tx in the block

  val upfrontGasCost: UInt256 = UInt256(senderBalance / 2)

  def validateStx(stx: SignedTransaction, fromBeforeHomestead: Boolean): Either[SignedTransactionError, Unit] = {
    val (senderAccount, blockHeader) =
      if(fromBeforeHomestead)
        (senderAccountBeforeHomestead, blockHeaderBeforeHomestead)
      else
        (senderAccountAfterHomestead, blockHeaderAfterHomestead)
    signedTransactionValidator.validate(
      stx = stx,
      senderAccount = senderAccount,
      blockHeader = blockHeader,
      upfrontGasCost = upfrontGasCost,
      accumGasUsed = accumGasUsed
    )
  }

  it should "report as valid a tx from before homestead" in {
    validateStx(signedTxBeforeHomestead, fromBeforeHomestead = true) match {
      case Right(_)  => succeed
      case _ => fail
    }
  }

  it should "report as valid a tx from after homestead" in {
    validateStx(signedTxAfterHomestead, fromBeforeHomestead = false) match {
      case Right(_) => succeed
      case _ => fail
    }
  }

  it should "report as syntactic invalid a tx with long nonce" in {
    val invalidNonce = (0 until Transaction.NonceLength + 1).map(_ => 1.toByte).toArray
    val signedTxWithInvalidNonce = signedTxBeforeHomestead.copy(tx = txBeforeHomestead.copy(nonce = BigInt(invalidNonce)))
    validateStx(signedTxWithInvalidNonce, fromBeforeHomestead = true) match {
      case Left(_ :TransactionSyntaxError) => succeed
      case _ => fail
    }
  }

  it should "report as syntactic invalid a tx with long gas limit" in {
    val invalidGasLimit = (0 until Transaction.GasLength + 1).map(_ => 1.toByte).toArray
    val signedTxWithInvalidGasLimit = signedTxBeforeHomestead.copy(tx = txBeforeHomestead.copy(gasLimit = BigInt(invalidGasLimit)))
    validateStx(signedTxWithInvalidGasLimit, fromBeforeHomestead = true) match {
      case Left(_: TransactionSyntaxError) => succeed
      case _ => fail
    }
  }

  it should "report as syntactic invalid a tx with long gas price" in {
    val invalidGasPrice = (0 until Transaction.GasLength + 1).map(_ => 1.toByte).toArray
    val signedTxWithInvalidGasPrice = signedTxBeforeHomestead.copy(tx = txBeforeHomestead.copy(gasPrice = BigInt(invalidGasPrice)))
    validateStx(signedTxWithInvalidGasPrice, fromBeforeHomestead = true) match {
      case Left(_: TransactionSyntaxError) => succeed
      case _ => fail
    }
  }

  it should "report as syntactic invalid a tx with long value" in {
    val invalidValue = (0 until Transaction.ValueLength + 1).map(_ => 1.toByte).toArray
    val signedTxWithInvalidValue = signedTxBeforeHomestead.copy(tx = txBeforeHomestead.copy(value = BigInt(invalidValue)))
    validateStx(signedTxWithInvalidValue, fromBeforeHomestead = true) match {
      case Left(_: TransactionSyntaxError) => succeed
      case _ => fail
    }
  }

  it should "report as syntactic invalid a tx with long s" in {
    val signatureWithInvalidS = signedTxBeforeHomestead.signature.copy(s = new BigInteger(1, (0 until ECDSASignature.SLength + 1).map(_ => 1.toByte).toArray))
    val signedTxWithInvalidSignatureLength = signedTxBeforeHomestead.copy(signature = signatureWithInvalidS)
    validateStx(signedTxWithInvalidSignatureLength, fromBeforeHomestead = true) match {
      case Left(_: TransactionSyntaxError) => succeed
      case _ => fail
    }
  }

  it should "report as syntactic invalid a tx with long r" in {
    val signatureWithInvalidR = signedTxBeforeHomestead.signature.copy(r = new BigInteger(1, (0 until ECDSASignature.RLength + 1).map(_ => 1.toByte).toArray))
    val signedTxWithInvalidSignatureLength = signedTxBeforeHomestead.copy(signature = signatureWithInvalidR)
    validateStx(signedTxWithInvalidSignatureLength, fromBeforeHomestead = true) match {
      case Left(_: TransactionSyntaxError) => succeed
      case _ => fail
    }
  }

  it should "report a tx with invalid r as having invalid signature" in {
    val signatureWithInvalidR = signedTxBeforeHomestead.signature.copy(r = new BigInteger("0"))
    val signedTxWithInvalidSignatureRandom = signedTxAfterHomestead.copy(signature = signatureWithInvalidR)
    validateStx(signedTxWithInvalidSignatureRandom, fromBeforeHomestead = false) match {
      case Left(TransactionSignatureError) => succeed
      case _ => fail
    }
  }

  it should "report a tx with invalid s as having invalid signature" in {
    val signatureWithInvalidS = signedTxAfterHomestead.signature.copy(s = (signedTransactionValidator.secp256k1n / 2 + 1).bigInteger)
    val signedTxWithInvalidSignature = signedTxAfterHomestead.copy(signature = signatureWithInvalidS)
    validateStx(signedTxWithInvalidSignature, fromBeforeHomestead = false) match {
      case Left(TransactionSignatureError) => succeed
      case _ => fail
    }
  }

  it should "report as invalid a tx with invalid nonce" in {
    val txWithInvalidNonce = txAfterHomestead.copy(nonce = txAfterHomestead.nonce + 1)
    val signedTxWithInvalidNonce = signedTxAfterHomestead.copy(tx = txWithInvalidNonce)
    validateStx(signedTxWithInvalidNonce, fromBeforeHomestead = false) match {
      case Left(_: TransactionNonceError) => succeed
      case _ => fail
    }
  }

  it should "report as invalid a tx with too low gas limit for intrinsic gas" in {
    val txIntrinsicGas = EvmConfig.forBlock(blockHeaderAfterHomestead.number, blockchainConfig)
      .calcTransactionIntrinsicGas(txAfterHomestead.payload, txAfterHomestead.isContractInit)
    val txWithInvalidGasLimit = txAfterHomestead.copy(gasLimit = txIntrinsicGas / 2)
    val signedTxWithInvalidGasLimit = signedTxAfterHomestead.copy(tx = txWithInvalidGasLimit)
    validateStx(signedTxWithInvalidGasLimit, fromBeforeHomestead = false) match {
      case Left(_: TransactionNotEnoughGasForIntrinsicError) => succeed
      case _ => fail
    }
  }

  it should "report as invalid a tx with upfront cost higher than the sender's balance" in {
    val senderAccountWithLowBalance = senderAccountAfterHomestead.copy(balance = upfrontGasCost / 2)
    signedTransactionValidator.validate(
      stx = signedTxAfterHomestead,
      senderAccount = senderAccountWithLowBalance,
      blockHeader = blockHeaderAfterHomestead,
      upfrontGasCost = upfrontGasCost,
      accumGasUsed = accumGasUsed
    ) match {
      case Left(_: TransactionSenderCantPayUpfrontCostError) => succeed
      case _ => fail
    }
  }

  it should "report as invalid a tx with too high gas limit for block gas limit" in {
    val txWithInvalidGasLimit = txAfterHomestead.copy(gasLimit = blockHeaderAfterHomestead.gasLimit + 1)
    val signedTxWithInvalidGasLimit = signedTxAfterHomestead.copy(tx = txWithInvalidGasLimit)
    validateStx(signedTxWithInvalidGasLimit, fromBeforeHomestead = false) match {
      case Left(_: TransactionGasLimitTooBigError) => succeed
      case _ => fail
    }
  }
}
