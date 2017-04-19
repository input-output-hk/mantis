package io.iohk.ethereum.validators

import java.math.BigInteger

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Address, SignedTransaction, Transaction}
import io.iohk.ethereum.validators.SignedTransactionError.{TransactionSignatureError, TransactionSyntaxError}
import io.iohk.ethereum.utils.Config

trait SignedTransactionValidator {

  def validateTransaction(stx: SignedTransaction, fromBeforeHomestead: Boolean): Either[SignedTransactionError, SignedTransaction]

}

object SignedTransactionValidator extends SignedTransactionValidator {

  val secp256k1n: BigInt = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")

  /**
    * Validates a transaction
    *
    * @param stx                  Transaction to validate
    * @param fromBeforeHomestead  Whether the block to which this transaction belongs is from
    *                             before the HomesteadBlock
    * @return Transaction if valid, error otherwise
    */
  def validateTransaction(stx: SignedTransaction, fromBeforeHomestead: Boolean): Either[SignedTransactionError, SignedTransaction] = {
    for {
      _ <- checkSyntacticValidity(stx)
      _ <- validateSignature(stx, fromBeforeHomestead)
    } yield stx
  }

  /**
    * Validates if the transaction is syntactically valid (lengths of the transaction fields are correct)
    *
    * @param stx Transaction to validate
    * @return Either the validated transaction or TransactionSyntaxError if an error was detected
    */
  private def checkSyntacticValidity(stx: SignedTransaction): Either[SignedTransactionError, SignedTransaction] = {
    import stx.tx._
    import stx._
    import Transaction._

    val maxNonceValue = BigInt(2).pow(8 * NonceLength) - 1
    val maxGasValue = BigInt(2).pow(8 * GasLength) - 1
    val maxValue = BigInt(2).pow(8 * ValueLength) - 1
    val maxR = BigInt(2).pow(8 * ECDSASignature.RLength) - 1
    val maxS = BigInt(2).pow(8 * ECDSASignature.SLength) - 1

    if (nonce > maxNonceValue)
      Left(TransactionSyntaxError(s"Invalid nonce: $nonce > $maxNonceValue"))
    else if(gasLimit > maxGasValue)
      Left(TransactionSyntaxError(s"Invalid gasLimit: $gasLimit > $maxGasValue"))
    else if(gasPrice > maxGasValue)
      Left(TransactionSyntaxError(s"Invalid gasPrice: $gasPrice > $maxGasValue"))
    else if(value > maxValue)
      Left(TransactionSyntaxError(s"Invalid value: $value > $maxValue"))
    else if(BigInt(signature.r) > maxR)
      Left(TransactionSyntaxError(s"Invalid signatureRandom: ${signature.r} > $maxR"))
    else if(BigInt(signature.s) > maxS)
      Left(TransactionSyntaxError(s"Invalid signature: ${signature.s} > $maxS"))
    else
      Right(stx)
  }

  /**
    * Validates if the transaction signature is valid as stated in appendix F in YP
    *
    * @param stx                  Transaction to validate
    * @param fromBeforeHomestead  Whether the block to which this transaction belongs is from
    *                             before the [[Config.Blockchain.homesteadBlockNumber]]
    * @return Either the validated transaction or TransactionSignatureError if an error was detected
    */
  private def validateSignature(stx: SignedTransaction, fromBeforeHomestead: Boolean): Either[SignedTransactionError, SignedTransaction] = {
    val r = BigInt(stx.signature.r)
    val s = BigInt(stx.signature.s)

    val validR = r > 0 && r < secp256k1n
    val validS = s > 0 && s < (if(fromBeforeHomestead) secp256k1n else secp256k1n / 2)

    if(validR && validS) Right(stx)
    else Left(TransactionSignatureError)
  }
}

sealed trait SignedTransactionError

object SignedTransactionError {
  case object TransactionSignatureError extends SignedTransactionError
  case class TransactionSyntaxError(msg: String) extends SignedTransactionError
}
