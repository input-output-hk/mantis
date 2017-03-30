package io.iohk.ethereum.network.p2p.validators

import java.math.BigInteger

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Address, SignedTransaction, Transaction}
import io.iohk.ethereum.network.p2p.validators.SignedTransactionError.{TransactionSignatureError, TransactionSyntaxError}
import io.iohk.ethereum.utils.Config

object SignedTransactionValidator {

  val secp256k1n: BigInt = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")

  /**
    * Validates a transaction
    *
    * @param stx                  Transaction to validate
    * @param fromBeforeHomestead  Whether the block to which this transaction belongs is from
    *                             before the [[Config.Blockchain.HomesteadBlock]]
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

    def byteLength(b: BigInt): Int = b.toByteArray.length

    if (byteLength(nonce) > NonceLength)
      Left(TransactionSyntaxError(s"Invalid nonce length: ${byteLength(nonce)}"))
    else if(byteLength(gasLimit) > GasLength)
      Left(TransactionSyntaxError(s"Invalid gasLimit length: ${byteLength(gasLimit)}"))
    else if(byteLength(gasPrice) > GasLength)
      Left(TransactionSyntaxError(s"Invalid gasPrice length: ${byteLength(gasPrice)}"))
    else if(byteLength(value) > ValueLength)
      Left(TransactionSyntaxError(s"Invalid value length: ${byteLength(value)}"))
    else if(signatureRandom >= BigInt(2).pow(8 * ECDSASignature.RLength))
      Left(TransactionSyntaxError(s"Too big signatureRandom: $signatureRandom"))
    else if(signature >= BigInt(2).pow(8 * ECDSASignature.SLength))
      Left(TransactionSyntaxError(s"Too big signature: $signature"))
    else
      Right(stx)
  }

  /**
    * Validates if the transaction signature is valid as stated in appendix F in YP
    *
    * @param stx                  Transaction to validate
    * @param fromBeforeHomestead  Whether the block to which this transaction belongs is from
    *                             before the [[Config.Blockchain.HomesteadBlock]]
    * @return Either the validated transaction or TransactionSignatureError if an error was detected
    */
  private def validateSignature(stx: SignedTransaction, fromBeforeHomestead: Boolean): Either[SignedTransactionError, SignedTransaction] = {
    val validR = stx.signatureRandom > 0 && stx.signatureRandom < secp256k1n
    val validS = stx.signature > 0 && stx.signature < (if(fromBeforeHomestead) secp256k1n else secp256k1n / 2)

    if(validR && validS) Right(stx)
    else Left(TransactionSignatureError)
  }
}

sealed trait SignedTransactionError

object SignedTransactionError {
  case object TransactionSignatureError extends SignedTransactionError
  case class TransactionSyntaxError(msg: String) extends SignedTransactionError
}
