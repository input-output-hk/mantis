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

    if (byteLength(nonce) <= NonceLength &&
      receivingAddress.bytes.length == Address.Length &&
      byteLength(gasLimit) <= GasLength &&
      byteLength(gasPrice) <= GasLength &&
      byteLength(value) <= ValueLength &&
      signatureRandom.length <= ECDSASignature.RLength &&
      signature.length <= ECDSASignature.SLength) Right(stx) else Left(TransactionSyntaxError)
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
    val r: BigInt = BigInt(new BigInteger(1, stx.signatureRandom.toArray[Byte]))
    val s: BigInt = BigInt(new BigInteger(1, stx.signature.toArray[Byte]))
    val v: Byte = ECDSASignature.recIdFromSignatureV(stx.pointSign)

    val validR = r > 0 && r < secp256k1n
    val validS = s > 0 && s < (if(fromBeforeHomestead) secp256k1n else secp256k1n / 2)
    val validV = v == 0 || v == 1

    if(validR && validS && validV && stx.recoveredSenderAddress.isDefined) Right(stx)
    else Left(TransactionSignatureError)
  }
}

sealed trait SignedTransactionError

object SignedTransactionError {
  case object TransactionSignatureError extends SignedTransactionError
  case object TransactionSyntaxError extends SignedTransactionError
}
