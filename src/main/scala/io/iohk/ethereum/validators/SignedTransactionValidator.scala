package io.iohk.ethereum.validators

import java.math.BigInteger

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain._
import io.iohk.ethereum.validators.SignedTransactionError._
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import io.iohk.ethereum.vm.{EvmConfig, UInt256}

trait SignedTransactionValidator {

  def validate(stx: SignedTransaction, senderAccount: Account, blockHeader: BlockHeader, config: EvmConfig, blockchainConfig: BlockchainConfig,
               calculateUpfrontGasCost: Transaction => UInt256, accumGasLimit: BigInt): Either[SignedTransactionError, SignedTransaction]

}

object SignedTransactionValidator extends SignedTransactionValidator {

  val secp256k1n: BigInt = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")

  /**
    * Initial tests of intrinsic validity stated in Section 6 of YP
    *
    * @param stx                        Transaction to validate
    * @param senderAccount              Account of the sender of the tx
    * @param blockHeader                Container block
    * @param config                     used to obtain the homesteadBlockNumber
    * @param calculateUpfrontGasCost    Function used to calculate the upfront gas cost
    * @param accumGasUsed               Total amount of gas spent prior this transaction within the container block
    * @return Transaction if valid, error otherwise
    */
  def validate(stx: SignedTransaction, senderAccount: Account, blockHeader: BlockHeader, config: EvmConfig, blockchainConfig: BlockchainConfig,
               calculateUpfrontGasCost: Transaction => UInt256, accumGasUsed: BigInt): Either[SignedTransactionError, SignedTransaction] = {
    for {
      _ <- checkSyntacticValidity(stx)
      _ <- validateSignature(stx, fromBeforeHomestead = blockHeader.number < blockchainConfig.homesteadBlockNumber)
      _ <- validateNonce(stx, senderAccount.nonce)
      _ <- validateGas(stx, config)
      _ <- validateAccountHasEnoughGasToPayUpfrontCost(stx, senderAccount.balance, calculateUpfrontGasCost)
      _ <- validateGasLimit(stx, accumGasUsed, blockHeader.gasLimit)
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

  /**
    * Validates if the transaction nonce matches current sender account's nonce
    *
    * @param stx Transaction to validate
    * @return Either the validated transaction or an error description
    */
  private def validateNonce(stx: SignedTransaction, senderNonce: UInt256): Either[SignedTransactionError, SignedTransaction] = {
    if (senderNonce == UInt256(stx.tx.nonce)) Right(stx)
    else Left(TransactionNonceError(s"Expected nonce ${UInt256(stx.tx.nonce)} but got $senderNonce"))
  }

  /**
    * Validates the gas limit is no smaller than the intrinsic gas used by the transaction.
    *
    * @param stx Transaction to validate
    * @return Either the validated transaction or an error description
    */
  private def validateGas(stx: SignedTransaction, config: EvmConfig): Either[SignedTransactionError, SignedTransaction] = {
    import stx.tx
    if (stx.tx.gasLimit >= config.calcTransactionIntrinsicGas(tx.payload, tx.isContractInit)) Right(stx)
    else Left(TransactionGasError)
  }

  /**
    * Validates the sender account balance contains at least the cost required in up-front payment.
    *
    * @param stx Transaction to validate
    * @return Either the validated transaction or an error description
    */
  private def validateAccountHasEnoughGasToPayUpfrontCost(stx: SignedTransaction, senderBalance: UInt256, calculateUpfrontCost: Transaction => UInt256):
  Either[SignedTransactionError, SignedTransaction] = {
    val upfrontCost = calculateUpfrontCost(stx.tx)
    if (senderBalance >= upfrontCost) Right(stx)
    else Left(TransactionSenderCantPayUpfrontCostError(s"Upfrontcost ($upfrontCost) > sender balance ($senderBalance)"))
  }

  /**
    * The sum of the transaction’s gas limit and the gas utilised in this block prior must be no greater than the
    * block’s gasLimit
    *
    * @param stx           Transaction to validate
    * @param accumGasLimit Gas spent within tx container block prior executing stx
    * @param blockGasLimit Block gas limit
    * @return Either the validated transaction or an error description
    */
  private def validateGasLimit(stx: SignedTransaction, accumGasLimit: BigInt, blockGasLimit: BigInt): Either[SignedTransactionError, SignedTransaction] = {
    if (stx.tx.gasLimit + accumGasLimit <= blockGasLimit) Right(stx)
    else Left(TransactionGasLimitError)
  }
}

sealed trait SignedTransactionError

object SignedTransactionError {
  case object TransactionSignatureError extends SignedTransactionError
  case class TransactionSyntaxError(reason: String) extends SignedTransactionError
  case class TransactionNonceError(reason: String) extends SignedTransactionError
  case object TransactionGasError extends SignedTransactionError
  case class TransactionSenderCantPayUpfrontCostError(reason: String) extends SignedTransactionError
  case object TransactionGasLimitError extends SignedTransactionError
}
