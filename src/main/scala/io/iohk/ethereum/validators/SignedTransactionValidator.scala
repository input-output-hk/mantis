package io.iohk.ethereum.validators

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain._
import io.iohk.ethereum.validators.SignedTransactionError._
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.{EvmConfig, UInt256}

trait SignedTransactionValidator {

  def validate(stx: SignedTransaction, senderAccount: Account, blockHeader: BlockHeader,
               upfrontGasCost: UInt256, accumGasUsed: BigInt): Either[SignedTransactionError, Unit]

}

class SignedTransactionValidatorImpl(blockchainConfig: BlockchainConfig) extends SignedTransactionValidator {

  val secp256k1n: BigInt = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")

  /**
    * Initial tests of intrinsic validity stated in Section 6 of YP
    *
    * @param stx                        Transaction to validate
    * @param senderAccount              Account of the sender of the tx
    * @param blockHeader                Container block
    * @param upfrontGasCost    The upfront gas cost of the tx
    * @param accumGasUsed               Total amount of gas spent prior this transaction within the container block
    * @return Transaction if valid, error otherwise
    */
  def validate(stx: SignedTransaction, senderAccount: Account, blockHeader: BlockHeader,
               upfrontGasCost: UInt256, accumGasUsed: BigInt): Either[SignedTransactionError, Unit] = {
    for {
      _ <- checkSyntacticValidity(stx)
      _ <- validateSignature(stx, fromBeforeHomestead = blockHeader.number < blockchainConfig.homesteadBlockNumber)
      _ <- validateNonce(stx, senderAccount.nonce)
      _ <- validateGasLimitEnoughForIntrinsicGas(stx, blockHeader.number)
      _ <- validateAccountHasEnoughGasToPayUpfrontCost(stx, senderAccount.balance, upfrontGasCost)
      _ <- validateBlockHasEnoughGasLimitForTx(stx, accumGasUsed, blockHeader.gasLimit)
    } yield ()
  }

  /**
    * Validates if the transaction is syntactically valid (lengths of the transaction fields are correct)
    *
    * @param stx Transaction to validate
    * @return Either the validated transaction or TransactionSyntaxError if an error was detected
    */
  private def checkSyntacticValidity(stx: SignedTransaction): Either[SignedTransactionError, Unit] = {
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
      Right(())
  }

  /**
    * Validates if the transaction signature is valid as stated in appendix F in YP
    *
    * @param stx                  Transaction to validate
    * @param fromBeforeHomestead  Whether the block to which this transaction belongs is from
    *                             before the homesteadBlockNumber
    * @return Either the validated transaction or TransactionSignatureError if an error was detected
    */
  private def validateSignature(stx: SignedTransaction, fromBeforeHomestead: Boolean): Either[SignedTransactionError, Unit] = {
    val r = BigInt(stx.signature.r)
    val s = BigInt(stx.signature.s)

    val validR = r > 0 && r < secp256k1n
    val validS = s > 0 && s < (if(fromBeforeHomestead) secp256k1n else secp256k1n / 2)

    if(validR && validS) Right(())
    else Left(TransactionSignatureError)
  }

  /**
    * Validates if the transaction nonce matches current sender account's nonce
    *
    * @param stx Transaction to validate
    * @param senderNonce Nonce of the sender of the transaction
    * @return Either the validated transaction or a TransactionNonceError
    */
  private def validateNonce(stx: SignedTransaction, senderNonce: UInt256): Either[SignedTransactionError, Unit] = {
    if (senderNonce == UInt256(stx.tx.nonce)) Right(())
    else Left(TransactionNonceError(UInt256(stx.tx.nonce), senderNonce))
  }

  /**
    * Validates the gas limit is no smaller than the intrinsic gas used by the transaction.
    *
    * @param stx Transaction to validate
    * @param blockHeaderNumber Number of the block where the stx transaction was included
    * @return Either the validated transaction or a TransactionNotEnoughGasForIntrinsicError
    */
  private def validateGasLimitEnoughForIntrinsicGas(stx: SignedTransaction, blockHeaderNumber: BigInt): Either[SignedTransactionError, Unit] = {
    import stx.tx
    val config = EvmConfig.forBlock(blockHeaderNumber, blockchainConfig)
    val txIntrinsicGas = config.calcTransactionIntrinsicGas(tx.payload, tx.isContractInit)
    if (stx.tx.gasLimit >= txIntrinsicGas) Right(())
    else Left(TransactionNotEnoughGasForIntrinsicError(stx.tx.gasLimit, txIntrinsicGas))
  }

  /**
    * Validates the sender account balance contains at least the cost required in up-front payment.
    *
    * @param stx Transaction to validate
    * @param senderBalance Balance of the sender of the tx
    * @param upfrontCost Upfront cost of the transaction tx
    * @return Either the validated transaction or a TransactionSenderCantPayUpfrontCostError
    */
  private def validateAccountHasEnoughGasToPayUpfrontCost(stx: SignedTransaction, senderBalance: UInt256, upfrontCost: UInt256)
  : Either[SignedTransactionError, Unit] = {
    if (senderBalance >= upfrontCost) Right(())
    else Left(TransactionSenderCantPayUpfrontCostError(upfrontCost, senderBalance))
  }

  /**
    * The sum of the transaction’s gas limit and the gas utilised in this block prior must be no greater than the
    * block’s gasLimit
    *
    * @param stx           Transaction to validate
    * @param accumGasUsed Gas spent within tx container block prior executing stx
    * @param blockGasLimit Block gas limit
    * @return Either the validated transaction or a TransactionGasLimitTooBigError
    */
  private def validateBlockHasEnoughGasLimitForTx(stx: SignedTransaction, accumGasUsed: BigInt, blockGasLimit: BigInt)
  : Either[SignedTransactionError, Unit] = {
    if (stx.tx.gasLimit + accumGasUsed <= blockGasLimit) Right(())
    else Left(TransactionGasLimitTooBigError(stx.tx.gasLimit, accumGasUsed, blockGasLimit))
  }
}

sealed trait SignedTransactionError

object SignedTransactionError {
  case object TransactionSignatureError extends SignedTransactionError
  case class TransactionSyntaxError(reason: String) extends SignedTransactionError
  case class TransactionNonceError(txNonce: UInt256, senderNonce: UInt256) extends SignedTransactionError {
    override def toString: String =
      s"${getClass.getSimpleName}(Expected nonce $txNonce but got $senderNonce)"
  }
  case class TransactionNotEnoughGasForIntrinsicError(txGasLimit: BigInt, txIntrinsicGas: UInt256) extends SignedTransactionError {
    override def toString: String =
      s"${getClass.getSimpleName}(Tx gas limit ($txGasLimit) < tx intrinsic gas ($txIntrinsicGas))"
  }
  case class TransactionSenderCantPayUpfrontCostError(upfrontCost: UInt256, senderBalance: UInt256) extends SignedTransactionError {
    override def toString: String =
      s"${getClass.getSimpleName}(Upfrontcost ($upfrontCost) > sender balance ($senderBalance))"
  }
  case class TransactionGasLimitTooBigError(txGasLimit: BigInt, accumGasUsed: BigInt, blockGasLimit: BigInt) extends SignedTransactionError {
    override def toString: String =
      s"${getClass.getSimpleName}(Tx gas limit ($txGasLimit) + gas accum ($accumGasUsed) > block gas limit ($blockGasLimit))"
  }
}
