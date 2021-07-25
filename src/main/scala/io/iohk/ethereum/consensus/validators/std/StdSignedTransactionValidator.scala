package io.iohk.ethereum.consensus.validators
package std

import io.iohk.ethereum.consensus.validators.SignedTransactionError._
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain._
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.EvmConfig

object StdSignedTransactionValidator extends SignedTransactionValidator {

  val secp256k1n: BigInt = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")

  /** Initial tests of intrinsic validity stated in Section 6 of YP
    *
    * @param stx                        Transaction to validate
    * @param senderAccount              Account of the sender of the tx
    * @param blockHeader                Container block
    * @param upfrontGasCost    The upfront gas cost of the tx
    * @param accumGasUsed               Total amount of gas spent prior this transaction within the container block
    * @return Transaction if valid, error otherwise
    */
  def validate(
      stx: SignedTransaction,
      senderAccount: Account,
      blockHeader: BlockHeader,
      upfrontGasCost: UInt256,
      accumGasUsed: BigInt
  )(implicit blockchainConfig: BlockchainConfig): Either[SignedTransactionError, SignedTransactionValid] =
    for {
      _ <- checkSyntacticValidity(stx)
      _ <- validateSignature(stx, blockHeader.number)
      _ <- validateNonce(stx, senderAccount.nonce)
      _ <- validateGasLimitEnoughForIntrinsicGas(stx, blockHeader.number)
      _ <- validateAccountHasEnoughGasToPayUpfrontCost(senderAccount.balance, upfrontGasCost)
      _ <- validateBlockHasEnoughGasLimitForTx(stx, accumGasUsed, blockHeader.gasLimit)
    } yield SignedTransactionValid

  /** Validates if the transaction is syntactically valid (lengths of the transaction fields are correct)
    *
    * @param stx Transaction to validate
    * @return Either the validated transaction or TransactionSyntaxError if an error was detected
    */
  private def checkSyntacticValidity(stx: SignedTransaction): Either[SignedTransactionError, SignedTransactionValid] = {
    import LegacyTransaction._
    import stx._
    import stx.tx._

    val maxNonceValue = BigInt(2).pow(8 * NonceLength) - 1
    val maxGasValue = BigInt(2).pow(8 * GasLength) - 1
    val maxValue = BigInt(2).pow(8 * ValueLength) - 1
    val maxR = BigInt(2).pow(8 * ECDSASignature.RLength) - 1
    val maxS = BigInt(2).pow(8 * ECDSASignature.SLength) - 1

    if (nonce > maxNonceValue)
      Left(TransactionSyntaxError(s"Invalid nonce: $nonce > $maxNonceValue"))
    else if (gasLimit > maxGasValue)
      Left(TransactionSyntaxError(s"Invalid gasLimit: $gasLimit > $maxGasValue"))
    else if (gasPrice > maxGasValue)
      Left(TransactionSyntaxError(s"Invalid gasPrice: $gasPrice > $maxGasValue"))
    else if (value > maxValue)
      Left(TransactionSyntaxError(s"Invalid value: $value > $maxValue"))
    else if (signature.r > maxR)
      Left(TransactionSyntaxError(s"Invalid signatureRandom: ${signature.r} > $maxR"))
    else if (signature.s > maxS)
      Left(TransactionSyntaxError(s"Invalid signature: ${signature.s} > $maxS"))
    else
      Right(SignedTransactionValid)
  }

  /** Validates if the transaction signature is valid as stated in appendix F in YP
    *
    * @param stx                  Transaction to validate
    * @param blockNumber          Number of the block for this transaction
    * @return Either the validated transaction or TransactionSignatureError if an error was detected
    */
  private def validateSignature(
      stx: SignedTransaction,
      blockNumber: BigInt
  )(implicit blockchainConfig: BlockchainConfig): Either[SignedTransactionError, SignedTransactionValid] = {
    val r = stx.signature.r
    val s = stx.signature.s

    val beforeHomestead = blockNumber < blockchainConfig.forkBlockNumbers.homesteadBlockNumber
    val beforeEIP155 = blockNumber < blockchainConfig.forkBlockNumbers.eip155BlockNumber

    val validR = r > 0 && r < secp256k1n
    val validS = s > 0 && s < (if (beforeHomestead) secp256k1n else secp256k1n / 2)
    val validSigningSchema = if (beforeEIP155) !stx.isChainSpecific else true

    if (validR && validS && validSigningSchema) Right(SignedTransactionValid)
    else Left(TransactionSignatureError)
  }

  /** Validates if the transaction nonce matches current sender account's nonce
    *
    * @param stx Transaction to validate
    * @param senderNonce Nonce of the sender of the transaction
    * @return Either the validated transaction or a TransactionNonceError
    */
  private def validateNonce(
      stx: SignedTransaction,
      senderNonce: UInt256
  ): Either[SignedTransactionError, SignedTransactionValid] =
    if (senderNonce == UInt256(stx.tx.nonce)) Right(SignedTransactionValid)
    else Left(TransactionNonceError(UInt256(stx.tx.nonce), senderNonce))

  /** Validates the gas limit is no smaller than the intrinsic gas used by the transaction.
    *
    * @param stx Transaction to validate
    * @param blockHeaderNumber Number of the block where the stx transaction was included
    * @return Either the validated transaction or a TransactionNotEnoughGasForIntrinsicError
    */
  private def validateGasLimitEnoughForIntrinsicGas(
      stx: SignedTransaction,
      blockHeaderNumber: BigInt
  )(implicit blockchainConfig: BlockchainConfig): Either[SignedTransactionError, SignedTransactionValid] = {
    import stx.tx
    val config = EvmConfig.forBlock(blockHeaderNumber, blockchainConfig)
    val txIntrinsicGas = config.calcTransactionIntrinsicGas(tx.payload, tx.isContractInit)
    if (stx.tx.gasLimit >= txIntrinsicGas) Right(SignedTransactionValid)
    else Left(TransactionNotEnoughGasForIntrinsicError(stx.tx.gasLimit, txIntrinsicGas))
  }

  /** Validates the sender account balance contains at least the cost required in up-front payment.
    *
    * @param senderBalance Balance of the sender of the tx
    * @param upfrontCost Upfront cost of the transaction tx
    * @return Either the validated transaction or a TransactionSenderCantPayUpfrontCostError
    */
  private def validateAccountHasEnoughGasToPayUpfrontCost(
      senderBalance: UInt256,
      upfrontCost: UInt256
  ): Either[SignedTransactionError, SignedTransactionValid] =
    if (senderBalance >= upfrontCost) Right(SignedTransactionValid)
    else Left(TransactionSenderCantPayUpfrontCostError(upfrontCost, senderBalance))

  /** The sum of the transaction’s gas limit and the gas utilised in this block prior must be no greater than the
    * block’s gasLimit
    *
    * @param stx           Transaction to validate
    * @param accumGasUsed Gas spent within tx container block prior executing stx
    * @param blockGasLimit Block gas limit
    * @return Either the validated transaction or a TransactionGasLimitTooBigError
    */
  private def validateBlockHasEnoughGasLimitForTx(
      stx: SignedTransaction,
      accumGasUsed: BigInt,
      blockGasLimit: BigInt
  ): Either[SignedTransactionError, SignedTransactionValid] =
    if (stx.tx.gasLimit + accumGasUsed <= blockGasLimit) Right(SignedTransactionValid)
    else Left(TransactionGasLimitTooBigError(stx.tx.gasLimit, accumGasUsed, blockGasLimit))
}
