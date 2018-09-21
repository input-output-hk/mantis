package io.iohk.ethereum.consensus.validators

import io.iohk.ethereum.domain._

trait SignedTransactionValidator {
  def validate(
    stx: SignedTransaction,
    senderAccount: Account,
    blockHeader: BlockHeader,
    upfrontGasCost: UInt256,
    accumGasUsed: BigInt
  ): Either[SignedTransactionInvalid, SignedTransactionValid]
}


sealed trait SignedTransactionInvalid

object SignedTransactionInvalid {
  case object TransactionSignatureError extends SignedTransactionInvalid
  case class TransactionSyntaxError(reason: String) extends SignedTransactionInvalid
  case class TransactionNonceError(txNonce: UInt256, senderNonce: UInt256) extends SignedTransactionInvalid {
    override def toString: String =
      s"$productPrefix(Got tx nonce $txNonce but sender in mpt is: $senderNonce)"
  }
  case class TransactionNotEnoughGasForIntrinsicError(txGasLimit: BigInt, txIntrinsicGas: BigInt) extends SignedTransactionInvalid {
    override def toString: String =
      s"$productPrefix(Tx gas limit ($txGasLimit) < tx intrinsic gas ($txIntrinsicGas))"
  }
  case class TransactionSenderCantPayUpfrontCostError(upfrontCost: UInt256, senderBalance: UInt256) extends SignedTransactionInvalid {
    override def toString: String =
      s"$productPrefix(Upfrontcost ($upfrontCost) > sender balance ($senderBalance))"
  }
  case class TransactionGasLimitTooBigError(txGasLimit: BigInt, accumGasUsed: BigInt, blockGasLimit: BigInt) extends SignedTransactionInvalid {
    override def toString: String =
      s"$productPrefix(Tx gas limit ($txGasLimit) + gas accum ($accumGasUsed) > block gas limit ($blockGasLimit))"
  }
}

sealed trait SignedTransactionValid
case object SignedTransactionValid extends SignedTransactionValid
