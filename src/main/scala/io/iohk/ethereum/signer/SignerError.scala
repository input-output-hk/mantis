package io.iohk.ethereum.signer

trait SignerError
case class IncompatibleTransactionType(transactionType: String, context: String) extends SignerError
