package io.iohk.ethereum.signer

sealed trait SignerError
case class IncompatibleTransactionType(transactionType: String, context: String) extends SignerError
