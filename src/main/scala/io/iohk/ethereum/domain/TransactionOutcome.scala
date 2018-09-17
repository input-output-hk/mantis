package io.iohk.ethereum.domain

import akka.util.ByteString

trait TransactionOutcome

case class HashOutcome(stateHash: ByteString) extends TransactionOutcome

case object SuccessOutcome extends TransactionOutcome

case object FailureOutcome extends TransactionOutcome
