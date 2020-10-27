package io.iohk.ethereum.faucet

import akka.util.ByteString
import monix.eval.Task
import io.iohk.ethereum.faucet.FaucetService._

trait FaucetService {

  def sendFunds(
                 address: String
               ): Task[Either[SendFundsError, TransactionBuildJobHash]]
}

object FaucetService {
  sealed trait SendFundsError

  type TransactionBuildJobHash = ByteString //TODO: review
}
