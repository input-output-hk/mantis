package io.iohk.ethereum.faucet.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.faucet.FaucetStatus

object FaucetDomain {

  case class SendFundsRequest(address: Address)
  case class SendFundsResponse(txId: ByteString)
  case class StatusRequest()
  case class StatusResponse(status: FaucetStatus)

}
