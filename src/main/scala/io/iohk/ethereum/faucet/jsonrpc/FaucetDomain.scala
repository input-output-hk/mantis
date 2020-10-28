package io.iohk.ethereum.faucet.jsonrpc

import akka.util.ByteString

object FucetDomain {

  case class SendFundsRequest(address: String)
  case class SendFundsResponse(jobHash: ByteString)

  case class StatusRequest()
  case class StatusResponse(status: FaucetStatus)
}
