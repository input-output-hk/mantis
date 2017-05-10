package io.iohk.ethereum.jsonrpc


import scala.concurrent.Future


object EthService {
  val CurrentProtocolVersion = 63

  case class ProtocolVersionRequest()
  case class ProtocolVersionResponse(value: String)
}

class EthService {
  import EthService._

  def protocolVersion(req: ProtocolVersionRequest): Future[ProtocolVersionResponse] =
    Future.successful(ProtocolVersionResponse(f"0x$CurrentProtocolVersion%x"))
}
