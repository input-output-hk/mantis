package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.utils.{Config, VmConfig}

import scala.concurrent.Future

object Web3Service {
  case class Sha3Request(data: ByteString)
  case class Sha3Response(data: ByteString)

  case class ClientVersionRequest()
  case class ClientVersionResponse(value: String)
}

class Web3Service(vmConfig: VmConfig) {
  import Web3Service._

  def sha3(req: Sha3Request): ServiceResponse[Sha3Response] = {
    Future.successful(Right(Sha3Response(crypto.kec256(req.data))))
  }

  def clientVersion(req: ClientVersionRequest): ServiceResponse[ClientVersionResponse] = {
    val vmMode = vmConfig.mode.toString.toLowerCase
    val externalVmInfo = vmConfig.externalConfig.map { ext => s"-${ext.vmType}" }.getOrElse("")
    Future.successful(Right(ClientVersionResponse(s"${Config.clientVersion}/vm-$vmMode$externalVmInfo")))
  }

}
