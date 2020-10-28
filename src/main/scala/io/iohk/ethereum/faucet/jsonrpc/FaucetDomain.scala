package io.iohk.ethereum.faucet.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcControllerCommon.JsonDecoder.NoParamsDecoder
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcControllerCommon.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.JsonMethodsImplicits
import io.iohk.ethereum.jsonrpc.JsonRpcErrors.InvalidParams
import org.json4s.JsonAST.{JArray, JObject, JString}

object FaucetDomain {

  case class SendFundsRequest(address: Address)
  object SendFundsRequest extends JsonMethodsImplicits {
    implicit val sendFundsRequestDecoder: JsonDecoder[SendFundsRequest] = {
      case Some(JArray((input: JString) :: Nil)) => extractAddress(input).map(SendFundsRequest(_))
      case _ => Left(InvalidParams())
    }
  }

  case class SendFundsResponse(txId: ByteString)
  object SendFundsResponse extends JsonMethodsImplicits {
    implicit val sendFundsResponseEncoder: JsonEncoder[SendFundsResponse] = (t: SendFundsResponse) => encodeAsHex(t.txId)
  }

  case class StatusRequest()
  object StatusRequest extends JsonMethodsImplicits {
    implicit val statusRequestDecoder: JsonDecoder[StatusRequest] = new NoParamsDecoder(StatusRequest())
  }

  case class StatusResponse(status: String)
  object StatusResponse extends JsonMethodsImplicits {
    implicit val statusEncoder: JsonEncoder[StatusResponse] = (t: StatusResponse) => JObject(
      "status" -> JString(t.status)
    )
  }
}
