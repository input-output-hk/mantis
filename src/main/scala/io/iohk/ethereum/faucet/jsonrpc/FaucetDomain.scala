package io.iohk.ethereum.faucet.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.faucet.FaucetStatus
import io.iohk.ethereum.jsonrpc.JsonMethodsImplicits
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodDecoder}
import org.json4s.JsonAST.{JArray, JObject, JString}

object FaucetDomain {

  case class SendFundsRequest(address: Address)
  object SendFundsRequest extends JsonMethodsImplicits {
    implicit val sendFundsRequestDecoder: JsonMethodDecoder[SendFundsRequest] = {
      case Some(JArray((input: JString) :: Nil)) => extractAddress(input).map(SendFundsRequest(_))
      case _ => Left(InvalidParams())
    }
  }

  case class SendFundsResponse(txId: ByteString)
  object SendFundsResponse extends JsonMethodsImplicits {
    implicit val sendFundsResponseEncoder: JsonEncoder[SendFundsResponse] = (t: SendFundsResponse) =>
      encodeAsHex(t.txId)
  }

  case class StatusRequest()
  object StatusRequest extends JsonMethodsImplicits {
    implicit val statusRequestDecoder: JsonMethodDecoder[StatusRequest] = new NoParamsMethodDecoder(StatusRequest())
  }

  case class StatusResponse(status: FaucetStatus)
  object StatusResponse extends JsonMethodsImplicits {
    implicit val statusEncoder: JsonEncoder[StatusResponse] = (t: StatusResponse) =>
      JObject(
        "status" -> JString(t.status.toString)
      )
  }
}
