package io.iohk.ethereum.faucet.jsonrpc

import io.iohk.ethereum.faucet.jsonrpc.FaucetDomain.{SendFundsRequest, SendFundsResponse, StatusRequest, StatusResponse}
import io.iohk.ethereum.jsonrpc.JsonMethodsImplicits
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodDecoder}
import org.json4s.JsonAST.{JArray, JObject, JString}

object FaucetMethodsImplicits extends JsonMethodsImplicits {

  implicit val sendFundsRequestDecoder: JsonMethodDecoder[SendFundsRequest] = {
    case Some(JArray((input: JString) :: Nil)) => extractAddress(input).map(SendFundsRequest)
    case _ => Left(InvalidParams())
  }

  implicit val sendFundsResponseEncoder: JsonEncoder[SendFundsResponse] = (t: SendFundsResponse) => encodeAsHex(t.txId)

  implicit val statusRequestDecoder: JsonMethodDecoder[StatusRequest] = new NoParamsMethodDecoder(StatusRequest())

  implicit val statusEncoder: JsonEncoder[StatusResponse] = (t: StatusResponse) =>
    JObject(
      "status" -> JString(t.status.toString)
    )
}
