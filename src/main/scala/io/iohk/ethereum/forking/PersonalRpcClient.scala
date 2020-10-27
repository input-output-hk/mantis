package io.iohk.ethereum.forking
import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.util.ByteString
import io.circe.{Codec, Json}
import io.circe.syntax._
import io.circe.generic.semiauto.deriveCodec
import io.iohk.ethereum.domain.{Address, UInt256}
import io.iohk.ethereum.forking.JsonCodecs._
import io.iohk.ethereum.forking.PersonalRpcClient.TRANSFER_GAS_LIMIT
import io.iohk.ethereum.forking.RpcClient.RpcResponse

class PersonalRpcClient(node: Uri)(implicit actorSystem: ActorSystem) extends RpcClient(node, None) {
  def importKey(privateKey: ByteString, password: String): RpcResponse[Address] =
    doRequest[Address]("personal_importRawKey", List(privateKey.asJson, password.asJson))

  def unlock(address: Address, password: String): RpcResponse[Boolean] =
    doRequest("personal_unlockAccount", List(address.asJson, password.asJson))

  def listAccounts(): RpcResponse[List[Address]] =
    doRequest[List[Address]]("personal_listAccounts")

  def sendTransferTransaction(
      from: Address,
      to: Address,
      value: UInt256,
      gasPrice: UInt256,
      password: String
  ): RpcResponse[ByteString] = {
    val txRequest = Json.obj(
      "from" -> from.asJson,
      "to" -> to.asJson,
      "value" -> value.asJson,
      "gas" -> TRANSFER_GAS_LIMIT.asJson,
      "gasPrice" -> gasPrice.asJson
    )
    doRequest("personal_sendTransaction", List(txRequest, password.asJson))
  }
}
object PersonalRpcClient {
  val TRANSFER_GAS_LIMIT: UInt256 = UInt256(21000)
}
