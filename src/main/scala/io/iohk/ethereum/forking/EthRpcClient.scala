package io.iohk.ethereum.forking

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.util.ByteString
import io.circe.generic.semiauto.deriveCodec
import io.iohk.ethereum.forking.EthRpcClient.BlockResponse
import io.iohk.ethereum.forking.RpcClient.RpcResponse
import JsonCodecs._
import io.circe.Codec
import io.circe.syntax._
import cats.data.OptionT

class EthRpcClient(val name: String, node: Uri)(implicit actorSystem: ActorSystem) extends RpcClient(node, None) {
  val lastBlock: RpcResponse[Option[BlockResponse]] =
    doRequest[Option[BlockResponse]](method = "eth_getBlockByNumber", params = List("latest".asJson, false.asJson))

  val lastStableBlock: RpcResponse[Option[BlockResponse]] =
    OptionT[RpcResponse, BlockResponse](lastBlock)
      .map(last => (last.number - 12).max(0))
      .flatMapF(getBlockByNumber)
      .value

  def getBlockByNumber(nr: BigInt): RpcResponse[Option[BlockResponse]] = {
    doRequest[Option[BlockResponse]](method = "eth_getBlockByNumber", params = List(nr.asJson, false.asJson))
  }
}
object EthRpcClient {
  case class BlockResponse(number: BigInt, hash: ByteString, miner: Option[ByteString], transactions: List[ByteString])
  object BlockResponse {
    implicit val blockResponseCodec: Codec[BlockResponse] = deriveCodec
  }
}
