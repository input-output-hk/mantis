package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.crypto
import org.spongycastle.util.encoders.Hex

import scala.concurrent.Future

object Web3Service {
  case class Sha3Request(data: ByteString)
  case class Sha3Response(data: ByteString)

  case class ClientVersionRequest()
  case class ClientVersionResponse(value: String)

  case class HashRateSubmitRequest(hashRate: ByteString, id: ByteString)
  case class HashRateSubmitResponse(success: Boolean)

  case class GetWorkRequest()
  case class GetWorkResponse(powHeaderHash: ByteString, dagSeed: ByteString, target: ByteString)
}

class Web3Service {
  import Web3Service._

  def sha3(req: Sha3Request): Future[Sha3Response] = {
    Future.successful(Sha3Response(crypto.kec256(req.data)))
  }

  def clientVersion(req: ClientVersionRequest): Future[ClientVersionResponse] = {
    Future.successful(ClientVersionResponse("etc-client/v0.1"))
  }

  def submitHashRate(req: HashRateSubmitRequest): Future[HashRateSubmitResponse] = {
    //todo add logic
    Future.successful(HashRateSubmitResponse(true))
  }

  def getWork(req: GetWorkRequest): Future[GetWorkResponse] = {
    //todo add logic
    Future.successful(GetWorkResponse(
      ByteString(Hex.decode("de09f39b6f4f611b60e0ea7aceab7ca334bd35da94ed971f561bb75f6cab4ccf")),
      ByteString(Hex.decode("e586ce62651f6de0be923da595f6773a2d1bd9b41ca0ad927456061ed15c0a14")),
      ByteString(Hex.decode("00000000000a2b84b57eb59d5f3c8c8c87b4fd803357e2c582f01912a4c72e38"))
    ))
  }
}
