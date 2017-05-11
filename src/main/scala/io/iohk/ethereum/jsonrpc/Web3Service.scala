package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.crypto
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.utils.Config

import scala.concurrent.Future

object Web3Service {
  case class Sha3Request(data: ByteString)
  case class Sha3Response(data: ByteString)

  case class ClientVersionRequest()
  case class ClientVersionResponse(value: String)

  case class SubmitHashRateRequest(hashRate: BigInt, id: ByteString)
  case class SubmitHashRateResponse(success: Boolean)

  case class GetWorkRequest()
  case class GetWorkResponse(powHeaderHash: ByteString, dagSeed: ByteString, target: ByteString)

  case class SubmitWorkRequest(nonce: ByteString, powHeaderHash: ByteString, mixHash: ByteString)
  case class SubmitWorkResponse(success:Boolean)
}

class Web3Service {
  import Web3Service._

  def sha3(req: Sha3Request): Future[Sha3Response] = {
    Future.successful(Sha3Response(crypto.kec256(req.data)))
  }

  def clientVersion(req: ClientVersionRequest): Future[ClientVersionResponse] = {
    Future.successful(ClientVersionResponse(Config.clientVersion))
  }

  def submitHashRate(req: SubmitHashRateRequest): Future[SubmitHashRateResponse] = {
    //todo do we care about hash rate for now?
    Future.successful(SubmitHashRateResponse(true))
  }

  def getWork(req: GetWorkRequest): Future[GetWorkResponse] = {
    import io.iohk.ethereum.mining.pow.PowCache._
    //todo add logic for generating block for mining and generating powHeaderHash for block
    val blockNumber = 5000
    Future.successful(GetWorkResponse(
      powHeaderHash = ByteString(Hex.decode("de09f39b6f4f611b60e0ea7aceab7ca334bd35da94ed971f561bb75f6cab4ccf")),
      dagSeed = seedForBlock(blockNumber),
      target = ByteString(Hex.decode("00000ffffffa2b84b57eb59d5f3c8c8c87b4fd803357e2c582f01912a4c72e38"))
    ))
  }

  def submitWork(req: SubmitWorkRequest): Future[SubmitWorkResponse] = {
    //todo add logic for including mined block into blockchain
    Future.successful(SubmitWorkResponse(true))
  }
}
