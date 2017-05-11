package io.iohk.ethereum.jsonrpc


import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.Web3Service._
import org.spongycastle.util.encoders.Hex

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
