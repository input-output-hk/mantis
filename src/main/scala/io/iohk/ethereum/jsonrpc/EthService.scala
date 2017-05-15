package io.iohk.ethereum.jsonrpc


import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.mining.BlockGenerator

import scala.concurrent.Future


object EthService {
  val CurrentProtocolVersion = 63

  case class ProtocolVersionRequest()
  case class ProtocolVersionResponse(value: String)

  case class SubmitHashRateRequest(hashRate: BigInt, id: ByteString)
  case class SubmitHashRateResponse(success: Boolean)

  case class GetWorkRequest()
  case class GetWorkResponse(powHeaderHash: ByteString, dagSeed: ByteString, target: ByteString)

  case class SubmitWorkRequest(nonce: ByteString, powHeaderHash: ByteString, mixHash: ByteString)
  case class SubmitWorkResponse(success:Boolean)
}

class EthService(blockGenerator: BlockGenerator) {
  import EthService._

  def protocolVersion(req: ProtocolVersionRequest): Future[ProtocolVersionResponse] =
    Future.successful(ProtocolVersionResponse(f"0x$CurrentProtocolVersion%x"))

  def submitHashRate(req: SubmitHashRateRequest): Future[SubmitHashRateResponse] = {
    //todo do we care about hash rate for now?
    Future.successful(SubmitHashRateResponse(true))
  }

  def getWork(req: GetWorkRequest): Future[GetWorkResponse] = {
    import io.iohk.ethereum.mining.pow.PowCache._
    val block = blockGenerator.generateBlockForMining()
    Future.successful(GetWorkResponse(
      powHeaderHash = ByteString(kec256(BlockHeader.getEncodedWithoutNonce(block.header))),
      dagSeed = seedForBlock(block.header.number),
      target = ByteString((BigInt(2).pow(256) / block.header.difficulty).toByteArray)
    ))
  }

  def submitWork(req: SubmitWorkRequest): Future[SubmitWorkResponse] = {
    //todo add logic for including mined block into blockchain
    Future.successful(SubmitWorkResponse(true))
  }
}
