package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.NewCheckpoint
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.utils.Logger
import monix.eval.Task

class CheckpointingService(
    blockchain: Blockchain,
    syncController: ActorRef
) extends Logger {

  import CheckpointingService._

  def getLatestBlock(req: GetLatestBlockRequest): ServiceResponse[GetLatestBlockResponse] = {
    lazy val bestBlockNum = blockchain.getBestBlockNumber()
    lazy val blockToReturnNum = bestBlockNum - bestBlockNum % req.checkpointingInterval

    Task {
      blockchain.getBlockByNumber(blockToReturnNum)
    }.flatMap {
      case Some(b) =>
        val resp = GetLatestBlockResponse(b.hash, b.number)
        Task.now(Right(resp))

      case None =>
        log.error(
          s"Failed to retrieve block for checkpointing: block at number $blockToReturnNum was unavailable " +
            s"even though best block number was $bestBlockNum (re-org occurred?)"
        )
        getLatestBlock(req) // this can fail only during a re-org, so we just try again
    }
  }

  def pushCheckpoint(req: PushCheckpointRequest): ServiceResponse[PushCheckpointResponse] = Task {
    syncController ! NewCheckpoint(req.hash, req.signatures)
    Right(PushCheckpointResponse())
  }
}

object CheckpointingService {
  case class GetLatestBlockRequest(checkpointingInterval: Int)
  case class GetLatestBlockResponse(hash: ByteString, number: BigInt)

  case class PushCheckpointRequest(hash: ByteString, signatures: List[ECDSASignature])
  case class PushCheckpointResponse()
}
