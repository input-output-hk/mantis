package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.NewCheckpoint
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Block, Blockchain, Checkpoint}
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.utils.{ByteStringUtils, Logger}
import monix.eval.Task

class CheckpointingService(
    blockchain: Blockchain,
    ledger: Ledger,
    checkpointBlockGenerator: CheckpointBlockGenerator,
    syncController: ActorRef
) extends Logger {

  import CheckpointingService._

  def getLatestBlock(req: GetLatestBlockRequest): ServiceResponse[GetLatestBlockResponse] = {
    lazy val bestBlockNum = blockchain.getBestBlockNumber()
    lazy val blockToReturnNum = bestBlockNum - bestBlockNum % req.checkpointingInterval
    lazy val isValidParent = req.parentCheckpoint.forall(blockchain.getBlockHeaderByHash(_).isDefined)

    Task {
      blockchain.getBlockByNumber(blockToReturnNum)
    }.flatMap {
      case Some(b) if isValidParent =>
        Task.now(Right(GetLatestBlockResponse(Some(BlockInfo(b.hash, b.number)))))

      case Some(_) =>
        log.debug("Parent checkpoint is not found in a local blockchain")
        Task.now(Right(GetLatestBlockResponse(None)))

      case None =>
        log.error(
          s"Failed to retrieve block for checkpointing: block at number $blockToReturnNum was unavailable " +
            s"even though best block number was $bestBlockNum (re-org occurred?)"
        )
        getLatestBlock(req) // this can fail only during a re-org, so we just try again
    }
  }

  def pushCheckpoint(req: PushCheckpointRequest): ServiceResponse[PushCheckpointResponse] = Task {
    val parentHash = req.hash

    ledger.getBlockByHash(parentHash) match {
      case Some(parent) =>
        val checkpointBlock: Block = checkpointBlockGenerator.generate(parent, Checkpoint(req.signatures))
        syncController ! NewCheckpoint(checkpointBlock)

      case None =>
        log.error(s"Could not find parent (${ByteStringUtils.hash2string(parentHash)}) for new checkpoint block")
    }
    Right(PushCheckpointResponse())
  }
}

object CheckpointingService {
  case class GetLatestBlockRequest(checkpointingInterval: Int, parentCheckpoint: Option[ByteString])
  case class GetLatestBlockResponse(block: Option[BlockInfo])
  case class BlockInfo(hash: ByteString, number: BigInt)

  case class PushCheckpointRequest(hash: ByteString, signatures: List[ECDSASignature])
  case class PushCheckpointResponse()
}
