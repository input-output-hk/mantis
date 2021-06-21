package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.NewCheckpoint
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Block, Blockchain, BlockchainReader, Checkpoint}
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.utils.{ByteStringUtils, Logger}
import monix.eval.Task

class CheckpointingService(
    blockchain: Blockchain,
    blockchainReader: BlockchainReader,
    ledger: Ledger,
    checkpointBlockGenerator: CheckpointBlockGenerator,
    syncController: ActorRef
) extends Logger {

  import CheckpointingService._

  def getLatestBlock(req: GetLatestBlockRequest): ServiceResponse[GetLatestBlockResponse] = {
    lazy val bestBlockNum = blockchain.getBestBlockNumber()
    lazy val blockToReturnNum =
      if (req.checkpointingInterval != 0)
        bestBlockNum - bestBlockNum % req.checkpointingInterval
      else bestBlockNum
    lazy val isValidParent =
      req.parentCheckpoint.forall(blockchainReader.getBlockHeaderByHash(_).exists(_.number < blockToReturnNum))

    Task {
      blockchain.getBlockByNumber(blockToReturnNum)
    }.flatMap {
      case Some(b) if isValidParent =>
        Task.now(Right(GetLatestBlockResponse(Some(BlockInfo(b.hash, b.number)))))

      case Some(_) =>
        log.debug("No checkpoint candidate found for a specified parent")
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
  final case class GetLatestBlockRequest(checkpointingInterval: Int, parentCheckpoint: Option[ByteString])
  final case class GetLatestBlockResponse(block: Option[BlockInfo])
  final case class BlockInfo(hash: ByteString, number: BigInt)

  final case class PushCheckpointRequest(hash: ByteString, signatures: List[ECDSASignature])
  final case class PushCheckpointResponse()
}
