package io.iohk.ethereum.blockchain.sync

import akka.actor.{ActorRef, Props, Scheduler}
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.db.storage.{BlockHeadersStorage, TotalDifficultyStorage}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.utils.{BlockchainStatus, NodeStatus}
import org.spongycastle.util.encoders.Hex

class FastSyncBlockHeadersRequestHandler(
    peer: ActorRef,
    block: BigInt,
    maxHeaders: Int,
    nodeStatusHolder: Agent[NodeStatus],
    blockHeadersStorage: BlockHeadersStorage,
    totalDifficultyStorage: TotalDifficultyStorage)(implicit scheduler: Scheduler)
  extends FastSyncRequestHandler[GetBlockHeaders, BlockHeaders](peer) {

  override val requestMsg = GetBlockHeaders(Left(block), maxHeaders, 0, reverse = false)
  override val responseMsgCode = BlockHeaders.code

  override def handleResponseMsg(blockHeaders: BlockHeaders): Unit = {
    val blockHashes = blockHeaders.headers.map(_.hash)

    val (blockHashesObtained, blockHeadersObtained) = (blockHashes zip blockHeaders.headers).takeWhile{ case (hash, header) =>
      val parentTotalDifficulty = totalDifficultyStorage.get(header.parentHash)
      parentTotalDifficulty.foreach{ parentTD =>
        blockHeadersStorage.put(hash, header)
        totalDifficultyStorage.put(hash, parentTD + header.difficulty)
      }
      parentTotalDifficulty.isDefined
    }.unzip

    blockHeadersObtained.lastOption foreach { lastHeader =>
      nodeStatusHolder.send(_.copy(
        blockchainStatus = BlockchainStatus(lastHeader.difficulty, lastHeader.hash, lastHeader.number)))
    }

    if (blockHashesObtained.nonEmpty) {
      fastSyncController ! FastSyncController.EnqueueBlockBodies(blockHashesObtained)
      fastSyncController ! FastSyncController.EnqueueReceipts(blockHashesObtained)
    }

    if (blockHashesObtained.length != blockHashes.length) fastSyncController ! BlacklistSupport.BlacklistPeer(peer)

    log.info("Received {} block headers in {} ms", blockHashesObtained.size, timeTakenSoFar())
    cleanupAndStop()
  }

  override def handleTimeout(): Unit = {
    fastSyncController ! BlacklistSupport.BlacklistPeer(peer)
    cleanupAndStop()
  }

  override def handleTerminated(): Unit = {
    cleanupAndStop()
  }

}

object FastSyncBlockHeadersRequestHandler {
  def props(peer: ActorRef, block: BigInt, maxHeaders: Int,
            nodeStatusHolder: Agent[NodeStatus], blockHeadersStorage: BlockHeadersStorage,
            totalDifficultyStorage: TotalDifficultyStorage)
           (implicit scheduler: Scheduler): Props =
    Props(new FastSyncBlockHeadersRequestHandler(peer, block, maxHeaders, nodeStatusHolder, blockHeadersStorage, totalDifficultyStorage))
}
