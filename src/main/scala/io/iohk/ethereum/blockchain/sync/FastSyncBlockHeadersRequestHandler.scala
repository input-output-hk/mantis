package io.iohk.ethereum.blockchain.sync

import akka.actor.{ActorRef, Props, Scheduler}
import akka.agent.Agent
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.utils.{BlockchainStatus, NodeStatus}

class FastSyncBlockHeadersRequestHandler(
    peer: ActorRef,
    block: BigInt,
    maxHeaders: Int,
    nodeStatusHolder: Agent[NodeStatus],
    blockchain: Blockchain)(implicit scheduler: Scheduler)
  extends FastSyncRequestHandler[GetBlockHeaders, BlockHeaders](peer) {

  override val requestMsg = GetBlockHeaders(Left(block), maxHeaders, 0, reverse = false)
  override val responseMsgCode = BlockHeaders.code

  override def handleResponseMsg(blockHeaders: BlockHeaders): Unit = {
    val blockHashes = blockHeaders.headers.map(_.hash)

    val (blockHashesObtained, blockHeadersObtained) = blockHashes.zip(blockHeaders.headers).takeWhile{ case (hash, header) =>
      val parentTd: Option[BigInt] = blockchain.getTotalDifficultyByHash(header.parentHash)
      parentTd foreach { parentTotalDifficulty =>
        blockchain.save(header)
        blockchain.save(hash, parentTotalDifficulty + header.difficulty)
      }
      parentTd.isDefined
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
            nodeStatusHolder: Agent[NodeStatus], blockchain: Blockchain)
           (implicit scheduler: Scheduler): Props =
    Props(new FastSyncBlockHeadersRequestHandler(peer, block, maxHeaders, nodeStatusHolder, blockchain))
}
