package io.iohk.ethereum.blockchain.sync

import akka.actor.{Props, ActorRef}
import akka.agent.Agent
import io.iohk.ethereum.db.storage.BlockHeadersStorage
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.utils.{BlockchainStatus, NodeStatus}

class FastSyncBlockHeadersRequestHandler(
    peer: ActorRef,
    block: BigInt,
    maxHeaders: Int,
    nodeStatusHolder: Agent[NodeStatus],
    blockHeadersStorage: BlockHeadersStorage)
  extends FastSyncRequestHandler[GetBlockHeaders, BlockHeaders](peer) {

  override val requestMsg = GetBlockHeaders(Left(block), maxHeaders, 0, reverse = false)
  override val responseMsgCode = BlockHeaders.code

  override def handleResponseMsg(blockHeaders: BlockHeaders): Unit = {
    val blockHashes = blockHeaders.headers.map(_.hash)

    (blockHashes zip blockHeaders.headers).foreach { case (hash, header) =>
      blockHeadersStorage.put(hash, header)
    }

    blockHeaders.headers.lastOption foreach { lastHeader =>
      nodeStatusHolder.send(_.copy(
        blockchainStatus = BlockchainStatus(lastHeader.difficulty, lastHeader.hash, lastHeader.number)))
    }

    if (blockHashes.nonEmpty) {
      fastSyncController ! FastSyncController.EnqueueBlockBodies(blockHashes)
      fastSyncController ! FastSyncController.EnqueueReceipts(blockHashes)
    } else {
      fastSyncController ! BlacklistSupport.BlacklistPeer(peer)
    }

    log.info("Received {} block headers in {} ms", blockHeaders.headers.size, timeTakenSoFar())
    cleanupAndStop()
  }

  override def handleTimeout(): Unit = {
    fastSyncController ! BlacklistSupport.BlacklistPeer(peer)
    cleanupAndStop()
  }
}

object FastSyncBlockHeadersRequestHandler {
  def props(peer: ActorRef, block: BigInt, maxHeaders: Int,
            nodeStatusHolder: Agent[NodeStatus], blockHeadersStorage: BlockHeadersStorage): Props =
    Props(new FastSyncBlockHeadersRequestHandler(peer, block, maxHeaders, nodeStatusHolder, blockHeadersStorage))
}
