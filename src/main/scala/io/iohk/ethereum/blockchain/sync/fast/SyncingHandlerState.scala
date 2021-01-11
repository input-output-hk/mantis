package io.iohk.ethereum.blockchain.sync.fast

import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeader
import scala.util.Random

case class SyncingHandlerState(
    lastFullBlockNumber: BigInt = 0,
    blockBodiesQueue: Seq[ByteString] = Nil,
    receiptsQueue: Seq[ByteString] = Nil,
    downloadedNodesCount: Long = 0,
    totalNodesCount: Long = 0,
    bestBlockHeaderNumber: BigInt = 0,
    nextBlockToFullyValidate: BigInt = 1
) {
  def enqueueBlockBodies(blockBodies: Seq[ByteString]): SyncingHandlerState =
    copy(blockBodiesQueue = blockBodiesQueue ++ blockBodies)

  def enqueueReceipts(receipts: Seq[ByteString]): SyncingHandlerState =
    copy(receiptsQueue = receiptsQueue ++ receipts)

  def blockChainWorkQueued: Boolean = blockBodiesQueue.nonEmpty || receiptsQueue.nonEmpty

  def updateNextBlockToValidate(header: BlockHeader, pivot: BlockHeader, K: Int, X: Int): SyncingHandlerState = copy(
    nextBlockToFullyValidate =
      if (bestBlockHeaderNumber >= pivot.number - X)
        header.number + 1
      else
        (header.number + K / 2 + Random.nextInt(K)).min(pivot.number - X)
  )

  def updateDiscardedBlocks(header: BlockHeader, N: Int): SyncingHandlerState = copy(
    blockBodiesQueue = Seq.empty,
    receiptsQueue = Seq.empty,
    bestBlockHeaderNumber = (header.number - N - 1) max 0,
    nextBlockToFullyValidate = (header.number - N) max 1
  )

  def isBlockchainWorkFinished(safeDownloadTarget: BigInt): Boolean =
    bestBlockHeaderNumber >= safeDownloadTarget && !blockChainWorkQueued
}

object SyncingHandlerState {

  def fromPersistentState(persistentState: PersistentSyncState): SyncingHandlerState = {
    SyncingHandlerState(
      persistentState.lastFullBlockNumber,
      persistentState.blockBodiesQueue,
      persistentState.receiptsQueue,
      persistentState.downloadedNodesCount,
      persistentState.totalNodesCount,
      persistentState.bestBlockHeaderNumber,
      persistentState.nextBlockToFullyValidate
    )
  }
}
