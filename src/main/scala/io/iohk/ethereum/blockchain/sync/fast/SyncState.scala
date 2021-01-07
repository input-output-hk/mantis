package io.iohk.ethereum.blockchain.sync.fast

import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeader
import scala.util.Random

case class SyncState(
    pivotBlock: BlockHeader,
    lastFullBlockNumber: BigInt = 0,
    safeDownloadTarget: BigInt = 0,
    blockBodiesQueue: Seq[ByteString] = Nil,
    receiptsQueue: Seq[ByteString] = Nil,
    downloadedNodesCount: Long = 0,
    totalNodesCount: Long = 0,
    bestBlockHeaderNumber: BigInt = 0,
    nextBlockToFullyValidate: BigInt = 1,
    pivotBlockUpdateFailures: Int = 0,
    updatingPivotBlock: Boolean = false,
    stateSyncFinished: Boolean = false
) {

  def enqueueBlockBodies(blockBodies: Seq[ByteString]): SyncState =
    copy(blockBodiesQueue = blockBodiesQueue ++ blockBodies)

  def enqueueReceipts(receipts: Seq[ByteString]): SyncState =
    copy(receiptsQueue = receiptsQueue ++ receipts)

  def blockChainWorkQueued: Boolean = blockBodiesQueue.nonEmpty || receiptsQueue.nonEmpty

  def updateNextBlockToValidate(header: BlockHeader, K: Int, X: Int): SyncState = copy(
    nextBlockToFullyValidate =
      if (bestBlockHeaderNumber >= pivotBlock.number - X)
        header.number + 1
      else
        (header.number + K / 2 + Random.nextInt(K)).min(pivotBlock.number - X)
  )

  def updateDiscardedBlocks(header: BlockHeader, N: Int): SyncState = copy(
    blockBodiesQueue = Seq.empty,
    receiptsQueue = Seq.empty,
    bestBlockHeaderNumber = (header.number - N - 1) max 0,
    nextBlockToFullyValidate = (header.number - N) max 1
  )

  def updatePivotBlock(newPivot: BlockHeader, numberOfSafeBlocks: BigInt, updateFailures: Boolean): SyncState =
    copy(
      pivotBlock = newPivot,
      safeDownloadTarget = newPivot.number + numberOfSafeBlocks,
      pivotBlockUpdateFailures = if (updateFailures) pivotBlockUpdateFailures + 1 else pivotBlockUpdateFailures,
      updatingPivotBlock = false
    )

  def isBlockchainWorkFinished: Boolean =
    bestBlockHeaderNumber >= safeDownloadTarget && !blockChainWorkQueued
}
