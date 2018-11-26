package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSync._
import io.iohk.ethereum.domain.BlockHeader

import scala.util.Random

case class FastSyncState(
  targetBlock: BlockHeader,
  safeDownloadTarget: BigInt = 0,
  pendingMptNodes: Seq[HashType] = Nil,
  pendingNonMptNodes: Seq[HashType] = Nil,
  blockBodiesQueue: Seq[ByteString] = Nil,
  receiptsQueue: Seq[ByteString] = Nil,
  downloadedNodesCount: Int = 0,
  totalNodesCount: Int = 0,
  bestBlockHeaderNumber: BigInt = 0,
  nextBlockToFullyValidate: BigInt = 1,
  targetBlockUpdateFailures: Int = 0,
  updatingTargetBlock: Boolean = false
) {

  def enqueueBlockBodies(blockBodies: Seq[ByteString]): FastSyncState = copy(blockBodiesQueue = blockBodiesQueue ++ blockBodies)

  def enqueueReceipts(receipts: Seq[ByteString]): FastSyncState = copy(receiptsQueue = receiptsQueue ++ receipts)

  def setBestBlockNumber(block: BigInt): FastSyncState = copy(bestBlockHeaderNumber = block)

  def addPendingNodes(hashes: Seq[HashType]): FastSyncState = {
    val (mpt, nonMpt) = hashes.partition {
      case _: StateMptNodeHash | _: ContractStorageMptNodeHash => true
      case _: EvmCodeHash | _: StorageRootHash                 => false
    }
    // Nodes are prepended in order to traverse mpt in-depth.
    // For mpt nodes is not needed but to keep it consistent, it was applied too
    copy(pendingMptNodes = mpt ++ pendingMptNodes, pendingNonMptNodes = nonMpt ++ pendingNonMptNodes)
  }

  def blockChainWorkQueued: Boolean =  blockBodiesQueue.nonEmpty || receiptsQueue.nonEmpty

  def pendingNodes: Boolean = pendingNonMptNodes.nonEmpty || pendingMptNodes.nonEmpty

  def anythingQueued: Boolean = pendingNodes || blockChainWorkQueued

  def bestBlockDoesNotReachDownloadTarget: Boolean = bestBlockHeaderNumber < safeDownloadTarget

  def shouldAssignWork: Boolean = bestBlockDoesNotReachDownloadTarget || blockChainWorkQueued

  def shouldDownloadMoreItems: Boolean = anythingQueued || bestBlockDoesNotReachDownloadTarget && !updatingTargetBlock

  def updateNextBlockToValidate(header: BlockHeader, K: Int, X: Int): FastSyncState = {
    val headerNumber = header.number
    val targetOffset = targetBlock.number - X
    val newNextBlock = if (bestBlockHeaderNumber >= targetOffset) {
      headerNumber + 1
    } else {
      (headerNumber + K / 2 + Random.nextInt(K)).min(targetOffset)
    }
    copy(nextBlockToFullyValidate = newNextBlock)
  }

  def updateDiscardedBlocks(header: BlockHeader, N: Int): FastSyncState = copy(
    blockBodiesQueue = Nil,
    receiptsQueue = Nil,
    bestBlockHeaderNumber = (header.number - N - 1) max 0,
    nextBlockToFullyValidate = (header.number - N) max 1
  )

  def updateTargetBlock(newTarget: BlockHeader, numberOfSafeBlocks: BigInt, updateFailures: Boolean): FastSyncState = copy(
    targetBlock = newTarget,
    safeDownloadTarget = newTarget.number + numberOfSafeBlocks,
    targetBlockUpdateFailures = if (updateFailures) targetBlockUpdateFailures + 1 else targetBlockUpdateFailures
  )
}

