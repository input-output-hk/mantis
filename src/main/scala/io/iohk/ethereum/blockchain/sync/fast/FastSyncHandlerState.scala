package io.iohk.ethereum.blockchain.sync.fast

import java.time.Instant

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.fast.FastSync._
import io.iohk.ethereum.blockchain.sync.fast.FastSyncHandlerState.{ HandlerAndNodes, HandlerAndQueue, PendingNodes }
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.utils.Config.SyncConfig

case class FastSyncHandlerState(
  syncState: FastSyncState,
  requestedHeaders: Map[Peer, BigInt] = Map.empty,
  assignedHandlers: Map[ActorRef, Peer] = Map.empty,
  peerRequestsTime: Map[Peer, Instant] = Map.empty,
  requestedMptNodes: HandlerAndNodes = Map.empty,
  requestedNonMptNodes: HandlerAndNodes = Map.empty,
  requestedBlockBodies: HandlerAndQueue = Map.empty,
  requestedReceipts: HandlerAndQueue = Map.empty
) {

  def withSyncState(state: FastSyncState): FastSyncHandlerState = copy(syncState = state)

  def withRequestedHeaders(headers: Map[Peer, BigInt]): FastSyncHandlerState = copy(requestedHeaders = headers)

  def removeHandler(handler: ActorRef): FastSyncHandlerState = copy(assignedHandlers = assignedHandlers - handler)

  def removeFromNodes(requester: ActorRef): FastSyncHandlerState =
    copy(requestedMptNodes = requestedMptNodes - requester, requestedNonMptNodes = requestedNonMptNodes - requester)

  def withRequestedBlockBodies(bodies: HandlerAndQueue): FastSyncHandlerState = copy(requestedBlockBodies = bodies)

  def withRequestedReceipts(receipts: HandlerAndQueue): FastSyncHandlerState = copy(requestedReceipts = receipts)

  def shouldRequestBlockHeaders: Boolean = requestedHeaders.isEmpty && syncState.bestBlockDoesNotReachDownloadTarget

  def updateTargetBlock(target: BlockHeader, safeBlocksCount: Int, failures: Boolean): FastSyncHandlerState =
    withSyncState(syncState.updateTargetBlock(target, safeBlocksCount, updateFailures = failures))

  def updateValidationState(header: BlockHeader, syncConfig: SyncConfig): FastSyncHandlerState = {
    import syncConfig.{ fastSyncBlockValidationK => K, fastSyncBlockValidationX => X }
    withSyncState(syncState.updateNextBlockToValidate(header, K, X))
  }

  def withUpdatingTargetBlock(updating: Boolean): FastSyncHandlerState =
    withSyncState(syncState.copy(updatingTargetBlock = updating))

  def increaseUpdateFailures(): FastSyncHandlerState =
    withSyncState(syncState.copy(targetBlockUpdateFailures = syncState.targetBlockUpdateFailures + 1))

  def updateTargetSyncState(
    state: FinalBlockProcessingResult,
    target: BlockHeader,
    syncConfig: SyncConfig
  ): (FastSyncHandlerState, String) = {
    lazy val downloadTarget = syncState.safeDownloadTarget
    lazy val safeBlocksCount = syncConfig.fastSyncBlockValidationX
    val number = target.number

    state match {
      case ImportedLastBlock =>
        val block = syncState.targetBlock
        if (number - block.number <= syncConfig.maxTargetDifference) {
          val logMsg = "Current target block is fresh enough, starting state download"
          (withSyncState(syncState.copy(pendingMptNodes = Seq(StateMptNodeHash(block.stateRoot)))), logMsg)
        } else {
          val logMsg = s"Changing target block to $number, new safe target is $downloadTarget"
          (updateTargetBlock(target, safeBlocksCount, failures = false), logMsg)
        }

      case LastBlockValidationFailed =>
        val logMsg = s"Changing target block after failure, to $number, new safe target is $downloadTarget"
        (updateTargetBlock(target, safeBlocksCount, failures = true), logMsg)
    }
  }

  def updateBestBlockNumber(header: BlockHeader): FastSyncHandlerState = {
    val hashes = Seq(header.hash)
    val newSyncState = syncState.enqueueBlockBodies(hashes).enqueueReceipts(hashes)

    if (header.number > newSyncState.bestBlockHeaderNumber) {
      withSyncState(newSyncState.setBestBlockNumber(header.number))
    } else {
      withSyncState(newSyncState)
    }
  }

  def withEnqueueBlockBodies(bodies: Seq[ByteString]): FastSyncHandlerState =
    withSyncState(syncState.enqueueBlockBodies(bodies))

  def withEnqueueReceipts(receipts: Seq[ByteString]): FastSyncHandlerState =
    withSyncState(syncState.enqueueReceipts(receipts))

  def isFullySynced: Boolean =
    syncState.bestBlockHeaderNumber >= syncState.safeDownloadTarget && !syncState.anythingQueued && assignedHandlers.isEmpty

  def nextBestBlockNumber: BigInt = syncState.bestBlockHeaderNumber + 1

  def getRequestedNodes(requester: ActorRef): Seq[HashType] =
    requestedMptNodes.getOrElse(requester, Nil) ++ requestedNonMptNodes.getOrElse(requester, Nil)

  def getPendingNodes(nodesPerRequest: Int): PendingNodes = {
    val (nonMptNodesToGet, remainingNonMptNodes) = syncState.pendingNonMptNodes.splitAt(nodesPerRequest)
    val (mptNodesToGet, remainingMptNodes) = syncState.pendingMptNodes.splitAt(nodesPerRequest - nonMptNodesToGet.size)
    PendingNodes((mptNodesToGet, nonMptNodesToGet), (remainingMptNodes, remainingNonMptNodes))
  }

  def withNodes(handler: ActorRef, nodesPerRequest: Int, pendingNodes: PendingNodes): FastSyncHandlerState = {
    val PendingNodes((mptNodesToGet, nonMptNodesToGet), (remainingMptNodes, remainingNonMptNodes)) = pendingNodes

    copy(
      syncState = syncState.copy(pendingNonMptNodes = remainingNonMptNodes, pendingMptNodes = remainingMptNodes),
      requestedMptNodes = requestedMptNodes + (handler -> mptNodesToGet),
      requestedNonMptNodes = requestedNonMptNodes + (handler -> nonMptNodesToGet)
    )
  }

  def withBlockBodies(handler: ActorRef, remaining: Seq[ByteString], toGet: Seq[ByteString]): FastSyncHandlerState =
    copy(
      syncState = syncState.copy(blockBodiesQueue = remaining),
      requestedBlockBodies = requestedBlockBodies + (handler -> toGet)
    )

  def withReceipts(handler: ActorRef, remaining: Seq[ByteString], toGet: Seq[ByteString]): FastSyncHandlerState = copy(
    syncState = syncState.copy(receiptsQueue = remaining),
    requestedReceipts = requestedReceipts + (handler -> toGet)
  )

  def withHandlerAndPeer(handler: ActorRef, peer: Peer): FastSyncHandlerState = copy(
    assignedHandlers = assignedHandlers + (handler -> peer),
    peerRequestsTime = peerRequestsTime + (peer -> Instant.now())
  )

  def withNodeData(pendingNodes: Seq[HashType], downloaded: Int, total: Int): FastSyncHandlerState =
    withSyncState(syncState.addPendingNodes(pendingNodes).copy(downloadedNodesCount = downloaded, totalNodesCount = total))

  def persistSyncState(): FastSyncHandlerState = {
    def mapValuesToHashes[T](map: Map[ActorRef, Seq[T]]): Seq[T] = map.values.flatten.toSeq.distinct

    withSyncState(syncState.copy(
      pendingMptNodes = mapValuesToHashes(requestedMptNodes) ++ syncState.pendingMptNodes,
      pendingNonMptNodes = mapValuesToHashes(requestedNonMptNodes) ++ syncState.pendingNonMptNodes,
      blockBodiesQueue = mapValuesToHashes(requestedBlockBodies) ++ syncState.blockBodiesQueue,
      receiptsQueue = mapValuesToHashes(requestedReceipts) ++ syncState.receiptsQueue
    ))
  }

  def withPeerAndHandlerRemoved(peer: Peer, handler: ActorRef): FastSyncHandlerState = {
    val newSyncState = syncState
      .addPendingNodes(getRequestedNodes(handler))
      .enqueueBlockBodies(requestedBlockBodies.getOrElse(handler, Nil))
      .enqueueReceipts(requestedReceipts.getOrElse(handler, Nil))

    withSyncState(newSyncState)
      .removeHandler(handler)
      .removeFromNodes(handler)
      .withRequestedHeaders(requestedHeaders - peer)
      .withRequestedBlockBodies(requestedBlockBodies - handler)
      .withRequestedReceipts(requestedReceipts - handler)
  }

  /** Restarts download from a few blocks behind the current best block header, as an unexpected DB error happened */
  def reduceQueuesAndBestBlock(blockHeadersPerRequest: Int): FastSyncHandlerState = withSyncState(syncState.copy(
    blockBodiesQueue = Nil,
    receiptsQueue = Nil,
    // todo: adjust the formula to minimize re-downloaded block headers
    bestBlockHeaderNumber = (syncState.bestBlockHeaderNumber - 2 * blockHeadersPerRequest).max(0)
  ))
}

object FastSyncHandlerState {

  type HandlerAndNodes = Map[ActorRef, Seq[HashType]]
  type HandlerAndQueue = Map[ActorRef, Seq[ByteString]]

  /**
    * @param toGet     tuple of mpt nodes and non mpt nodes to get
    * @param remaining tuple of remaining mpt nodes and non mpt nodes
    */
  case class PendingNodes(toGet: (Seq[HashType], Seq[HashType]), remaining: (Seq[HashType], Seq[HashType]))

}
