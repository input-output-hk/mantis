package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerActor}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.utils.Config.Sync._

trait FastSync {
  selfSyncController: SyncController =>

  import FastSync._
  import SyncController._

  def startFastSync(): Unit = {
    log.info("Trying to start block synchronization (fast mode)")
    fastSyncStateStorage.getSyncState() match {
      case Some(syncState) => startFastSync(syncState)
      case None => startFastSyncFromScratch()
    }
  }

  private def startingFastSync: Receive = handlePeerUpdates orElse {
    case RetryStart => startFastSync()
  }

  private def startFastSync(syncState: SyncState): Unit = {
    context become new SyncingHandler(syncState).receive
    self ! ProcessSyncing
  }

  private def startFastSyncFromScratch(): Unit = {
    val peersUsedToChooseTarget = peersToDownloadFrom.filter(_._2.forkAccepted)

    if (peersUsedToChooseTarget.size >= minPeersToChooseTargetBlock) {
      peersUsedToChooseTarget.foreach { case (peer, PeerInfo(status, _, _, _)) =>
        peerEventBus ! Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id)))
        etcPeerManager ! EtcPeerManagerActor.SendMessage(GetBlockHeaders(Right(status.bestHash), 1, 0, reverse = false), peer.id)
      }
      log.debug("Asking {} peers for block headers", peersUsedToChooseTarget.size)
      val timeout = scheduler.scheduleOnce(peerResponseTimeout, self, BlockHeadersTimeout)
      context become waitingForBlockHeaders(peersUsedToChooseTarget.keySet, Map.empty, timeout)
    } else {
      log.info("Block synchronization (fast mode) not started. Need at least {} peers, but there are only {} available at the moment. Retrying in {}",
        minPeersToChooseTargetBlock, peersUsedToChooseTarget.size, startRetryInterval)
      scheduleStartRetry(startRetryInterval)
      context become startingFastSync
    }
  }

  private def waitingForBlockHeaders(waitingFor: Set[Peer],
                             received: Map[Peer, BlockHeader],
                             timeout: Cancellable): Receive = handlePeerUpdates orElse {
    case MessageFromPeer(BlockHeaders(Seq(blockHeader)), peerId) =>
      peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peerId)))

      val newWaitingFor = waitingFor.filterNot(_.id == peerId)

      waitingFor.find(_.id == peerId).foreach { peer =>
        val newReceived = received + (peer -> blockHeader)

        if (newWaitingFor.isEmpty) {
          timeout.cancel()
          tryStartFastSync(newReceived)
        } else context become waitingForBlockHeaders(newWaitingFor, newReceived, timeout)
      }

    case MessageFromPeer(BlockHeaders(blockHeaders), peerId) =>
      peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peerId)))
      blacklist(peerId, blacklistDuration,s"did not respond with 1 header but with ${blockHeaders.size}, blacklisting for $blacklistDuration")
      waitingFor.find(_.id == peerId).foreach { peer =>
        context become waitingForBlockHeaders(waitingFor - peer, received, timeout)
      }

    case BlockHeadersTimeout =>
      waitingFor.foreach { peer =>
        peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id)))
        blacklist(peer.id, blacklistDuration, s"did not respond within required time with block header, blacklisting for $blacklistDuration")
      }
      tryStartFastSync(received)
  }

  private def tryStartFastSync(receivedHeaders: Map[Peer, BlockHeader]): Unit = {
    log.debug("Trying to start fast sync. Received {} block headers", receivedHeaders.size)
    if (receivedHeaders.size >= minPeersToChooseTargetBlock) {
      val (mostUpToDatePeer, mostUpToDateBlockHeader) = receivedHeaders.maxBy(_._2.number)
      val targetBlock = mostUpToDateBlockHeader.number - targetBlockOffset

      if (targetBlock < 1) {
        log.debug("Target block is less than 1, starting regular sync")
        appStateStorage.fastSyncDone()
        context become idle
        self ! FastSyncDone
      } else {
        log.debug("Starting fast sync. Asking peer {} for target block header ({})", mostUpToDatePeer.id, targetBlock)

        peerEventBus ! Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(mostUpToDatePeer.id)))
        etcPeerManager ! EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(targetBlock), 1, 0, reverse = false), mostUpToDatePeer.id)
        val timeout = scheduler.scheduleOnce(peerResponseTimeout, self, TargetBlockTimeout)
        context become waitingForTargetBlock(mostUpToDatePeer, targetBlock, timeout)
      }

    } else {
      log.info("Block synchronization (fast mode) not started. Need to receive block headers from at least {} peers, but received only from {}. Retrying in {}",
        minPeersToChooseTargetBlock, receivedHeaders.size, startRetryInterval)
      scheduleStartRetry(startRetryInterval)
      context become startingFastSync
    }
  }

  def waitingForTargetBlock(peer: Peer,
                            targetBlockNumber: BigInt,
                            timeout: Cancellable): Receive = handlePeerUpdates orElse {
    case MessageFromPeer(blockHeaders: BlockHeaders, peerId) =>
      timeout.cancel()
      peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id)))

      val targetBlockHeaderOpt = blockHeaders.headers.find(header => header.number == targetBlockNumber)
      targetBlockHeaderOpt match {
        case Some(targetBlockHeader) =>
          log.info("Starting block synchronization (fast mode)")
          val initialSyncState = SyncState(targetBlockHeader,
            mptNodesQueue = Seq(StateMptNodeHash(targetBlockHeader.stateRoot)))
          startFastSync(initialSyncState)

        case None =>
          blacklist(peer.id, blacklistDuration, s"did not respond with target block header, blacklisting and scheduling retry in $startRetryInterval")
          log.info("Block synchronization (fast mode) not started. Target block header not received. Retrying in {}", startRetryInterval)
          scheduleStartRetry(startRetryInterval)
          context become startingFastSync
      }

    case TargetBlockTimeout =>
      blacklist(peer.id, blacklistDuration, s"did not respond with target block header (timeout), blacklisting and scheduling retry in $startRetryInterval")
      log.info("Block synchronization (fast mode) not started. Target block header receive timeout. Retrying in {}", startRetryInterval)
      peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id)))
      scheduleStartRetry(startRetryInterval)
      context become startingFastSync
  }

  def scheduleStartRetry(interval: FiniteDuration): Unit = {
    scheduler.scheduleOnce(interval, self, RetryStart)
  }

  private class SyncingHandler(initialSyncState: SyncState) {

    private val blockHeadersHandlerName = "block-headers-request-handler"

    private var mptNodesQueue: Seq[HashType] = initialSyncState.mptNodesQueue
    private var nonMptNodesQueue: Seq[HashType] = initialSyncState.nonMptNodesQueue
    private var blockBodiesQueue: Seq[ByteString] = initialSyncState.blockBodiesQueue
    private var receiptsQueue: Seq[ByteString] = initialSyncState.receiptsQueue
    private var downloadedNodesCount: Int = initialSyncState.downloadedNodesCount
    private var bestBlockHeaderNumber: BigInt = initialSyncState.bestBlockHeaderNumber

    private var assignedHandlers: Map[ActorRef, Peer] = Map.empty

    private val syncStateStorageActor = context.actorOf(Props[FastSyncStateActor], "state-storage")

    private var requestedMptNodes: Map[ActorRef, Seq[HashType]] = Map.empty
    private var requestedNonMptNodes: Map[ActorRef, Seq[HashType]] = Map.empty
    private var requestedBlockBodies: Map[ActorRef, Seq[ByteString]] = Map.empty
    private var requestedReceipts: Map[ActorRef, Seq[ByteString]] = Map.empty

    syncStateStorageActor ! fastSyncStateStorage

    private val syncStatePersistCancellable =
      scheduler.schedule(persistStateSnapshotInterval, persistStateSnapshotInterval) {
        syncStateStorageActor ! SyncState(
          initialSyncState.targetBlock,
          requestedMptNodes.values.flatten.toSeq.distinct ++ mptNodesQueue,
          requestedNonMptNodes.values.flatten.toSeq.distinct ++ nonMptNodesQueue,
          requestedBlockBodies.values.flatten.toSeq.distinct ++ blockBodiesQueue,
          requestedReceipts.values.flatten.toSeq.distinct ++ receiptsQueue,
          downloadedNodesCount,
          bestBlockHeaderNumber)
      }

    def receive: Receive = handlePeerUpdates orElse {
      case EnqueueNodes(hashes) =>
        hashes.foreach {
          case h: EvmCodeHash => nonMptNodesQueue = h +: nonMptNodesQueue
          case h: StorageRootHash => nonMptNodesQueue = h +: nonMptNodesQueue
          case h: StateMptNodeHash => mptNodesQueue = h +: mptNodesQueue
          case h: ContractStorageMptNodeHash => mptNodesQueue = h +: mptNodesQueue
        }

      case EnqueueBlockBodies(hashes) =>
        blockBodiesQueue ++= hashes

      case EnqueueReceipts(hashes) =>
        receiptsQueue ++= hashes

      case UpdateDownloadedNodesCount(num) =>
        downloadedNodesCount += num

      case BlockBodiesReceived(_, requestedHashes, blockBodies) =>
        insertBlocks(requestedHashes, blockBodies)

      case BlockHeadersReceived(_, headers) =>
        insertHeaders(headers)

      case ProcessSyncing =>
        processSyncing()

      case SyncRequestHandler.Done =>
        context unwatch sender()
        assignedHandlers -= sender()
        cleanupRequestedMaps(sender())
        processSyncing()

      case Terminated(ref) if assignedHandlers.contains(ref) =>
        context unwatch ref
        assignedHandlers -= ref
        mptNodesQueue ++= requestedMptNodes.getOrElse(ref, Nil)
        nonMptNodesQueue ++= requestedNonMptNodes.getOrElse(ref, Nil)
        blockBodiesQueue ++= requestedBlockBodies.getOrElse(ref, Nil)
        receiptsQueue ++= requestedReceipts.getOrElse(ref, Nil)
        cleanupRequestedMaps(ref)

      case PrintStatus =>
        val totalNodesCount = downloadedNodesCount + mptNodesQueue.size + nonMptNodesQueue.size
        log.info(
          s"""|Block: ${appStateStorage.getBestBlockNumber()}/${initialSyncState.targetBlock.number}.
              |Peers: ${assignedHandlers.size}/${handshakedPeers.size} (${blacklistedPeers.size} blacklisted).
              |State: $downloadedNodesCount/$totalNodesCount known nodes""".stripMargin.replace("\n", " "))
    }

    private def cleanupRequestedMaps(handler: ActorRef): Unit = {
      requestedMptNodes = requestedMptNodes - handler
      requestedNonMptNodes = requestedNonMptNodes - handler
      requestedBlockBodies = requestedBlockBodies - handler
      requestedReceipts = requestedReceipts - handler
    }

    private def insertBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): Unit = {
      //todo this is moved from FastSyncBlockBodiesRequestHandler.scala we should add block validation here [EC-249]
      //load header from chain by hash and check consistency with BlockValidator.validateHeaderAndBody
      //if invalid blacklist peer
      (requestedHashes zip blockBodies).foreach { case (hash, body) =>
        blockchain.save(hash, body)
      }

      val receivedHashes = requestedHashes.take(blockBodies.size)
      updateBestBlockIfNeeded(receivedHashes)
      val remainingBlockBodies = requestedHashes.drop(blockBodies.size)
      if (remainingBlockBodies.nonEmpty) {
        self ! FastSync.EnqueueBlockBodies(remainingBlockBodies)
      }
    }

    private def insertHeaders(headers: Seq[BlockHeader]): Unit = {
      val blockHeadersObtained = headers.takeWhile { header =>
        val parentTd: Option[BigInt] = blockchain.getTotalDifficultyByHash(header.parentHash)
        parentTd foreach { parentTotalDifficulty =>
          blockchain.save(header)
          blockchain.save(header.hash, parentTotalDifficulty + header.difficulty)
        }
        parentTd.isDefined
      }

      blockHeadersObtained.lastOption.foreach { lastHeader =>
        if (lastHeader.number > bestBlockHeaderNumber) {
          bestBlockHeaderNumber = lastHeader.number
        }
      }
    }

    def processSyncing(): Unit = {
      if (fullySynced) {
        finishFastSync()
      } else {
        if (anythingToDownload) processDownloads()
        else log.debug("No more items to request, waiting for {} responses", assignedHandlers.size)
      }
    }

    def finishFastSync(): Unit = {
      log.info("Block synchronization in fast mode finished, switching to regular mode")
      cleanup()
      appStateStorage.fastSyncDone()
      context become idle
      self ! FastSyncDone
    }

    def cleanup(): Unit = {
      syncStatePersistCancellable.cancel()
      syncStateStorageActor ! PoisonPill
      fastSyncStateStorage.purge()
    }

    def processDownloads(): Unit = {
      if (unassignedPeers.isEmpty) {
        if (assignedHandlers.nonEmpty) {
          log.debug("There are no available peers, waiting for responses")
        } else {
          log.debug("There are no peers to download from, scheduling a retry in {}", syncRetryInterval)
          scheduler.scheduleOnce(syncRetryInterval, self, ProcessSyncing)
        }
      } else {
        unassignedPeers
          .take(maxConcurrentRequests - assignedHandlers.size)
          .foreach(assignWork)
      }
    }

    def assignWork(peer: Peer): Unit = {
      if (receiptsQueue.nonEmpty) {
        requestReceipts(peer)
      } else if (blockBodiesQueue.nonEmpty) {
        requestBlockBodies(peer)
      } else if (context.child(blockHeadersHandlerName).isEmpty &&
        initialSyncState.targetBlock.number > bestBlockHeaderNumber) {
        requestBlockHeaders(peer)
      } else if (nonMptNodesQueue.nonEmpty || mptNodesQueue.nonEmpty) {
        requestNodes(peer)
      }
    }

    def requestReceipts(peer: Peer): Unit = {
      val (receiptsToGet, remainingReceipts) = receiptsQueue.splitAt(receiptsPerRequest)
      val handler = context.actorOf(FastSyncReceiptsRequestHandler.props(
        peer, etcPeerManager, peerEventBus, receiptsToGet, appStateStorage, blockchain))
      context watch handler
      assignedHandlers += (handler -> peer)
      receiptsQueue = remainingReceipts
      requestedReceipts += handler -> receiptsToGet
    }

    def requestBlockBodies(peer: Peer): Unit = {
      val (blockBodiesToGet, remainingBlockBodies) = blockBodiesQueue.splitAt(blockBodiesPerRequest)
      val handler = context.actorOf(SyncBlockBodiesRequestHandler.props(peer, etcPeerManager, peerEventBus, blockBodiesToGet))
      context watch handler
      assignedHandlers += (handler -> peer)
      blockBodiesQueue = remainingBlockBodies
      requestedBlockBodies += handler -> blockBodiesToGet
    }

    def requestBlockHeaders(peer: Peer): Unit = {
      val limit: BigInt = if (blockHeadersPerRequest < (initialSyncState.targetBlock.number - bestBlockHeaderNumber))
        blockHeadersPerRequest
      else
        initialSyncState.targetBlock.number - bestBlockHeaderNumber

      val request = GetBlockHeaders(Left(bestBlockHeaderNumber + 1), limit, skip = 0, reverse = false)
      val handler = context.actorOf(
        SyncBlockHeadersRequestHandler.props(peer, etcPeerManager, peerEventBus, request, resolveBranches = false),
        blockHeadersHandlerName)
      context watch handler
      assignedHandlers += (handler -> peer)
    }

    def requestNodes(peer: Peer): Unit = {
      val (nonMptNodesToGet, remainingNonMptNodes) = nonMptNodesQueue.splitAt(nodesPerRequest)
      val (mptNodesToGet, remainingMptNodes) = mptNodesQueue.splitAt(nodesPerRequest - nonMptNodesToGet.size)
      val nodesToGet = nonMptNodesToGet ++ mptNodesToGet
      val handler = context.actorOf(FastSyncNodesRequestHandler.props(peer, etcPeerManager, peerEventBus, nodesToGet, blockchain,
        blockchainStorages.nodesKeyValueStorageFor(Some(initialSyncState.targetBlock.number))))
      context watch handler
      assignedHandlers += (handler -> peer)
      nonMptNodesQueue = remainingNonMptNodes
      mptNodesQueue = remainingMptNodes
      requestedMptNodes += handler -> mptNodesToGet
      requestedNonMptNodes += handler -> nonMptNodesToGet
    }

    def unassignedPeers: Set[Peer] = peersToDownloadFrom.keySet diff assignedHandlers.values.toSet

    def anythingToDownload: Boolean =
      anythingQueued || bestBlockHeaderNumber < initialSyncState.targetBlock.number

    def anythingQueued: Boolean =
      nonMptNodesQueue.nonEmpty ||
      mptNodesQueue.nonEmpty ||
      blockBodiesQueue.nonEmpty ||
      receiptsQueue.nonEmpty

    def fullySynced: Boolean = {
      bestBlockHeaderNumber >= initialSyncState.targetBlock.number &&
      !anythingQueued &&
      assignedHandlers.isEmpty
    }
  }

  private def updateBestBlockIfNeeded(receivedHashes: Seq[ByteString]): Unit = {
    val fullBlocks = receivedHashes.flatMap { hash =>
      for {
        header <- blockchain.getBlockHeaderByHash(hash)
        _ <- blockchain.getReceiptsByHash(hash)
      } yield header
    }

    if (fullBlocks.nonEmpty) {
      val bestReceivedBlock = fullBlocks.maxBy(_.number)
      if (appStateStorage.getBestBlockNumber() < bestReceivedBlock.number) {
        appStateStorage.putBestBlockNumber(bestReceivedBlock.number)
      }
    }
  }
}

object FastSync {
  private case object RetryStart
  private case object BlockHeadersTimeout
  private case object TargetBlockTimeout

  private case object ProcessSyncing

  case class SyncState(
    targetBlock: BlockHeader,
    mptNodesQueue: Seq[HashType] = Nil,
    nonMptNodesQueue: Seq[HashType] = Nil,
    blockBodiesQueue: Seq[ByteString] = Nil,
    receiptsQueue: Seq[ByteString] = Nil,
    downloadedNodesCount: Int = 0,
    bestBlockHeaderNumber: BigInt = 0)

  case class EnqueueNodes(hashes: Seq[HashType])
  case class EnqueueBlockBodies(hashes: Seq[ByteString])
  case class EnqueueReceipts(hashes: Seq[ByteString])

  case class UpdateDownloadedNodesCount(update: Int)

  sealed trait HashType {
    def v: ByteString
  }
  case class StateMptNodeHash(v: ByteString) extends HashType
  case class ContractStorageMptNodeHash(v: ByteString) extends HashType
  case class EvmCodeHash(v: ByteString) extends HashType
  case class StorageRootHash(v: ByteString) extends HashType
}
