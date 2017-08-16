package io.iohk.ethereum.blockchain.sync


import java.time.Instant
import java.util.Date

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}
import io.iohk.ethereum.utils.Config.Sync._
import io.iohk.ethereum.validators.BlockValidator

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

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
          log.info(s"Starting block synchronization (fast mode), target block ${targetBlockHeader.number}")
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
    private var peerRequestsTime: Map[Peer, Instant] = Map.empty

    private val syncStateStorageActor = context.actorOf(Props[FastSyncStateActor], "state-storage")

    private var requestedMptNodes: Map[ActorRef, Seq[HashType]] = Map.empty
    private var requestedNonMptNodes: Map[ActorRef, Seq[HashType]] = Map.empty
    private var requestedBlockBodies: Map[ActorRef, Seq[ByteString]] = Map.empty
    private var requestedReceipts: Map[ActorRef, Seq[ByteString]] = Map.empty

    syncStateStorageActor ! fastSyncStateStorage

    private var blockChainOnlyPeers = Seq.empty[Peer]

    //Delay before starting to persist snapshot. It should be 0, as the presence of it marks that fast sync was started
    private val persistStateSnapshotDelay: FiniteDuration = 0.seconds

    private val syncStatePersistCancellable = scheduler.schedule(persistStateSnapshotDelay, persistStateSnapshotInterval, self, PersistSyncState)
    private val heartBeat = scheduler.schedule(syncRetryInterval, syncRetryInterval * 2, self, ProcessSyncing)

    // scalastyle:off cyclomatic.complexity
    def receive: Receive = handlePeerUpdates orElse handleFailingMptPeers orElse {
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

      case BlockBodiesReceived(peer, requestedHashes, blockBodies) =>
        handleBlockBodies(peer, requestedHashes, blockBodies)

      case BlockHeadersReceived(_, headers) =>
        insertHeaders(headers)

      case ProcessSyncing =>
        processSyncing()

      case SyncRequestHandler.Done =>
        context unwatch sender()
        assignedHandlers -= sender()
        cleanupRequestedMaps(sender())
        self ! ProcessSyncing

      case Terminated(ref) if assignedHandlers.contains(ref) =>
        handleActorTerminate(ref)

      case PrintStatus =>
        printStatus()

      case PersistSyncState =>
        persistSyncState()
    }

    private def handleBlockBodies(peer: Peer, requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]) = {
      validateBlocks(requestedHashes, blockBodies) match {
        case Valid =>
          insertBlocks(requestedHashes, blockBodies)
        case Invalid =>
          blacklist(peer.id, blacklistDuration, s"responded with block bodies not matching block headers, blacklisting for $blacklistDuration")
          self ! FastSync.EnqueueBlockBodies(requestedHashes)
        case DbError =>
          blockBodiesQueue = Seq.empty
          receiptsQueue = Seq.empty
          //todo adjust the formula to minimize redownloaded block headers
          bestBlockHeaderNumber = bestBlockHeaderNumber - 2 * blockHeadersPerRequest
          log.debug("missing block header for known hash")
          self ! ProcessSyncing
      }
    }

    private def handleActorTerminate(ref: ActorRef) = {
      context unwatch ref
      val peer = assignedHandlers(ref)
      peerRequestsTime -= peer
      assignedHandlers -= ref
      mptNodesQueue ++= requestedMptNodes.getOrElse(ref, Nil)
      nonMptNodesQueue ++= requestedNonMptNodes.getOrElse(ref, Nil)
      blockBodiesQueue ++= requestedBlockBodies.getOrElse(ref, Nil)
      receiptsQueue ++= requestedReceipts.getOrElse(ref, Nil)
      cleanupRequestedMaps(ref)
    }

    private def persistSyncState(): Unit = {
      syncStateStorageActor ! SyncState(
        initialSyncState.targetBlock,
        requestedMptNodes.values.flatten.toSeq.distinct ++ mptNodesQueue,
        requestedNonMptNodes.values.flatten.toSeq.distinct ++ nonMptNodesQueue,
        requestedBlockBodies.values.flatten.toSeq.distinct ++ blockBodiesQueue,
        requestedReceipts.values.flatten.toSeq.distinct ++ receiptsQueue,
        downloadedNodesCount,
        bestBlockHeaderNumber)
    }

    private def handleFailingMptPeers: Receive = {
      case MarkPeerBlockchainOnly(peer) => if (!blockChainOnlyPeers.contains(peer)) {
        blockChainOnlyPeers = (peer +: blockChainOnlyPeers).take(blockChainOnlyPeersPoolSize)
      }
    }

    private def printStatus() = {
      val totalNodesCount = downloadedNodesCount + mptNodesQueue.size + nonMptNodesQueue.size
      val formatPeer: (Peer) => String = peer => s"${peer.remoteAddress.getAddress.getHostAddress}:${peer.remoteAddress.getPort}"
      log.info(
        s"""|Block: ${appStateStorage.getBestBlockNumber()}/${initialSyncState.targetBlock.number}.
            |Peers waiting_for_response/connected: ${assignedHandlers.size}/${handshakedPeers.size} (${blacklistedPeers.size} blacklisted).
            |State: $downloadedNodesCount/$totalNodesCount nodes.
            |""".stripMargin.replace("\n", " "))
      log.debug(
        s"""|Connection status: connected(${assignedHandlers.values.map(formatPeer).toSeq.sorted.mkString(", ")})/
            |handshaked(${handshakedPeers.keys.map(formatPeer).toSeq.sorted.mkString(", ")})
            | blacklisted(${blacklistedPeers.map { case (id, _) => id.value }.mkString(", ")})
            |""".stripMargin.replace("\n", " ")
      )
    }

    private def cleanupRequestedMaps(handler: ActorRef): Unit = {
      requestedMptNodes = requestedMptNodes - handler
      requestedNonMptNodes = requestedNonMptNodes - handler
      requestedBlockBodies = requestedBlockBodies - handler
      requestedReceipts = requestedReceipts - handler
    }

    private def validateBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): BlockBodyValidationResult = {
      var result: BlockBodyValidationResult = Valid
      (requestedHashes zip blockBodies)
        .map { case (hash, body) => (blockchain.getBlockHeaderByHash(hash), body) }
        .forall {
          case (Some(header), body) =>
            val validationResult: Either[BlockValidator.BlockError, Block] = validators.blockValidator.validateHeaderAndBody(header, body)
            result = validationResult.fold(_ => Invalid, _ => Valid)
            validationResult.isRight
          case _ =>
            result = DbError
            false
        }
      result
    }

    private def insertBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): Unit = {
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

      val blockHashes = blockHeadersObtained.map(_.hash)
      self ! FastSync.EnqueueBlockBodies(blockHashes)
      self ! FastSync.EnqueueReceipts(blockHashes)
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
      blockChainOnlyPeers = Seq.empty
      peerRequestsTime = Map.empty
      self ! FastSyncDone
    }

    def cleanup(): Unit = {
      heartBeat.cancel()
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
        val now = Instant.now()
        val peers = unassignedPeers
          .filter(p => peerRequestsTime.get(p).forall(d => d.plusMillis(fastSyncThrottle.toMillis).isBefore(now)))
        val blockChainPeers = blockChainOnlyPeers.toSet
        (peers -- blockChainPeers)
          .take(maxConcurrentRequests - assignedHandlers.size)
          .toSeq.sortBy(_.ref.toString())
          .foreach(assignWork)
        peers
          .intersect(blockChainPeers)
          .take(maxConcurrentRequests - assignedHandlers.size)
          .toSeq.sortBy(_.ref.toString())
          .foreach(assignBlockChainWork)
      }
    }

    def assignWork(peer: Peer): Unit = {
      if (nonMptNodesQueue.nonEmpty || mptNodesQueue.nonEmpty) {
        requestNodes(peer)
      } else {
        assignBlockChainWork(peer)
      }
    }

    def assignBlockChainWork(peer: Peer): Unit = {
      if (receiptsQueue.nonEmpty) {
        requestReceipts(peer)
      } else if (blockBodiesQueue.nonEmpty) {
        requestBlockBodies(peer)
      } else if (context.child(blockHeadersHandlerName).isEmpty &&
        initialSyncState.targetBlock.number > bestBlockHeaderNumber) {
        requestBlockHeaders(peer)
      }
    }

    def requestReceipts(peer: Peer): Unit = {
      val (receiptsToGet, remainingReceipts) = receiptsQueue.splitAt(receiptsPerRequest)
      val handler = context.actorOf(FastSyncReceiptsRequestHandler.props(
        peer, etcPeerManager, peerEventBus, receiptsToGet, appStateStorage, blockchain))
      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
      receiptsQueue = remainingReceipts
      requestedReceipts += handler -> receiptsToGet
    }

    def requestBlockBodies(peer: Peer): Unit = {
      val (blockBodiesToGet, remainingBlockBodies) = blockBodiesQueue.splitAt(blockBodiesPerRequest)
      val handler = context.actorOf(SyncBlockBodiesRequestHandler.props(peer, etcPeerManager, peerEventBus, blockBodiesToGet))
      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
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
      peerRequestsTime += (peer -> Instant.now())
    }

    def requestNodes(peer: Peer): Unit = {
      val (nonMptNodesToGet, remainingNonMptNodes) = nonMptNodesQueue.splitAt(nodesPerRequest)
      val (mptNodesToGet, remainingMptNodes) = mptNodesQueue.splitAt(nodesPerRequest - nonMptNodesToGet.size)
      val nodesToGet = nonMptNodesToGet ++ mptNodesToGet
      val handler = context.actorOf(FastSyncNodesRequestHandler.props(peer, etcPeerManager, peerEventBus, nodesToGet, blockchain,
        blockchainStorages.nodesKeyValueStorageFor(Some(initialSyncState.targetBlock.number))))
      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
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
  private case object PersistSyncState
  case class MarkPeerBlockchainOnly(peer: Peer)

  private sealed trait BlockBodyValidationResult
  private case object Valid extends BlockBodyValidationResult
  private case object Invalid extends BlockBodyValidationResult
  private case object DbError extends BlockBodyValidationResult

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
