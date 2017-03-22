package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.network.PeerActor.Status.Handshaked
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.utils.Config.FastSync._

trait FastSync {
  selfSyncController: SyncController =>

  import FastSync._
  import SyncController._

  def startFastSync(): Unit = {
    log.info("Starting fast sync")
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
      peersUsedToChooseTarget.foreach { case (peer, Handshaked(status, _, _)) =>
        peer ! PeerActor.Subscribe(Set(BlockHeaders.code))
        peer ! PeerActor.SendMessage(GetBlockHeaders(Right(status.bestHash), 1, 0, reverse = false))
      }
      log.info("Asking {} peers for block headers", peersUsedToChooseTarget.size)
      val timeout = scheduler.scheduleOnce(peerResponseTimeout, self, BlockHeadersTimeout)
      context become waitingForBlockHeaders(peersUsedToChooseTarget.keySet, Map.empty, timeout)
    } else {
      log.warning("Cannot start fast sync, not enough peers to download from. Scheduling retry in {}", startRetryInterval)
      scheduleStartRetry(startRetryInterval)
      context become startingFastSync
    }
  }

  private def waitingForBlockHeaders(waitingFor: Set[ActorRef],
                             received: Map[ActorRef, BlockHeader],
                             timeout: Cancellable): Receive = handlePeerUpdates orElse {
    case PeerActor.MessageReceived(BlockHeaders(blockHeaders)) if blockHeaders.size == 1 =>
      sender() ! PeerActor.Unsubscribe

      val newWaitingFor = waitingFor - sender()
      val newReceived = received + (sender() -> blockHeaders.head)

      if (newWaitingFor.isEmpty) {
        timeout.cancel()
        tryStartFastSync(newReceived)
      } else context become waitingForBlockHeaders(newWaitingFor, newReceived, timeout)

    case PeerActor.MessageReceived(BlockHeaders(_)) =>
      sender() ! PeerActor.Unsubscribe
      blacklist(sender(), blacklistDuration)
      context become waitingForBlockHeaders(waitingFor - sender(), received, timeout)

    case BlockHeadersTimeout =>
      waitingFor.foreach { peer =>
        peer ! PeerActor.Unsubscribe
        blacklist(peer, blacklistDuration)
      }
      tryStartFastSync(received)
  }

  private def tryStartFastSync(receivedHeaders: Map[ActorRef, BlockHeader]): Unit = {
    log.info("Trying to start fast sync. Received {} block headers", receivedHeaders.size)
    if (receivedHeaders.size >= minPeersToChooseTargetBlock) {
      val (mostUpToDatePeer, mostUpToDateBlockHeader) = receivedHeaders.maxBy(_._2.number)
      val targetBlock = mostUpToDateBlockHeader.number - targetBlockOffset

      if (targetBlock < 1) {
        log.info("Target block is less than 1, starting regular sync")
        appStateStorage.fastSyncDone()
        context become idle
        self ! FastSyncDone
      } else {
        log.info("Starting fast sync. Asking peer {} for target block header ({})", mostUpToDatePeer.path.name, targetBlock)
        mostUpToDatePeer ! PeerActor.Subscribe(Set(BlockHeaders.code))
        mostUpToDatePeer ! PeerActor.SendMessage(GetBlockHeaders(Left(targetBlock), 1, 0, reverse = false))
        val timeout = scheduler.scheduleOnce(peerResponseTimeout, self, TargetBlockTimeout)
        context become waitingForTargetBlock(mostUpToDatePeer, targetBlock, timeout)
      }

    } else {
      log.info("Did not receive enough status block headers to start fast sync. Retry in {}", startRetryInterval)
      scheduleStartRetry(startRetryInterval)
      context become startingFastSync
    }
  }

  def waitingForTargetBlock(peer: ActorRef,
                            targetBlockNumber: BigInt,
                            timeout: Cancellable): Receive = handlePeerUpdates orElse {
    case PeerActor.MessageReceived(blockHeaders: BlockHeaders) =>
      timeout.cancel()
      peer ! PeerActor.Unsubscribe

      val targetBlockHeaderOpt = blockHeaders.headers.find(header => header.number == targetBlockNumber)
      targetBlockHeaderOpt match {
        case Some(targetBlockHeader) =>
          log.info("Received target block from peer, starting fast sync")
          val initialSyncState = SyncState(targetBlockHeader,
            mptNodesQueue = Seq(StateMptNodeHash(targetBlockHeader.stateRoot)))
          startFastSync(initialSyncState)

        case None =>
          log.info("Peer ({}) did not respond with target block header, blacklisting and scheduling retry in {}",
            sender().path.name,
            startRetryInterval)

          blacklist(sender(), blacklistDuration)
          scheduleStartRetry(startRetryInterval)
          context become startingFastSync
      }

    case TargetBlockTimeout =>
      log.info("Peer ({}) did not respond with target block header (timeout), blacklisting and scheduling retry in {}",
        sender().path.name,
        startRetryInterval)

      blacklist(sender(), blacklistDuration)
      peer ! PeerActor.Unsubscribe
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

    private var assignedHandlers: Map[ActorRef, ActorRef] = Map.empty

    private val syncStateStorageActor= context.actorOf(Props[FastSyncStateActor], "state-storage")

    syncStateStorageActor ! fastSyncStateStorage

    private val syncStatePersistCancellable =
      scheduler.schedule(persistStateSnapshotInterval, persistStateSnapshotInterval) {
        syncStateStorageActor ! SyncState(
          initialSyncState.targetBlock,
          mptNodesQueue,
          nonMptNodesQueue,
          blockBodiesQueue,
          receiptsQueue,
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
        processSyncing()

      case Terminated(ref) if assignedHandlers.contains(ref) =>
        context unwatch ref
        assignedHandlers -= ref

      case PrintStatus =>
        val totalNodesCount = downloadedNodesCount + mptNodesQueue.size + nonMptNodesQueue.size
        log.info(
          s"""|Block: ${appStateStorage.getBestBlockNumber()}/${initialSyncState.targetBlock.number}.
              |Peers: ${assignedHandlers.size}/${handshakedPeers.size} (${blacklistedPeers.size} blacklisted).
              |State: $downloadedNodesCount/$totalNodesCount known nodes.""".stripMargin.replace("\n", " "))
    }

    private def insertBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): Unit = {
      //todo this is moved from FastSyncBlockBodiesRequestHandler.scala we should add block validation here
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
        else log.info("No more items to request, waiting for {} responses", assignedHandlers.size)
      }
    }

    def finishFastSync(): Unit = {
      log.info("Fast sync finished")
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
          log.warning("There are no available peers, waiting for responses")
        } else {
          log.warning("There are no peers to download from, scheduling a retry in {}", syncRetryInterval)
          scheduler.scheduleOnce(syncRetryInterval, self, ProcessSyncing)
        }
      } else {
        unassignedPeers
          .take(maxConcurrentRequests - assignedHandlers.size)
          .foreach(assignWork)
      }
    }

    def assignWork(peer: ActorRef): Unit = {
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

    def requestReceipts(peer: ActorRef): Unit = {
      val (receiptsToGet, remainingReceipts) = receiptsQueue.splitAt(receiptsPerRequest)
      val handler = context.actorOf(FastSyncReceiptsRequestHandler.props(
        peer, receiptsToGet, appStateStorage, blockchain))
      context watch handler
      assignedHandlers += (handler -> peer)
      receiptsQueue = remainingReceipts
    }

    def requestBlockBodies(peer: ActorRef): Unit = {
      val (blockBodiesToGet, remainingBlockBodies) = blockBodiesQueue.splitAt(blockBodiesPerRequest)
      val handler = context.actorOf(SyncBlockBodiesRequestHandler.props(
        peer, blockBodiesToGet, appStateStorage))
      context watch handler
      assignedHandlers += (handler -> peer)
      blockBodiesQueue = remainingBlockBodies
    }

    def requestBlockHeaders(peer: ActorRef): Unit = {
      val limit: BigInt = if (blockHeadersPerRequest < (initialSyncState.targetBlock.number - bestBlockHeaderNumber))
        blockHeadersPerRequest
      else
        initialSyncState.targetBlock.number - bestBlockHeaderNumber

      val request = GetBlockHeaders(Left(bestBlockHeaderNumber + 1), limit, skip = 0, reverse = false)
      val handler = context.actorOf(SyncBlockHeadersRequestHandler.props(peer, request, resolveBranches = false), blockHeadersHandlerName)
      context watch handler
      assignedHandlers += (handler -> peer)
    }

    def requestNodes(peer: ActorRef): Unit = {
      val (nonMptNodesToGet, remainingNonMptNodes) = nonMptNodesQueue.splitAt(nodesPerRequest)
      val (mptNodesToGet, remainingMptNodes) = mptNodesQueue.splitAt(nodesPerRequest - nonMptNodesToGet.size)
      val nodesToGet = nonMptNodesToGet ++ mptNodesToGet
      val handler = context.actorOf(FastSyncNodesRequestHandler.props(peer, nodesToGet, blockchain, mptNodeStorage))
      context watch handler
      assignedHandlers += (handler -> peer)
      nonMptNodesQueue = remainingNonMptNodes
      mptNodesQueue = remainingMptNodes
    }

    def unassignedPeers: Set[ActorRef] = peersToDownloadFrom.keySet diff assignedHandlers.values.toSet

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
