package io.iohk.ethereum.blockchain.sync

import java.time.Instant

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSyncStateHandler._
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.db.storage.{AppStateStorage, FastSyncStateStorage}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{FiniteDuration, _}
import scala.util.Random

// scalastyle:off file.size.limit
class FastSync(
    val fastSyncStateStorage: FastSyncStateStorage,
    val appStateStorage: AppStateStorage,
    val blockchain: Blockchain,
    val validators: Validators,
    val peerEventBus: ActorRef,
    val etcPeerManager: ActorRef,
    val syncConfig: SyncConfig,
    implicit val scheduler: Scheduler)
  extends Actor with ActorLogging
    with PeerListSupport with BlacklistSupport
    with FastSyncReceiptsValidator with SyncBlocksValidator {

  import FastSync._
  import syncConfig._

  val syncController: ActorRef = context.parent

  override def receive: Receive = idle

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  def idle: Receive = handleCommonMessages orElse {
    case Start => start()
  }

  def start(): Unit = {
    log.info("Trying to start block synchronization (fast mode)")
    fastSyncStateStorage.getSyncState() match {
      case Some(syncState) => startWithState(syncState)
      case None => startFromScratch()
    }
  }

  def startWithState(syncState: SyncState): Unit = {
    if (syncState.updatingTargetBlock) {
      log.info(s"FastSync interrupted during targetBlock update, choosing new target block")
      val syncingHandler = new SyncingHandler(syncState, new FastSyncStateHandler(blockchain, validators, syncConfig, appStateStorage))
      val targetBlockSelector = context.actorOf(FastSyncTargetBlockSelector.props(etcPeerManager, peerEventBus, syncConfig, scheduler), "target-block-selector")
      targetBlockSelector ! FastSyncTargetBlockSelector.ChooseTargetBlock
      context become syncingHandler.waitingForTargetBlockUpdate(ImportedLastBlock)
    } else {
      log.info(s"Starting block synchronization (fast mode), target block ${syncState.targetBlock.number}, " +
        s"block to download to ${syncState.safeDownloadTarget}")
      val syncingHandler = new SyncingHandler(syncState, new FastSyncStateHandler(blockchain, validators, syncConfig, appStateStorage))
      context become syncingHandler.receive
      syncingHandler.processSyncing()
    }
  }

  def startFromScratch(): Unit = {
    val targetBlockSelector = context.actorOf(FastSyncTargetBlockSelector.props(etcPeerManager, peerEventBus, syncConfig, scheduler), "target-block-selector")
    targetBlockSelector ! FastSyncTargetBlockSelector.ChooseTargetBlock
    context become waitingForTargetBlock
  }

  def waitingForTargetBlock: Receive = handleCommonMessages orElse {
    case FastSyncTargetBlockSelector.Result(targetBlockHeader) =>
      if (targetBlockHeader.number < 1) {
        log.info("Unable to start block synchronization in fast mode: target block is less than 1")
        appStateStorage.fastSyncDone()
        context become idle
        syncController ! Done
      } else {
        val initialSyncState =
          SyncState(targetBlockHeader, safeDownloadTarget = targetBlockHeader.number + syncConfig.fastSyncBlockValidationX)
        startWithState(initialSyncState)
      }
  }

  // scalastyle:off
  private class SyncingHandler(var syncState: SyncState, fastSyncStateHandler: FastSyncStateHandler) {

    private val BlockHeadersHandlerName = "block-headers-request-handler"

    private var requestedHeaders: Map[Peer, BigInt] = Map.empty

    private var assignedHandlers: Map[ActorRef, Peer] = Map.empty
    private var peerRequestsTime: Map[Peer, Instant] = Map.empty

    private var requestedMptNodes: Map[ActorRef, Seq[HashType]] = Map.empty
    private var requestedNonMptNodes: Map[ActorRef, Seq[HashType]] = Map.empty
    private var requestedBlockBodies: Map[ActorRef, Seq[ByteString]] = Map.empty
    private var requestedReceipts: Map[ActorRef, Seq[ByteString]] = Map.empty

    private val syncStateStorageActor = context.actorOf(Props[FastSyncStateStorageActor], "state-storage")
    syncStateStorageActor ! fastSyncStateStorage

    //Delay before starting to persist snapshot. It should be 0, as the presence of it marks that fast sync was started
    private val persistStateSnapshotDelay: FiniteDuration = 0.seconds
    private val syncStatePersistCancellable = scheduler.schedule(persistStateSnapshotDelay, persistStateSnapshotInterval, self, PersistSyncState)
    private val printStatusCancellable = scheduler.schedule(printStatusInterval, printStatusInterval, self, PrintStatus)
    private val heartBeat = scheduler.schedule(syncRetryInterval, syncRetryInterval * 2, self, ProcessSyncing)

    // scalastyle:off
    def receive: Receive = handleCommonMessages orElse {
      case InitTargetBlockUpdate(state) => updateTargetBlock(state)
      case ProcessSyncing => processSyncing()
      case PrintStatus => printStatus()
      case PersistSyncState => persistSyncState()

      case ResponseReceived(peer, BlockHeaders(blockHeaders), timeTaken) =>
        log.info("*** Received {} block headers in {} ms ***", blockHeaders.size, timeTaken)
        requestedHeaders.get(peer).foreach{ requestedNum =>
          removeRequestHandler(sender())
          requestedHeaders -= peer
          fastSyncStateHandler.handleHeaders(syncState, peer, blockHeaders, requestedNum) match {
            case HandlingResult(ContinueSyncing(None), finalState) =>
              syncState = finalState
              processSyncing()
            case HandlingResult(ContinueSyncing(Some(blackListCommand)), finalState) =>
              syncState = finalState
              blacklist(blackListCommand.peer.id, blacklistDuration, blackListCommand.reason)
              processSyncing()
            case HandlingResult(InitTargetBlockUpdate(reason), finalState) =>
              reason match {
                case LastBlockValidationFailed => blacklist(peer.id, blacklistDuration, "failed validation of final headers")
                case _ =>
              }
              syncState = finalState
              updateTargetBlock(reason)
          }
        }

      case ResponseReceived(peer, BlockBodies(blockBodies), timeTaken) =>
        log.info("Received {} block bodies in {} ms", blockBodies.size, timeTaken)
        val requestedBodies = requestedBlockBodies.getOrElse(sender(), Nil)
        requestedBlockBodies -= sender()
        removeRequestHandler(sender())
        handleStateChange(fastSyncStateHandler.handleBlockBodies(syncState, peer, requestedBodies, blockBodies))
        processSyncing()

      case ResponseReceived(peer, Receipts(receipts), timeTaken) =>
        log.info("Received {} receipts in {} ms", receipts.size, timeTaken)
        val requestedHashes = requestedReceipts.getOrElse(sender(), Nil)
        requestedReceipts -= sender()
        removeRequestHandler(sender())
        handleStateChange(fastSyncStateHandler.handleReceipts(syncState, peer, requestedHashes, receipts))
        processSyncing()

      case ResponseReceived(peer, nodeData: NodeData, timeTaken) =>
        log.info("Received {} state nodes in {} ms", nodeData.values.size, timeTaken)
        val requestedHashes = requestedMptNodes.getOrElse(sender(), Nil) ++ requestedNonMptNodes.getOrElse(sender(), Nil)
        requestedMptNodes -= sender()
        requestedNonMptNodes -= sender()
        removeRequestHandler(sender())
        handleStateChange(fastSyncStateHandler.handleNodeData(syncState, peer, requestedHashes, nodeData))
        processSyncing()

      case PeerRequestHandler.RequestFailed(peer, reason) =>
        handleRequestFailure(peer, sender(), reason)

      case Terminated(ref) if assignedHandlers.contains(ref) =>
        handleRequestFailure(assignedHandlers(ref), ref, "Unexpected error")
    }

    private def handleStateChange(change: HandlingResult[ContinueSyncing]): Unit = change match {
      case HandlingResult(ContinueSyncing(None), finalState) =>
        syncState = finalState
      case HandlingResult(ContinueSyncing(Some(blackListCommand)), finalState) =>
        syncState = finalState
        blacklist(blackListCommand.peer.id, blacklistDuration, blackListCommand.reason)
    }

    def waitingForTargetBlockUpdate(processState: FinalBlockProcessingResult): Receive = handleCommonMessages orElse {
      case FastSyncTargetBlockSelector.Result(targetBlockHeader) =>
        log.info(s"new target block with number ${targetBlockHeader.number} received")
        fastSyncStateHandler.handleNewTargetBlock(syncState, processState, targetBlockHeader) match {
          case HandlingResult(BackToSyncing, finalState) =>
            syncState = finalState
            context become this.receive
            processSyncing()
          case HandlingResult(AskForNewTarget(state), finalState) =>
            syncState = finalState
            scheduler.scheduleOnce(syncRetryInterval, self, InitTargetBlockUpdate(state))
        }

      case PersistSyncState => persistSyncState()

      case InitTargetBlockUpdate(state) => updateTargetBlock(state)
    }

    private def updateTargetBlock(state: FinalBlockProcessingResult): Unit = {
      fastSyncStateHandler.updateTargetBlock(syncState, state) match {
        case HandlingResult(AbortFastSync, _) =>
          log.warning(s"Sync failure! Number of targetBlock Failures reached maximum.")
          sys.exit(1)
        case HandlingResult(ContTargetBlockUpdate(newResult), newSyncState) =>
          syncState = newSyncState
          if (assignedHandlers.nonEmpty) {
            log.info(s"Still waiting for some responses, rescheduling target block update")
            scheduler.scheduleOnce(syncRetryInterval, self, InitTargetBlockUpdate(state))
          } else {
            log.info("Asking for new target block")
            val targetBlockSelector =
              context.actorOf(FastSyncTargetBlockSelector.props(etcPeerManager, peerEventBus, syncConfig, scheduler))
            targetBlockSelector ! FastSyncTargetBlockSelector.ChooseTargetBlock
            context become waitingForTargetBlockUpdate(newResult)
          }
      }
    }

    private def removeRequestHandler(handler: ActorRef): Unit = {
      context unwatch handler
      assignedHandlers -= handler
    }

    private def handleRequestFailure(peer: Peer, handler: ActorRef, reason: String) = {
      removeRequestHandler(handler)

      syncState = syncState
        .addPendingNodes(requestedMptNodes.getOrElse(handler, Nil))
        .addPendingNodes(requestedNonMptNodes.getOrElse(handler, Nil))
        .enqueueBlockBodies(requestedBlockBodies.getOrElse(handler, Nil))
        .enqueueReceipts(requestedReceipts.getOrElse(handler, Nil))

      requestedMptNodes = requestedMptNodes - handler
      requestedNonMptNodes = requestedNonMptNodes - handler
      requestedBlockBodies = requestedBlockBodies - handler
      requestedReceipts = requestedReceipts - handler

      requestedHeaders -= peer
      if (handshakedPeers.contains(peer)) {
        blacklist(peer.id, blacklistDuration, reason)
      }
    }

    private def persistSyncState(): Unit = {
      syncStateStorageActor ! syncState.copy(
        pendingMptNodes = requestedMptNodes.values.flatten.toSeq.distinct ++ syncState.pendingMptNodes,
        pendingNonMptNodes = requestedNonMptNodes.values.flatten.toSeq.distinct ++ syncState.pendingNonMptNodes,
        blockBodiesQueue = requestedBlockBodies.values.flatten.toSeq.distinct ++ syncState.blockBodiesQueue,
        receiptsQueue = requestedReceipts.values.flatten.toSeq.distinct ++ syncState.receiptsQueue)
    }

    private def printStatus() = {
      val formatPeer: (Peer) => String = peer => s"${peer.remoteAddress.getAddress.getHostAddress}:${peer.remoteAddress.getPort}"
      log.info(
        s"""|Block: ${appStateStorage.getBestBlockNumber()}/${syncState.targetBlock.number}.
            |Peers waiting_for_response/connected: ${assignedHandlers.size}/${handshakedPeers.size} (${blacklistedPeers.size} blacklisted).
            |State: ${syncState.downloadedNodesCount}/${syncState.totalNodesCount} nodes.
            |""".stripMargin.replace("\n", " "))
      log.debug(
        s"""|Connection status: connected(${assignedHandlers.values.map(formatPeer).toSeq.sorted.mkString(", ")})/
            |handshaked(${handshakedPeers.keys.map(formatPeer).toSeq.sorted.mkString(", ")})
            | blacklisted(${blacklistedPeers.map { case (id, _) => id.value }.mkString(", ")})
            |""".stripMargin.replace("\n", " ")
      )
    }

    def processSyncing(): Unit = {
      if (fullySynced) {
        finish()
      } else {
        if (syncState.readyToDownload) processDownloads()
        else log.info("No more items to request, waiting for {} responses", assignedHandlers.size)
      }
    }

    def finish(): Unit = {
      log.info("Block synchronization in fast mode finished, switching to regular mode")
      // We have downloaded to target + fastSyncBlockValidationX, se we must discard those last blocks
      fastSyncStateHandler.discardLastBlocks(syncState.safeDownloadTarget, syncConfig.fastSyncBlockValidationX - 1)
      cleanup()
      appStateStorage.fastSyncDone()
      context become idle
      peerRequestsTime = Map.empty
      syncController ! Done
    }

    def cleanup(): Unit = {
      heartBeat.cancel()
      syncStatePersistCancellable.cancel()
      printStatusCancellable.cancel()
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
        peers
          .take(maxConcurrentRequests - assignedHandlers.size)
          .toSeq.sortBy(_.ref.toString())
          .foreach(assignWork)
      }
    }

    def assignWork(peer: Peer): Unit = {
      if (syncState.blockChainWorkLeft) {
        assignBlockchainWork(peer)
      } else {
        requestNodes(peer)
      }
    }

    def assignBlockchainWork(peer: Peer): Unit = {
      if (syncState.receiptsQueue.nonEmpty) {
        requestReceipts(peer)
      } else if (syncState.blockBodiesQueue.nonEmpty) {
        requestBlockBodies(peer)
      } else if (requestedHeaders.isEmpty &&
        context.child(BlockHeadersHandlerName).isEmpty &&
        syncState.bestBlockHeaderNumber < syncState.safeDownloadTarget) {
        requestBlockHeaders(peer)
      }
    }

    def requestReceipts(peer: Peer): Unit = {
      val (newState, receiptsToGet) = fastSyncStateHandler.getReceiptsToRequest(syncState)

      val handler = context.actorOf(
        PeerRequestHandler.props[GetReceipts, Receipts](
          peer, peerResponseTimeout, etcPeerManager, peerEventBus,
          requestMsg = GetReceipts(receiptsToGet),
          responseMsgCode = Receipts.code))

      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
      syncState = newState
      requestedReceipts += handler -> receiptsToGet
    }

    def requestBlockBodies(peer: Peer): Unit = {
      val (newState, blockBodiesToGet) = fastSyncStateHandler.getBodiesToRequest(syncState)

      val handler = context.actorOf(
        PeerRequestHandler.props[GetBlockBodies, BlockBodies](
          peer, peerResponseTimeout, etcPeerManager, peerEventBus,
          requestMsg = GetBlockBodies(blockBodiesToGet),
          responseMsgCode = BlockBodies.code))

      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
      syncState = newState
      requestedBlockBodies += handler -> blockBodiesToGet
    }

    def requestBlockHeaders(peer: Peer): Unit = {
      val limit: BigInt = fastSyncStateHandler.getHeadersToRequest(syncState)

      val handler = context.actorOf(
        PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
          peer, peerResponseTimeout, etcPeerManager, peerEventBus,
          requestMsg = GetBlockHeaders(Left(syncState.bestBlockHeaderNumber + 1), limit, skip = 0, reverse = false),
          responseMsgCode = BlockHeaders.code), BlockHeadersHandlerName)

      context watch handler
      assignedHandlers += (handler -> peer)
      requestedHeaders += (peer -> limit)
      peerRequestsTime += (peer -> Instant.now())
    }

    def requestNodes(peer: Peer): Unit = {
      val (newState, nodesToGet, mptNodesToGet, nonMptNodesToGet) = fastSyncStateHandler.getNodesToRequest(syncState)

      val handler = context.actorOf(
        PeerRequestHandler.props[GetNodeData, NodeData](
          peer, peerResponseTimeout, etcPeerManager, peerEventBus,
          requestMsg = GetNodeData(nodesToGet.map(_.v)),
          responseMsgCode = NodeData.code))

      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
      syncState = newState
      requestedMptNodes += handler -> mptNodesToGet
      requestedNonMptNodes += handler -> nonMptNodesToGet
    }

    def unassignedPeers: Set[Peer] = peersToDownloadFrom.keySet diff assignedHandlers.values.toSet

    def fullySynced: Boolean = {
      !syncState.anythingToDownload && assignedHandlers.isEmpty
    }
  }

}

object FastSync {
  // scalastyle:off parameter.number
  def props(fastSyncStateStorage: FastSyncStateStorage, appStateStorage: AppStateStorage, blockchain: Blockchain,
  validators: Validators, peerEventBus: ActorRef, etcPeerManager: ActorRef, syncConfig: SyncConfig, scheduler: Scheduler): Props =
    Props(new FastSync(fastSyncStateStorage, appStateStorage, blockchain, validators, peerEventBus, etcPeerManager, syncConfig, scheduler))

  private case object ProcessSyncing
  private[sync] case object PersistSyncState
  private case object PrintStatus

  case object Start
  case object Done

  case class SyncState(
    targetBlock: BlockHeader,
    safeDownloadTarget: BigInt = 0,
    pendingMptNodes: Seq[HashType] = Nil,
    pendingNonMptNodes: Seq[HashType] = Nil,
    blockBodiesQueue: Seq[ByteString] = Nil,
    receiptsQueue: Seq[ByteString] = Nil,
    downloadedNodesCount: Int = 0,
    bestBlockHeaderNumber: BigInt = 0,
    nextBlockToFullyValidate: BigInt = 1,
    targetBlockUpdateFailures: Int = 0,
    updatingTargetBlock: Boolean = false) {

    def enqueueBlockBodies(blockBodies: Seq[ByteString]): SyncState =
      copy(blockBodiesQueue = blockBodiesQueue ++ blockBodies)

    def enqueueReceipts(receipts: Seq[ByteString]): SyncState =
      copy(receiptsQueue = receiptsQueue ++ receipts)

    def addPendingNodes(hashes: Seq[HashType]): SyncState = {
      val (mpt, nonMpt) = hashes.partition {
        case _: StateMptNodeHash | _: ContractStorageMptNodeHash => true
        case _: EvmCodeHash | _: StorageRootHash => false
      }
      // Nodes are prepended in order to traverse mpt in-depth. For mpt nodes is not needed but to keep it consistent,
      // it was applied too
      copy(
        pendingMptNodes = mpt ++ pendingMptNodes,
        pendingNonMptNodes = nonMpt ++ pendingNonMptNodes)
    }

    def anythingQueued: Boolean =
      pendingNonMptNodes.nonEmpty ||
      pendingMptNodes.nonEmpty ||
        blockChainWorkQueued

    def blockChainWorkQueued: Boolean =  blockBodiesQueue.nonEmpty || receiptsQueue.nonEmpty

    def blockChainWorkLeft: Boolean =
      bestBlockHeaderNumber < safeDownloadTarget || blockChainWorkQueued

    def updateNextBlockToValidate(header: BlockHeader, K: Int, X:Int): SyncState = copy(
      nextBlockToFullyValidate =
        if (bestBlockHeaderNumber >= targetBlock.number - X)
          header.number + 1
        else
          (header.number + K / 2 + Random.nextInt(K)).min(targetBlock.number - X)
    )

    def updateDiscardedBlocks(header: BlockHeader, N:Int): SyncState = copy(
      blockBodiesQueue = Seq.empty,
      receiptsQueue = Seq.empty,
      bestBlockHeaderNumber = (header.number - N - 1) max 0,
      nextBlockToFullyValidate = (header.number - N) max 1
    )

    val totalNodesCount: Int = downloadedNodesCount + pendingMptNodes.size + pendingNonMptNodes.size

    def updateTargetBlock(newTarget: BlockHeader, numberOfSafeBlocks:BigInt, updateFailures: Boolean): SyncState = copy(
      targetBlock = newTarget,
      safeDownloadTarget = newTarget.number + numberOfSafeBlocks,
      targetBlockUpdateFailures = if (updateFailures) targetBlockUpdateFailures + 1 else targetBlockUpdateFailures
    )

    def anythingToDownload: Boolean =
      anythingQueued || bestBlockHeaderNumber < safeDownloadTarget

    def readyToDownload: Boolean =
      anythingToDownload && !updatingTargetBlock
  }

}
