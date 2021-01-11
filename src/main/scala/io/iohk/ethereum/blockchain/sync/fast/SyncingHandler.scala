package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.{Actor, ActorLogging, _}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status.Progress
import io.iohk.ethereum.blockchain.sync.fast.FastSync.PivotBlockUpdateReason
import io.iohk.ethereum.blockchain.sync.fast.ReceiptsValidator.ReceiptsValidationResult
import io.iohk.ethereum.blockchain.sync.fast.SyncBlocksValidator.BlockBodyValidationResult
import io.iohk.ethereum.blockchain.sync.fast.SyncStateSchedulerActor._
import io.iohk.ethereum.blockchain.sync.fast.SyncingHandler.AskForPivotBlockUpdate
import io.iohk.ethereum.blockchain.sync.{BlacklistSupport, PeerListSupport, _}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import java.time.Instant
import org.bouncycastle.util.encoders.Hex
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

// scalastyle:off number.of.methods
class SyncingHandler(
    initialSyncState: PersistentSyncState,
    storage: SyncingHandlerStorage,
    validator: FastSyncValidator,
    val peerEventBus: ActorRef,
    val etcPeerManager: ActorRef,
    pivotBlockHandler: PivotBlockHandler,
    val syncConfig: SyncConfig
)(implicit
    val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with PeerListSupport
    with BlacklistSupport {

  import FastSync._
  import syncConfig._

  private val BlockHeadersHandlerName = "block-headers-request-handler"
  //not part of syncstate as we do not want to persist is.

  private var requestedHeaders: Map[Peer, BigInt] = Map.empty

  private var syncState: SyncingHandlerState = SyncingHandlerState.fromPersistentState(initialSyncState)

  private var assignedHandlers: Map[ActorRef, Peer] = Map.empty
  private var peerRequestsTime: Map[Peer, Instant] = Map.empty

  private var requestedBlockBodies: Map[ActorRef, Seq[ByteString]] = Map.empty
  private var requestedReceipts: Map[ActorRef, Seq[ByteString]] = Map.empty

  //Delay before starting to persist snapshot. It should be 0, as the presence of it marks that fast sync was started
  private val persistStateSnapshotDelay: FiniteDuration = 0.seconds
  private val syncStatePersistCancellable =
    scheduler.scheduleWithFixedDelay(persistStateSnapshotDelay, persistStateSnapshotInterval, self, PersistSyncState)
  private val printStatusCancellable =
    scheduler.scheduleWithFixedDelay(printStatusInterval, printStatusInterval, self, PrintStatus)
  private val heartBeat =
    scheduler.scheduleWithFixedDelay(syncRetryInterval, syncRetryInterval * 2, self, ProcessSyncing)

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages orElse handleStatus

  def handleStatus: Receive = {
    case SyncProtocol.GetStatus =>
      sender() ! currentSyncingStatus
    case SyncStateSchedulerActor.StateSyncStats(saved, missing) =>
      syncState = syncState.copy(downloadedNodesCount = saved, totalNodesCount = (saved + missing))
  }

  override def receive: Receive = handleCommonMessages orElse {
    case ProcessSyncing =>
      processSyncing()
      context become syncing
    case AskForPivotBlockUpdate(reason) =>
      pivotBlockHandler.askForPivotBlockUpdate()
      context become waitingForPivotBlockUpdate(reason)
  }

  def syncing: Receive = handleCommonMessages orElse {
    case UpdatePivotBlock(reason) =>
      updatePivotBlock(reason)
    case WaitingForNewTargetBlock =>
      log.info("State sync stopped until receiving new pivot block")
      updatePivotBlock(ImportedLastBlock)
    case ProcessSyncing => processSyncing()
    case PrintStatus => printStatus()
    case PersistSyncState => persistSyncState()
    case StateSyncFinished =>
      syncState = syncState.copy(stateSyncFinished = true)
      processSyncing()

    case ResponseReceived(peer, BlockHeaders(blockHeaders), timeTaken) =>
      log.info("*** Received {} block headers in {} ms ***", blockHeaders.size, timeTaken)
      requestedHeaders.get(peer).foreach { requestedNum =>
        removeRequestHandler(sender())
        requestedHeaders -= peer
        handleBlockHeaders(peer, blockHeaders, requestedNum)
      }

    case ResponseReceived(peer, BlockBodies(blockBodies), timeTaken) =>
      log.info("Received {} block bodies in {} ms", blockBodies.size, timeTaken)
      val requestedBodies = requestedBlockBodies.getOrElse(sender(), Nil)
      requestedBlockBodies -= sender()
      removeRequestHandler(sender())
      handleBlockBodies(peer, requestedBodies, blockBodies)

    case ResponseReceived(peer, Receipts(receipts), timeTaken) =>
      log.info("Received {} receipts in {} ms", receipts.size, timeTaken)
      val requestedHashes = requestedReceipts.getOrElse(sender(), Nil)
      requestedReceipts -= sender()
      removeRequestHandler(sender())
      handleReceipts(peer, requestedHashes, receipts)

    case PeerRequestHandler.RequestFailed(peer, reason) =>
      handleRequestFailure(peer, sender(), reason)

    case Terminated(ref) if assignedHandlers.contains(ref) =>
      handleRequestFailure(assignedHandlers(ref), ref, "Unexpected error")
  }

  def waitingForPivotBlockUpdate(updateReason: PivotBlockUpdateReason): Receive = handleCommonMessages orElse {
    case PivotBlockSelector.Result(pivotBlockHeader)
        if pivotBlockHandler.newPivotIsGoodEnough(pivotBlockHeader, updateReason) =>
      log.info(s"New pivot block with number ${pivotBlockHeader.number} received")
      pivotBlockHandler.updatePivotSyncState(updateReason, pivotBlockHeader)
      context become syncing
      processSyncing()

    case PivotBlockSelector.Result(_) =>
      log.info("Received pivot block is older than old one, re-scheduling asking for new one")
      pivotBlockHandler.reScheduleAskForNewPivot(updateReason)

    case PersistSyncState =>
      persistSyncState()

    case UpdatePivotBlock(state) =>
      updatePivotBlock(state)
  }

  def currentSyncingStatus: SyncProtocol.Status =
    SyncProtocol.Status.Syncing(
      initialSyncState.lastFullBlockNumber,
      Progress(syncState.lastFullBlockNumber, pivotBlockHandler.pivotBlock.number),
      Some(
        Progress(syncState.downloadedNodesCount, syncState.totalNodesCount.max(1))
      ) //There's always at least one state root to fetch
    )

  private def updatePivotBlock(updateReason: PivotBlockUpdateReason): Unit = {
    if (!pivotBlockHandler.maxAttemptsReached) {
      if (assignedHandlers.nonEmpty || syncState.blockChainWorkQueued) {
        log.info(s"Still waiting for some responses, rescheduling pivot block update")
        scheduler.scheduleOnce(1.second, self, UpdatePivotBlock(updateReason))
        processSyncing()
      } else {
        pivotBlockHandler.askForPivotBlockUpdate()
        context become waitingForPivotBlockUpdate(updateReason)
      }
    } else {
      log.warning(s"Sync failure! Number of pivot block update failures reached maximum.")
      sys.exit(1)
    }
  }

  private def removeRequestHandler(handler: ActorRef): Unit = {
    context unwatch handler
    assignedHandlers -= handler
  }

  @tailrec
  private def processHeaders(peer: Peer, headers: Seq[BlockHeader]): HeaderProcessingResult = {
    if (headers.nonEmpty) {
      val header = headers.head
      processHeader(header, peer) match {
        case Left(result) => result
        case Right((header, weight)) =>
          updateSyncState(header, weight)
          if (header.number == pivotBlockHandler.safeDownloadTarget) {
            ImportedPivotBlock
          } else {
            processHeaders(peer, headers.tail)
          }
      }
    } else
      HeadersProcessingFinished
  }

  private def validateHeader(header: BlockHeader, peer: Peer): Either[HeaderProcessingResult, BlockHeader] = {
    val shouldValidate = header.number >= syncState.nextBlockToFullyValidate

    if (shouldValidate) {
      validator.validate(header) match {
        case Right(_) =>
          updateValidationState(header)
          Right(header)

        case Left(error) =>
          log.warning(s"Block header validation failed during fast sync at block ${header.number}: $error")
          Left(ValidationFailed(header, peer))
      }
    } else {
      Right(header)
    }
  }

  private def updateSyncState(header: BlockHeader, parentWeight: ChainWeight): Unit = {
    storage.updateSyncState(header, parentWeight)

    if (header.number > syncState.bestBlockHeaderNumber) {
      syncState = syncState.copy(bestBlockHeaderNumber = header.number)
    }

    syncState = syncState
      .enqueueBlockBodies(Seq(header.hash))
      .enqueueReceipts(Seq(header.hash))
  }

  private def updateValidationState(header: BlockHeader): Unit = {
    syncState = syncState.updateNextBlockToValidate(
      header,
      pivotBlockHandler.pivotBlock,
      fastSyncBlockValidationK,
      fastSyncBlockValidationX
    )
  }

  private def processHeader(
      header: BlockHeader,
      peer: Peer
  ): Either[HeaderProcessingResult, (BlockHeader, ChainWeight)] =
    for {
      validatedHeader <- validateHeader(header, peer)
      parentWeight <- storage.getParentChainWeight(header)
    } yield (validatedHeader, parentWeight)

  private def handleRewind(header: BlockHeader, peer: Peer, N: Int, duration: FiniteDuration): Unit = {
    blacklist(peer.id, duration, "block header validation failed")
    if (header.number <= pivotBlockHandler.safeDownloadTarget) {
      storage.discardLastBlocks(header.number, N)
      syncState = syncState.updateDiscardedBlocks(header, N)
      if (header.number >= pivotBlockHandler.pivotBlock.number) {
        updatePivotBlock(LastBlockValidationFailed)
      } else {
        processSyncing()
      }
    } else {
      processSyncing()
    }
  }

  private def handleBlockHeaders(peer: Peer, headers: Seq[BlockHeader], requested: BigInt) = {
    if (validator.checkHeadersChain(headers, requested, syncState)) {
      processHeaders(peer, headers) match {
        case ParentChainWeightNotFound(header) =>
          // We could end in wrong fork and get blocked so we should rewind our state a little
          // we blacklist peer just in case we got malicious peer which would send us bad blocks, forcing us to rollback
          // to genesis
          log.warning("Parent chain weight not found for block {}, not processing rest of headers", header.idTag)
          handleRewind(header, peer, syncConfig.fastSyncBlockValidationN, syncConfig.blacklistDuration)
        case HeadersProcessingFinished =>
          processSyncing()
        case ImportedPivotBlock =>
          updatePivotBlock(ImportedLastBlock)
        case ValidationFailed(header, peerToBlackList) =>
          log.warning(s"validation of header ${header.idTag} failed")
          // pow validation failure indicate that either peer is malicious or it is on wrong fork
          handleRewind(
            header,
            peerToBlackList,
            syncConfig.fastSyncBlockValidationN,
            syncConfig.criticalBlacklistDuration
          )
      }
    } else {
      blacklist(peer.id, blacklistDuration, "error in block headers response")
      processSyncing()
    }
  }

  private def handleBlockBodies(peer: Peer, requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]) = {
    if (blockBodies.isEmpty) {
      val reason =
        s"got empty block bodies response for known hashes: ${requestedHashes.map(ByteStringUtils.hash2string)}"
      blacklist(peer.id, blacklistDuration, reason)
      syncState = syncState.enqueueBlockBodies(requestedHashes)
    } else {
      validator.validateBlocks(requestedHashes, blockBodies) match {
        case BlockBodyValidationResult.Valid =>
          insertBlocks(requestedHashes, blockBodies)
        case BlockBodyValidationResult.Invalid =>
          blacklist(
            peer.id,
            blacklistDuration,
            s"responded with block bodies not matching block headers, blacklisting for $blacklistDuration"
          )
          syncState = syncState.enqueueBlockBodies(requestedHashes)
        case BlockBodyValidationResult.DbError =>
          redownloadBlockchain()
      }
    }

    processSyncing()
  }

  private def handleReceipts(peer: Peer, requestedHashes: Seq[ByteString], receipts: Seq[Seq[Receipt]]) = {
    if (receipts.isEmpty) {
      val reason = s"got empty receipts for known hashes: ${requestedHashes.map(ByteStringUtils.hash2string)}"
      blacklist(peer.id, blacklistDuration, reason)
      syncState = syncState.enqueueReceipts(requestedHashes)
    } else {
      validator.validateReceipts(requestedHashes, receipts) match {
        case ReceiptsValidationResult.Valid(blockHashesWithReceipts) =>
          storage.storeReceipts(blockHashesWithReceipts)
          val receivedHashes = blockHashesWithReceipts.unzip._1
          updateBestBlockIfNeeded(receivedHashes)

          val remainingReceipts = requestedHashes.drop(receipts.size)
          syncState = syncState.enqueueReceipts(remainingReceipts)

        case ReceiptsValidationResult.Invalid(error) =>
          val reason =
            s"got invalid receipts for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}" +
              s" due to: $error"
          blacklist(peer.id, blacklistDuration, reason)
          syncState = syncState.enqueueReceipts(requestedHashes)

        case ReceiptsValidationResult.DbError =>
          redownloadBlockchain()
      }
    }

    processSyncing()
  }

  private def handleRequestFailure(peer: Peer, handler: ActorRef, reason: String) = {
    removeRequestHandler(handler)

    syncState = syncState
      .enqueueBlockBodies(requestedBlockBodies.getOrElse(handler, Nil))
      .enqueueReceipts(requestedReceipts.getOrElse(handler, Nil))

    requestedBlockBodies = requestedBlockBodies - handler
    requestedReceipts = requestedReceipts - handler

    requestedHeaders -= peer
    if (handshakedPeers.contains(peer)) {
      blacklist(peer.id, blacklistDuration, reason)
    }
  }

  /**
    * Restarts download from a few blocks behind the current best block header, as an unexpected DB error happened
    */
  private def redownloadBlockchain(): Unit = {
    syncState = syncState.copy(
      blockBodiesQueue = Seq.empty,
      receiptsQueue = Seq.empty,
      //todo adjust the formula to minimize redownloaded block headers
      bestBlockHeaderNumber = (syncState.bestBlockHeaderNumber - 2 * blockHeadersPerRequest).max(0)
    )
    log.debug("missing block header for known hash")
  }

  private def persistSyncState(): Unit = {
    val bodies = requestedBlockBodies.values.flatten.toSeq
    val receipts = requestedReceipts.values.flatten.toSeq
    storage.persistSyncState(buildPersistentSyncState(), bodies, receipts)
  }

  private def buildPersistentSyncState(): PersistentSyncState = {
    PersistentSyncState(
      pivotBlockHandler.pivotBlock,
      syncState.lastFullBlockNumber,
      pivotBlockHandler.safeDownloadTarget,
      syncState.blockBodiesQueue,
      syncState.receiptsQueue,
      syncState.downloadedNodesCount,
      syncState.totalNodesCount,
      syncState.bestBlockHeaderNumber,
      syncState.nextBlockToFullyValidate,
      pivotBlockHandler.pivotBlockUpdateFailures,
      pivotBlockHandler.updatingPivotBlock,
      pivotBlockHandler.stateSyncFinished
    )
  }

  private def printStatus(): Unit = {
    log.info(s"""|Block: ${storage.getBestBlockNumber()}/${pivotBlockHandler.pivotBlock.number}.
          |Peers waiting_for_response/connected: ${assignedHandlers.size}/${handshakedPeers.size} (${blacklistedPeers.size} blacklisted).
          |State: ${syncState.downloadedNodesCount}/${syncState.totalNodesCount} nodes.
          |""".stripMargin.replace("\n", " "))
    log.debug(
      s"""|Connection status: connected(${assignedHandlers.values.map(_.formattedAddress).toSeq.sorted.mkString(", ")})/
          |handshaked(${handshakedPeers.keys.map(_.formattedAddress).toSeq.sorted.mkString(", ")})
          | blacklisted(${blacklistedPeers.map { case (id, _) => id.value }.mkString(", ")})
          |""".stripMargin.replace("\n", " ")
    )
  }

  private def insertBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): Unit = {
    storage.storeBlocks(requestedHashes, blockBodies)
    val receivedHashes = requestedHashes.take(blockBodies.size)
    updateBestBlockIfNeeded(receivedHashes)
    val remainingBlockBodies = requestedHashes.drop(blockBodies.size)
    syncState = syncState.enqueueBlockBodies(remainingBlockBodies)
  }

  def noBlockchainWorkRemaining: Boolean =
    syncState.isBlockchainWorkFinished(pivotBlockHandler.safeDownloadTarget) && assignedHandlers.isEmpty

  def processSyncing(): Unit = {
    SyncMetrics.measure(buildPersistentSyncState())
    if (fullySynced) {
      finish()
    } else {
      if (blockchainDataToDownload) {
        processDownloads()
      } else if (
        noBlockchainWorkRemaining && !syncState.stateSyncFinished && pivotBlockHandler.notInTheMiddleOfUpdate
      ) {
        if (pivotBlockHandler.pivotBlockIsStale(peersToDownloadFrom)) {
          log.info("Restarting state sync to new pivot block")
          storage.requestSyncRestart()
          pivotBlockHandler.stateSyncRestartRequested = true
        }
      } else {
        log.info("No more items to request, waiting for {} responses", assignedHandlers.size)
      }
    }
  }

  def finish(): Unit = {
    log.info("Block synchronization in fast mode finished, switching to regular mode")
    // We have downloaded to target + fastSyncBlockValidationX, se we must discard those last blocks
    storage.discardLastBlocks(pivotBlockHandler.safeDownloadTarget, syncConfig.fastSyncBlockValidationX - 1)
    cancelScheduledTasks()
    storage.persistFastSyncDone()
    context.parent ! FastSync.Done // Should parent also watch this actor?
    context.stop(self)
  }

  def cancelScheduledTasks(): Unit = {
    heartBeat.cancel()
    syncStatePersistCancellable.cancel()
    printStatusCancellable.cancel()
  }

  def processDownloads(): Unit = {
    if (unassignedPeers.isEmpty) {
      if (assignedHandlers.nonEmpty) {
        log.debug("There are no available peers, waiting for responses")
      } else {
        scheduler.scheduleOnce(syncRetryInterval, self, ProcessSyncing)
      }
    } else {
      val now = Instant.now()
      val peers = unassignedPeers
        .filter(p => peerRequestsTime.get(p.peer).forall(d => d.plusMillis(fastSyncThrottle.toMillis).isBefore(now)))
      peers
        .take(maxConcurrentRequests - assignedHandlers.size)
        .sortBy(_.info.maxBlockNumber)(Ordering[BigInt].reverse)
        .foreach(assignBlockchainWork)
    }
  }

  def assignBlockchainWork(peerWithInfo: PeerWithInfo): Unit = {
    val PeerWithInfo(peer, peerInfo) = peerWithInfo
    if (syncState.receiptsQueue.nonEmpty) {
      requestReceipts(peer)
    } else if (syncState.blockBodiesQueue.nonEmpty) {
      requestBlockBodies(peer)
    } else if (
      requestedHeaders.isEmpty &&
      context.child(BlockHeadersHandlerName).isEmpty &&
      syncState.bestBlockHeaderNumber < pivotBlockHandler.safeDownloadTarget &&
      peerInfo.maxBlockNumber >= pivotBlockHandler.pivotBlock.number
    ) {
      requestBlockHeaders(peer)
    }
  }

  def requestReceipts(peer: Peer): Unit = {
    val (receiptsToGet, remainingReceipts) = syncState.receiptsQueue.splitAt(receiptsPerRequest)

    val handler = context.actorOf(
      PeerRequestHandler.props[GetReceipts, Receipts](
        peer,
        peerResponseTimeout,
        etcPeerManager,
        peerEventBus,
        requestMsg = GetReceipts(receiptsToGet),
        responseMsgCode = Codes.ReceiptsCode
      )
    )

    context watch handler
    assignedHandlers += (handler -> peer)
    peerRequestsTime += (peer -> Instant.now())
    syncState = syncState.copy(receiptsQueue = remainingReceipts)
    requestedReceipts += handler -> receiptsToGet
  }

  def requestBlockBodies(peer: Peer): Unit = {
    val (blockBodiesToGet, remainingBlockBodies) = syncState.blockBodiesQueue.splitAt(blockBodiesPerRequest)

    val handler = context.actorOf(
      PeerRequestHandler.props[GetBlockBodies, BlockBodies](
        peer,
        peerResponseTimeout,
        etcPeerManager,
        peerEventBus,
        requestMsg = GetBlockBodies(blockBodiesToGet),
        responseMsgCode = Codes.BlockBodiesCode
      )
    )

    context watch handler
    assignedHandlers += (handler -> peer)
    peerRequestsTime += (peer -> Instant.now())
    syncState = syncState.copy(blockBodiesQueue = remainingBlockBodies)
    requestedBlockBodies += handler -> blockBodiesToGet
  }

  def requestBlockHeaders(peer: Peer): Unit = {
    val limit: BigInt =
      if (blockHeadersPerRequest < (pivotBlockHandler.safeDownloadTarget - syncState.bestBlockHeaderNumber))
        blockHeadersPerRequest
      else
        pivotBlockHandler.safeDownloadTarget - syncState.bestBlockHeaderNumber

    val handler = context.actorOf(
      PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
        peer,
        peerResponseTimeout,
        etcPeerManager,
        peerEventBus,
        requestMsg = GetBlockHeaders(Left(syncState.bestBlockHeaderNumber + 1), limit, skip = 0, reverse = false),
        responseMsgCode = Codes.BlockHeadersCode
      ),
      BlockHeadersHandlerName
    )

    context watch handler
    assignedHandlers += (handler -> peer)
    requestedHeaders += (peer -> limit)
    peerRequestsTime += (peer -> Instant.now())
  }

  def unassignedPeers: List[PeerWithInfo] =
    (peersToDownloadFrom -- assignedHandlers.values).map(PeerWithInfo.tupled).toList

  def blockchainDataToDownload: Boolean =
    syncState.blockChainWorkQueued || syncState.bestBlockHeaderNumber < pivotBlockHandler.safeDownloadTarget

  def fullySynced: Boolean = {
    syncState.isBlockchainWorkFinished(pivotBlockHandler.safeDownloadTarget) &&
    assignedHandlers.isEmpty &&
    syncState.stateSyncFinished
  }

  private def updateBestBlockIfNeeded(receivedHashes: Seq[ByteString]): Unit = {
    val lastStoredBestBlockNumber = storage.updateBestBlockIfNeeded(receivedHashes)
    lastStoredBestBlockNumber.foreach(n => syncState = syncState.copy(lastFullBlockNumber = n))
  }
}

object SyncingHandler {

  def props(
      initialSyncState: PersistentSyncState,
      fastSyncStorageHelper: SyncingHandlerStorage,
      validator: FastSyncValidator,
      peerEventBus: ActorRef,
      etcPeerManager: ActorRef,
      pivotBlockHandler: PivotBlockHandler,
      syncConfig: SyncConfig,
      scheduler: Scheduler
  ): Props = {
    Props(
      new SyncingHandler(
        initialSyncState,
        fastSyncStorageHelper,
        validator,
        peerEventBus,
        etcPeerManager,
        pivotBlockHandler,
        syncConfig
      )(scheduler)
    )
  }

  case object ProcessSyncing
  case class AskForPivotBlockUpdate(reason: PivotBlockUpdateReason)
}
