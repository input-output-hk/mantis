package io.iohk.ethereum.blockchain.sync.fast

import akka.actor._
import akka.util.ByteString
import cats.data.NonEmptyList
import cats.implicits._
import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistReason._
import io.iohk.ethereum.blockchain.sync.Blacklist._
import io.iohk.ethereum.blockchain.sync.PeerListSupportNg.PeerWithInfo
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status.Progress
import io.iohk.ethereum.blockchain.sync._
import io.iohk.ethereum.blockchain.sync.fast.HeaderSkeleton._
import io.iohk.ethereum.blockchain.sync.fast.ReceiptsValidator.ReceiptsValidationResult
import io.iohk.ethereum.blockchain.sync.fast.SyncBlocksValidator.BlockBodyValidationResult
import io.iohk.ethereum.blockchain.sync.fast.SyncStateSchedulerActor.{
  RestartRequested,
  StartSyncingTo,
  StateSyncFinished,
  WaitingForNewTargetBlock
}
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.db.storage.{AppStateStorage, EvmCodeStorage, FastSyncStateStorage, NodeStorage}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.ETH62._
import io.iohk.ethereum.network.p2p.messages.ETH63._
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import org.bouncycastle.util.encoders.Hex

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random
import scala.collection.mutable
import scala.util.Try
import scala.util.Success
import java.util.concurrent.atomic.AtomicInteger

// scalastyle:off file.size.limit
class FastSync(
    val fastSyncStateStorage: FastSyncStateStorage,
    val appStateStorage: AppStateStorage,
    val blockchain: Blockchain,
    val blockchainReader: BlockchainReader,
    evmCodeStorage: EvmCodeStorage,
    nodeStorage: NodeStorage,
    val validators: Validators,
    val peerEventBus: ActorRef,
    val etcPeerManager: ActorRef,
    val blacklist: Blacklist,
    val syncConfig: SyncConfig,
    implicit val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with PeerListSupportNg
    with ReceiptsValidator
    with SyncBlocksValidator {

  import FastSync._
  import syncConfig._

  val syncController: ActorRef = context.parent

  override def receive: Receive = idle

  def idle: Receive = handlePeerListMessages orElse {
    case SyncProtocol.Start => start()
    case SyncProtocol.GetStatus => sender() ! SyncProtocol.Status.NotSyncing
  }

  def start(): Unit = {
    log.info("Trying to start block synchronization (fast mode)")
    fastSyncStateStorage.getSyncState() match {
      case Some(syncState) => startWithState(syncState)
      case None => startFromScratch()
    }
  }

  def startWithState(syncState: SyncState): Unit = {
    log.info("Starting fast sync with existing state and asking for new pivot block")
    val syncingHandler = new SyncingHandler(syncState)
    syncingHandler.askForPivotBlockUpdate(SyncRestart)
  }

  def startFromScratch(): Unit = {
    log.info("Starting fast sync from scratch")
    val pivotBlockSelector = context.actorOf(
      PivotBlockSelector.props(etcPeerManager, peerEventBus, syncConfig, scheduler, context.self, blacklist),
      "pivot-block-selector"
    )
    pivotBlockSelector ! PivotBlockSelector.SelectPivotBlock
    context become waitingForPivotBlock
  }

  def waitingForPivotBlock: Receive = handlePeerListMessages orElse {
    case SyncProtocol.GetStatus => sender() ! SyncProtocol.Status.NotSyncing
    case PivotBlockSelector.Result(pivotBlockHeader) =>
      if (pivotBlockHeader.number < 1) {
        log.info("Unable to start block synchronization in fast mode: pivot block is less than 1")
        appStateStorage.fastSyncDone().commit()
        context become idle
        syncController ! Done
      } else {
        val initialSyncState =
          SyncState(
            pivotBlockHeader,
            safeDownloadTarget = pivotBlockHeader.number + syncConfig.fastSyncBlockValidationX
          )
        val syncingHandler = new SyncingHandler(initialSyncState)
        context.become(syncingHandler.receive)
        syncingHandler.processSyncing()
      }
  }

  private val actorCounter = new AtomicInteger
  private def countActor: Int = actorCounter.incrementAndGet

  // scalastyle:off number.of.methods
  private class SyncingHandler(initialSyncState: SyncState, var masterPeer: Option[Peer] = None) {

    //not part of syncstate as we do not want to persist is.
    private var stateSyncRestartRequested = false

    private var requestedHeaders: Map[Peer, HeaderRange] = Map.empty

    private var syncState = initialSyncState

    private var assignedHandlers: Map[ActorRef, Peer] = Map.empty
    private var peerRequestsTime: Map[Peer, Instant] = Map.empty

    // TODO ETCM-701 get rid of state and move skeleton download to a separate actor
    private val blockHeadersQueue: mutable.Queue[HeaderRange] = mutable.Queue.empty
    private var currentSkeletonState: Option[HeaderSkeleton] = None
    private var skeletonHandler: Option[ActorRef] = None
    private var batchFailuresCount = 0

    private var requestedBlockBodies: Map[ActorRef, Seq[ByteString]] = Map.empty
    private var requestedReceipts: Map[ActorRef, Seq[ByteString]] = Map.empty

    private val syncStateStorageActor = context.actorOf(Props[StateStorageActor](), s"$countActor-state-storage")
    syncStateStorageActor ! fastSyncStateStorage

    private val branchResolver = context.actorOf(
      FastSyncBranchResolverActor
        .props(self, peerEventBus, etcPeerManager, blockchain, blacklist, syncConfig, appStateStorage, scheduler),
      s"$countActor-fast-sync-branch-resolver"
    )

    private val syncStateScheduler = context.actorOf(
      SyncStateSchedulerActor
        .props(
          SyncStateScheduler(blockchain, evmCodeStorage, nodeStorage, syncConfig.stateSyncBloomFilterSize),
          syncConfig,
          etcPeerManager,
          peerEventBus,
          blacklist,
          scheduler
        ),
      s"$countActor-state-scheduler"
    )

    //Delay before starting to persist snapshot. It should be 0, as the presence of it marks that fast sync was started
    private val persistStateSnapshotDelay: FiniteDuration = 0.seconds
    private val syncStatePersistCancellable =
      scheduler.scheduleWithFixedDelay(persistStateSnapshotDelay, persistStateSnapshotInterval, self, PersistSyncState)
    private val printStatusCancellable =
      scheduler.scheduleWithFixedDelay(printStatusInterval, printStatusInterval, self, PrintStatus)
    private val heartBeat =
      scheduler.scheduleWithFixedDelay(syncRetryInterval, syncRetryInterval * 2, self, ProcessSyncing)

    private val startTime: Long = System.currentTimeMillis()
    private def totalMinutesTaken(): Long = TimeUnit.MILLISECONDS.toMinutes(System.currentTimeMillis() - startTime)

    def handleStatus: Receive = {
      case SyncProtocol.GetStatus => sender() ! currentSyncingStatus
      case SyncStateSchedulerActor.StateSyncStats(saved, missing) =>
        syncState = syncState.copy(downloadedNodesCount = saved, totalNodesCount = saved + missing)
    }

    def receive: Receive = handlePeerListMessages orElse handleStatus orElse handleRequestFailure orElse {
      case UpdatePivotBlock(reason) => updatePivotBlock(reason)
      case WaitingForNewTargetBlock =>
        log.info("State sync stopped until receiving new pivot block")
        updatePivotBlock(ImportedLastBlock)
      case ProcessSyncing => processSyncing()
      case PrintStatus => printStatus()
      case PersistSyncState => persistSyncState()
      case r @ ResponseReceived(_, _, _) => handleResponses(r)
      case StateSyncFinished =>
        syncState = syncState.copy(stateSyncFinished = true)
        processSyncing()
    }

    def handleRequestFailure: Receive = {
      case PeerRequestHandler.RequestFailed(peer, reason) =>
        handleRequestFailure(peer, sender(), FastSyncRequestFailed(reason))
      case Terminated(ref) =>
        assignedHandlers.get(ref).foreach {
          handleRequestFailure(_, ref, PeerActorTerminated)
        }
    }

    // TODO ETCM-701 will be moved to separate actor and refactored
    private def handleResponses: Receive = {
      case ResponseReceived(peer, BlockHeaders(blockHeaders), timeTaken) =>
        log.info(
          "*** Received {} block headers from peer [{}] in {} ms ***",
          blockHeaders.size,
          peer.id,
          timeTaken
        )
        FastSyncMetrics.setBlockHeadersDownloadTime(timeTaken)
        currentSkeletonState match {
          case Some(currentSkeleton) =>
            if (skeletonHandler.contains(sender())) handleSkeletonResponse(peer, blockHeaders, currentSkeleton)
            else handleHeaderBatchResponse(peer, blockHeaders, currentSkeleton)
          case None =>
            log.warning(
              s"Received response to fill in header skeleton, but current header skeleton is not defined."
            )
            processSyncing()
        }
      case ResponseReceived(peer, BlockBodies(blockBodies), timeTaken) =>
        log.info("Received {} block bodies from peer [{}] in {} ms", blockBodies.size, peer.id, timeTaken)
        FastSyncMetrics.setBlockBodiesDownloadTime(timeTaken)

        val requestedBodies = requestedBlockBodies.getOrElse(sender(), Nil)
        requestedBlockBodies -= sender()
        removeRequestHandler(sender())
        handleBlockBodies(peer, requestedBodies, blockBodies)
      case ResponseReceived(peer, Receipts(receipts), timeTaken) =>
        log.info("Received {} receipts from peer [{}] in {} ms", receipts.size, peer.id, timeTaken)
        FastSyncMetrics.setBlockReceiptsDownloadTime(timeTaken)

        val requestedHashes = requestedReceipts.getOrElse(sender(), Nil)
        requestedReceipts -= sender()
        removeRequestHandler(sender())
        handleReceipts(peer, requestedHashes, receipts)
    }

    private def handleSkeletonResponse(
        peer: Peer,
        blockHeaders: Seq[BlockHeader],
        currentSkeleton: HeaderSkeleton
    ): Unit = {
      def validateDownloadedHeaders = blockHeaders.toList.traverse_(validateHeaderOnly)

      log.info("Handling new received skeleton from peer [{}].", peer.id.value)

      skeletonHandler.foreach(context.unwatch)
      skeletonHandler = None

      validateDownloadedHeaders match {
        case Left(error) =>
          log.info(s"Validation of skeleton from $peer failed: $error")
          blockHeadersError(peer, BlacklistReason.BlockHeaderValidationFailed)
        case Right(_) =>
          currentSkeleton.setSkeletonHeaders(blockHeaders) match {
            case Left(error) =>
              // TODO ETCM-701 if this error keeps happening, switch master peer
              log.warning("Failed to set skeleton headers from peer [{}]: [{}]", peer.id.value, error.msg)
              requestSkeletonHeaders(peer)
            case Right(updatedSkeleton) =>
              log.debug(
                "Updated current skeleton header. Included batches (starting numbers): [{}]",
                updatedSkeleton.batchStartingHeaderNumbers.mkString(", ")
              )
              currentSkeletonState = Some(updatedSkeleton)

              val blockHeadersToRequest =
                updatedSkeleton.batchStartingHeaderNumbers.map { from =>
                  HeaderRange(from, updatedSkeleton.batchSize)
                }

              blockHeadersQueue.enqueueAll(blockHeadersToRequest)
          }
      }
    }

    private def handleHeaderBatchResponse(
        peer: Peer,
        blockHeaders: Seq[BlockHeader],
        currentSkeleton: HeaderSkeleton
    ): Unit = {
      def validHeadersChain(headers: Seq[BlockHeader], requestedNum: BigInt): Boolean = {
        headers.nonEmpty && headers.size <= requestedNum && checkHeadersChain(headers)
      }

      removeRequestHandler(sender())
      requestedHeaders.get(peer) match {
        case Some(requested) =>
          log.debug("Validating [{}] received block headers from peer [{}]", blockHeaders.size, peer.id.value)
          requestedHeaders -= peer
          if (validHeadersChain(blockHeaders, requested.limit))
            fillSkeletonGap(peer, requested, blockHeaders, currentSkeleton)
          else {
            handleHeaderResponseError(
              InvalidDownloadedChain(blockHeaders),
              requested,
              peer,
              BlacklistReason.WrongBlockHeaders
            )
          }
        case None => log.warning("Received block headers from peer [{}] but weren't expecting any.", peer.id.value)
      }
    }

    private def fillSkeletonGap(
        peer: Peer,
        request: HeaderRange,
        blockHeaders: Seq[BlockHeader],
        currentSkeleton: HeaderSkeleton
    ): Unit = {
      log.debug(
        "Attempting to use [{}] block headers from peer [{}] to fill in header skeleton.",
        blockHeaders.size,
        peer.id
      )
      currentSkeleton.addBatch(blockHeaders) match {
        case Right(skeleton) =>
          log.debug("Successfully added headers from peer [{}] to current skeleton.", peer.id.value)
          skeleton.fullChain match {
            case Some(fullChain) =>
              log.debug("Current header skeleton completed. Starting to request bodies and receipts.")
              handleBlockHeadersChain(peer, fullChain)
              currentSkeletonState = None
            case None =>
              log.debug("Skeleton is still incomplete. Waiting for remaining headers.")
              currentSkeletonState = Some(skeleton)
          }
        case Left(error) =>
          log.warning("Failed to add headers from peer [{}] to current skeleton. Error: [{}]", peer.id.value, error.msg)
          handleHeaderResponseError(error, request, peer, BlacklistReason.BlockHeaderValidationFailed)
      }
    }

    private def handleHeaderResponseError(
        error: HeaderSkeletonError,
        request: HeaderRange,
        peer: Peer,
        reason: BlacklistReason
    ): Unit = {
      def handleMasterPeerFailure(header: BlockHeader): Unit = {
        batchFailuresCount += 1
        if (batchFailuresCount > fastSyncMaxBatchRetries) {
          log.info("Max number of allowed failures reached. Switching branch and master peer.")

          blockHeadersQueue.dequeueAll(_ => true)

          handleRewind(header, masterPeer.get, fastSyncBlockValidationN, blacklistDuration, continueSyncing = false)

          // Start branch resolution and wait for response from the FastSyncBranchResolver actor.
          context become waitingForBranchResolution
          branchResolver ! FastSyncBranchResolverActor.StartBranchResolver
        }
      }

      blockHeadersQueue.enqueue(request)
      error match {
        // These are the reasons that make the master peer suspicious
        case InvalidPenultimateHeader(_, header) => handleMasterPeerFailure(header)
        case InvalidBatchHash(_, header) => handleMasterPeerFailure(header)
        // Otherwise probably it's just this peer's fault
        case _ =>
          log.warning(error.msg)
          blockHeadersError(peer, reason)
      }
    }

    private def waitingForBranchResolution: Receive = handleStatus orElse handleRequestFailure orElse {
      case FastSyncBranchResolverActor.BranchResolvedSuccessful(firstCommonBlockNumber, newMasterPeer) =>
        log.debug(
          s"Resolved branch with first common block number $firstCommonBlockNumber for new master peer $newMasterPeer"
        )
        // Reset the batch failures count
        batchFailuresCount = 0

        context.children.foreach { child =>
          log.debug(s"Unwatching and killing $child")
          context.unwatch(child)
          child ! PoisonPill
        }

        // Restart syncing from the valid block available in state.
        log.debug("Starting with fresh SyncingHandler")
        val syncingHandler = new SyncingHandler(
          syncState.copy(
            bestBlockHeaderNumber = firstCommonBlockNumber,
            nextBlockToFullyValidate = firstCommonBlockNumber + 1,
            pivotBlockUpdateFailures = 0
          ),
          masterPeer = Some(newMasterPeer)
        )
        context.become(syncingHandler.receive)
        syncingHandler.processSyncing()

      case _: FastSyncBranchResolverActor.BranchResolutionFailed =>
        // there isn't much we can do if we don't find a branch/peer to continue syncing, so let's try again
        branchResolver ! FastSyncBranchResolverActor.StartBranchResolver
    }

    private def blockHeadersError(peer: Peer, blacklistReason: BlacklistReason): Unit = {
      blacklist.add(peer.id, blacklistDuration, blacklistReason)
      processSyncing()
    }

    def askForPivotBlockUpdate(updateReason: PivotBlockUpdateReason): Unit = {
      syncState = syncState.copy(updatingPivotBlock = true)
      log.info("Asking for new pivot block")
      val pivotBlockSelector = {
        context.actorOf(
          PivotBlockSelector.props(etcPeerManager, peerEventBus, syncConfig, scheduler, context.self, blacklist),
          s"$countActor-pivot-block-selector-update"
        )
      }
      pivotBlockSelector ! PivotBlockSelector.SelectPivotBlock
      context become waitingForPivotBlockUpdate(updateReason)
    }

    private def newPivotIsGoodEnough(
        newPivot: BlockHeader,
        currentState: SyncState,
        updateReason: PivotBlockUpdateReason
    ): Boolean = {
      def stalePivotAfterRestart: Boolean =
        newPivot.number == currentState.pivotBlock.number && updateReason.isSyncRestart
      newPivot.number >= currentState.pivotBlock.number && !stalePivotAfterRestart
    }

    def waitingForPivotBlockUpdate(updateReason: PivotBlockUpdateReason): Receive =
      handlePeerListMessages orElse handleStatus orElse handleRequestFailure orElse {
        case PivotBlockSelector.Result(pivotBlockHeader)
            if newPivotIsGoodEnough(pivotBlockHeader, syncState, updateReason) =>
          log.info("New pivot block with number {} received", pivotBlockHeader.number)
          updatePivotSyncState(updateReason, pivotBlockHeader)
          context become this.receive
          processSyncing()

        case PivotBlockSelector.Result(pivotBlockHeader)
            if !newPivotIsGoodEnough(pivotBlockHeader, syncState, updateReason) =>
          log.info("Received pivot block is older than old one, re-scheduling asking for new one")
          reScheduleAskForNewPivot(updateReason)

        case PersistSyncState => persistSyncState()

        case UpdatePivotBlock(state) => updatePivotBlock(state)
      }

    private def reScheduleAskForNewPivot(updateReason: PivotBlockUpdateReason): Unit = {
      syncState = syncState.copy(pivotBlockUpdateFailures = syncState.pivotBlockUpdateFailures + 1)
      scheduler
        .scheduleOnce(syncConfig.pivotBlockReScheduleInterval, self, UpdatePivotBlock(updateReason))
    }

    def currentSyncingStatus: SyncProtocol.Status =
      SyncProtocol.Status.Syncing(
        initialSyncState.lastFullBlockNumber,
        Progress(syncState.lastFullBlockNumber, syncState.pivotBlock.number),
        Some(
          Progress(syncState.downloadedNodesCount, syncState.totalNodesCount.max(1))
        ) //There's always at least one state root to fetch
      )

    private def updatePivotBlock(updateReason: PivotBlockUpdateReason): Unit = {
      if (syncState.pivotBlockUpdateFailures <= syncConfig.maximumTargetUpdateFailures) {
        if (assignedHandlers.nonEmpty || syncState.blockChainWorkQueued) {
          log.info("Still waiting for some responses, rescheduling pivot block update")
          scheduler.scheduleOnce(1.second, self, UpdatePivotBlock(updateReason))
          processSyncing()
        } else {
          askForPivotBlockUpdate(updateReason)
        }
      } else {
        log.warning("Sync failure! Number of pivot block update failures reached maximum.")
        sys.exit(1)
      }
    }

    private def updatePivotSyncState(updateReason: PivotBlockUpdateReason, pivotBlockHeader: BlockHeader): Unit =
      updateReason match {
        case ImportedLastBlock =>
          if (pivotBlockHeader.number - syncState.pivotBlock.number <= syncConfig.maxTargetDifference) {
            log.info("Current pivot block is fresh enough, starting state download.")
            // Empty root has means that there were no transactions in blockchain, and Mpt trie is empty
            // Asking for this root would result only with empty transactions
            if (syncState.pivotBlock.stateRoot == ByteString(MerklePatriciaTrie.EmptyRootHash)) {
              syncState = syncState.copy(stateSyncFinished = true, updatingPivotBlock = false)
            } else {
              syncState = syncState.copy(updatingPivotBlock = false)
              stateSyncRestartRequested = false
              syncStateScheduler ! StartSyncingTo(pivotBlockHeader.stateRoot, pivotBlockHeader.number)
            }
          } else {
            syncState = syncState.updatePivotBlock(
              pivotBlockHeader,
              syncConfig.fastSyncBlockValidationX,
              updateFailures = false
            )
            log.info(
              "Changing pivot block to {}, new safe target is {}",
              pivotBlockHeader.number,
              syncState.safeDownloadTarget
            )
          }

        case LastBlockValidationFailed =>
          log.info(
            "Changing pivot block after failure, to {}, new safe target is {}",
            pivotBlockHeader.number,
            syncState.safeDownloadTarget
          )
          syncState =
            syncState.updatePivotBlock(pivotBlockHeader, syncConfig.fastSyncBlockValidationX, updateFailures = true)

        case SyncRestart =>
          // in case of node restart we are sure that new pivotBlockHeader > current pivotBlockHeader
          syncState = syncState.updatePivotBlock(
            pivotBlockHeader,
            syncConfig.fastSyncBlockValidationX,
            updateFailures = false
          )
          log.info(
            "Changing pivot block to {}, new safe target is {}",
            pivotBlockHeader.number,
            syncState.safeDownloadTarget
          )
      }

    private def removeRequestHandler(handler: ActorRef): Unit = {
      log.debug(s"Removing request handler ${handler.path}")
      context unwatch handler
      skeletonHandler = skeletonHandler.filter(_ != handler)
      assignedHandlers -= handler
    }

    // TODO [ETCM-676]: Move to blockchain and make sure it's atomic
    private def discardLastBlocks(startBlock: BigInt, blocksToDiscard: Int): Unit = {
      (startBlock to ((startBlock - blocksToDiscard) max 1) by -1).foreach { n =>
        blockchain.getBlockHeaderByNumber(n).foreach { headerToRemove =>
          blockchain.removeBlock(headerToRemove.hash, withState = false)
        }
      }
      // TODO (maybe ETCM-77): Manage last checkpoint number too
      appStateStorage.putBestBlockNumber((startBlock - blocksToDiscard - 1) max 0).commit()
    }

    private def validateHeader(header: BlockHeader, peer: Peer): Either[HeaderProcessingResult, BlockHeader] = {
      val shouldValidate = header.number >= syncState.nextBlockToFullyValidate

      if (shouldValidate) {
        validators.blockHeaderValidator.validate(header, blockchainReader.getBlockHeaderByHash) match {
          case Right(_) =>
            updateValidationState(header)
            Right(header)

          case Left(error) =>
            log.warning("Block header validation failed during fast sync at block {}: {}", header.number, error)
            Left(ValidationFailed(header, peer))
        }
      } else {
        Right(header)
      }
    }

    private def updateSyncState(header: BlockHeader, parentWeight: ChainWeight): Unit = {
      blockchain
        .storeBlockHeader(header)
        .and(blockchain.storeChainWeight(header.hash, parentWeight.increase(header)))
        .commit()

      if (header.number > syncState.bestBlockHeaderNumber) {
        syncState = syncState.copy(bestBlockHeaderNumber = header.number)
      }

      syncState = syncState
        .enqueueBlockBodies(Seq(header.hash))
        .enqueueReceipts(Seq(header.hash))
    }

    private def updateValidationState(header: BlockHeader): Unit = {
      import syncConfig.{fastSyncBlockValidationK => K, fastSyncBlockValidationX => X}
      syncState = syncState.updateNextBlockToValidate(header, K, X)
    }

    private def handleRewind(
        header: BlockHeader,
        peer: Peer,
        N: Int,
        duration: FiniteDuration,
        continueSyncing: Boolean = true
    ): Unit = {
      blacklist.add(peer.id, duration, BlockHeaderValidationFailed)
      if (header.number <= syncState.safeDownloadTarget) {
        discardLastBlocks(header.number, N)
        syncState = syncState.updateDiscardedBlocks(header, N)
        if (header.number >= syncState.pivotBlock.number) {
          updatePivotBlock(LastBlockValidationFailed)
        } else if (continueSyncing) {
          processSyncing()
        }
      } else if (continueSyncing) {
        processSyncing()
      }
    }

    // scalastyle:off method.length
    private def handleBlockHeadersChain(peer: Peer, headers: Seq[BlockHeader]): Unit = {
      def processHeader(header: BlockHeader): Either[HeaderProcessingResult, (BlockHeader, ChainWeight)] =
        for {
          validatedHeader <- validateHeader(header, peer)
          parentWeight <- getParentChainWeight(header)
        } yield (validatedHeader, parentWeight)

      def getParentChainWeight(header: BlockHeader) = {
        blockchain.getChainWeightByHash(header.parentHash).toRight(ParentChainWeightNotFound(header))
      }

      @tailrec
      def processHeaders(headers: Seq[BlockHeader]): HeaderProcessingResult = {
        if (headers.nonEmpty) {
          val header = headers.head
          processHeader(header) match {
            case Left(result) => result
            case Right((header, weight)) =>
              updateSyncState(header, weight)
              if (header.number == syncState.safeDownloadTarget) {
                ImportedPivotBlock
              } else {
                processHeaders(headers.tail)
              }
          }
        } else
          HeadersProcessingFinished
      }

      processHeaders(headers) match {
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
          log.warning("validation of header {} failed", header.idTag)
          // pow validation failure indicate that either peer is malicious or it is on wrong fork
          handleRewind(
            header,
            peerToBlackList,
            syncConfig.fastSyncBlockValidationN,
            syncConfig.criticalBlacklistDuration
          )
      }
    }

    private def handleBlockBodies(peer: Peer, requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): Unit = {
      if (blockBodies.isEmpty) {
        val knownHashes = requestedHashes.map(ByteStringUtils.hash2string)
        blacklist.add(peer.id, blacklistDuration, EmptyBlockBodies(knownHashes))
        syncState = syncState.enqueueBlockBodies(requestedHashes)
      } else {
        validateBlocks(requestedHashes, blockBodies) match {
          case BlockBodyValidationResult.Valid =>
            insertBlocks(requestedHashes, blockBodies)
          case BlockBodyValidationResult.Invalid =>
            blacklist.add(peer.id, blacklistDuration, BlockBodiesNotMatchingHeaders)
            syncState = syncState.enqueueBlockBodies(requestedHashes)
          case BlockBodyValidationResult.DbError =>
            redownloadBlockchain()
        }
      }

      processSyncing()
    }

    private def handleReceipts(peer: Peer, requestedHashes: Seq[ByteString], receipts: Seq[Seq[Receipt]]): Unit = {
      if (receipts.isEmpty) {
        val knownHashes = requestedHashes.map(ByteStringUtils.hash2string)
        blacklist.add(peer.id, blacklistDuration, EmptyReceipts(knownHashes))
        syncState = syncState.enqueueReceipts(requestedHashes)
      } else {
        validateReceipts(requestedHashes, receipts) match {
          case ReceiptsValidationResult.Valid(blockHashesWithReceipts) =>
            blockHashesWithReceipts
              .map { case (hash, receiptsForBlock) =>
                blockchain.storeReceipts(hash, receiptsForBlock)
              }
              .reduce(_.and(_))
              .commit()

            val receivedHashes = blockHashesWithReceipts.map(_._1)
            updateBestBlockIfNeeded(receivedHashes)

            val remainingReceipts = requestedHashes.drop(receipts.size)
            if (remainingReceipts.nonEmpty) {
              syncState = syncState.enqueueReceipts(remainingReceipts)
            }

          case ReceiptsValidationResult.Invalid(error) =>
            val knownHashes = requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))
            blacklist.add(peer.id, blacklistDuration, InvalidReceipts(knownHashes, error))
            syncState = syncState.enqueueReceipts(requestedHashes)

          case ReceiptsValidationResult.DbError =>
            redownloadBlockchain()
        }
      }

      processSyncing()
    }

    private def handleRequestFailure(peer: Peer, handler: ActorRef, reason: BlacklistReason): Unit = {
      if (skeletonHandler == Some(handler))
        currentSkeletonState = None

      removeRequestHandler(handler)

      requestedHeaders.get(peer).foreach(blockHeadersQueue.enqueue)
      syncState = syncState
        .enqueueBlockBodies(requestedBlockBodies.getOrElse(handler, Nil))
        .enqueueReceipts(requestedReceipts.getOrElse(handler, Nil))

      requestedHeaders -= peer
      requestedBlockBodies = requestedBlockBodies - handler
      requestedReceipts = requestedReceipts - handler

      blacklistIfHandshaked(peer.id, blacklistDuration, reason)
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
      log.debug("Missing block header for known hash")
    }

    private def persistSyncState(): Unit = {
      syncStateStorageActor ! syncState.copy(
        blockBodiesQueue = requestedBlockBodies.values.flatten.toSeq.distinct ++ syncState.blockBodiesQueue,
        receiptsQueue = requestedReceipts.values.flatten.toSeq.distinct ++ syncState.receiptsQueue
      )
    }

    private def printStatus(): Unit = {
      def formatPeerEntry(entry: PeerWithInfo): String = formatPeer(entry.peer)
      def formatPeer(peer: Peer): String =
        s"${peer.remoteAddress.getAddress.getHostAddress}:${peer.remoteAddress.getPort}"
      val blacklistedIds = blacklist.keys
      log.info(
        s"""|Block: {}/${syncState.pivotBlock.number}.
            |Peers waiting_for_response/connected: ${assignedHandlers.size}/${handshakedPeers.size} (${blacklistedIds.size} blacklisted).
            |State: ${syncState.downloadedNodesCount}/${syncState.totalNodesCount} nodes.
            |""".stripMargin.replace("\n", " "),
        appStateStorage.getBestBlockNumber()
      )
      log.debug(
        s"""|Connection status: connected({})/
            |handshaked({})
            | blacklisted({})
            |""".stripMargin.replace("\n", " "),
        assignedHandlers.values.map(formatPeer).toSeq.sorted.mkString(", "),
        handshakedPeers.values.toList.map(e => formatPeerEntry(e)).sorted.mkString(", "),
        blacklistedIds.map(_.value).mkString(", ")
      )
    }

    private def insertBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): Unit = {
      (requestedHashes zip blockBodies)
        .map { case (hash, body) =>
          blockchain.storeBlockBody(hash, body)
        }
        .reduce(_.and(_))
        .commit()

      val receivedHashes = requestedHashes.take(blockBodies.size)
      updateBestBlockIfNeeded(receivedHashes)
      val remainingBlockBodies = requestedHashes.drop(blockBodies.size)
      if (remainingBlockBodies.nonEmpty) {
        syncState = syncState.enqueueBlockBodies(remainingBlockBodies)
      }
    }

    def hasBestBlockFreshEnoughToUpdatePivotBlock(info: PeerInfo, state: SyncState, syncConfig: SyncConfig): Boolean = {
      (info.maxBlockNumber - syncConfig.pivotBlockOffset) - state.pivotBlock.number >= syncConfig.maxPivotBlockAge
    }

    private def getPeersWithFreshEnoughPivot(
        peers: NonEmptyList[PeerWithInfo],
        state: SyncState,
        syncConfig: SyncConfig
    ): List[(Peer, BigInt)] = {
      peers.collect {
        case PeerWithInfo(peer, info) if hasBestBlockFreshEnoughToUpdatePivotBlock(info, state, syncConfig) =>
          (peer, info.maxBlockNumber)
      }
    }

    def noBlockchainWorkRemaining: Boolean =
      syncState.isBlockchainWorkFinished && assignedHandlers.isEmpty

    def notInTheMiddleOfUpdate: Boolean =
      !(syncState.updatingPivotBlock || stateSyncRestartRequested)

    def pivotBlockIsStale(): Boolean = {
      val peersWithInfo = peersToDownloadFrom.values.toList
      if (peersWithInfo.isEmpty) {
        false
      } else {
        val peerWithBestBlockInNetwork = peersWithInfo.maxBy(_.peerInfo.maxBlockNumber)

        val bestPossibleTargetDifferenceInNetwork =
          (peerWithBestBlockInNetwork.peerInfo.maxBlockNumber - syncConfig.pivotBlockOffset) - syncState.pivotBlock.number

        val peersWithTooFreshPossiblePivotBlock =
          getPeersWithFreshEnoughPivot(NonEmptyList.fromListUnsafe(peersWithInfo), syncState, syncConfig)

        if (peersWithTooFreshPossiblePivotBlock.isEmpty) {
          log.info(
            s"There are no peers with too fresh possible pivot block. " +
              s"Current pivot block is {} blocks behind best possible target",
            bestPossibleTargetDifferenceInNetwork
          )
          false
        } else {
          val pivotBlockIsStale = peersWithTooFreshPossiblePivotBlock.size >= minPeersToChoosePivotBlock

          log.info(
            "There are {} peers with possible new pivot block, " +
              "best known pivot in current peer list has number {}",
            peersWithTooFreshPossiblePivotBlock.size,
            peerWithBestBlockInNetwork.peerInfo.maxBlockNumber
          )

          pivotBlockIsStale
        }
      }
    }

    def processSyncing(): Unit = {
      FastSyncMetrics.measure(syncState)
      log.debug(
        "Start of processSyncing: {}",
        Map(
          "fullySynced" -> fullySynced,
          "blockchainDataToDownload" -> blockchainDataToDownload,
          "noBlockchainWorkRemaining" -> noBlockchainWorkRemaining,
          "stateSyncFinished" -> syncState.stateSyncFinished,
          "notInTheMiddleOfUpdate" -> notInTheMiddleOfUpdate
        )
      )
      if (fullySynced) {
        finish()
      } else {
        if (blockchainDataToDownload) {
          processDownloads()
        } else if (noBlockchainWorkRemaining && !syncState.stateSyncFinished && notInTheMiddleOfUpdate) {
          if (pivotBlockIsStale()) {
            log.info("Restarting state sync to new pivot block")
            syncStateScheduler ! RestartRequested
            stateSyncRestartRequested = true
          }
        } else {
          log.info("No more items to request, waiting for {} responses", assignedHandlers.size)
        }
      }
    }

    def finish(): Unit = {
      val totalTime = totalMinutesTaken()
      FastSyncMetrics.setFastSyncTotalTimeGauge(totalTime.toDouble)
      log.info("Total time taken for FastSync was {} minutes", totalTime)
      log.info("Block synchronization in fast mode finished, switching to regular mode")

      // We have downloaded to target + fastSyncBlockValidationX, se we must discard those last blocks
      discardLastBlocks(syncState.safeDownloadTarget, syncConfig.fastSyncBlockValidationX - 1)
      cleanup()
      appStateStorage.fastSyncDone().commit()
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
          log.debug("There are no available peers, waiting for [{}] responses.", assignedHandlers.size)
        } else {
          scheduler.scheduleOnce(syncRetryInterval, self, ProcessSyncing)
        }
      } else {
        val now = Instant.now()
        val peers = unassignedPeers
          .filter(p => peerRequestsTime.get(p.peer).forall(d => d.plusMillis(fastSyncThrottle.toMillis).isBefore(now)))
        peers
          .take(maxConcurrentRequests - assignedHandlers.size)
          .sortBy(_.peerInfo.maxBlockNumber)(bigIntReverseOrdering)
          .foreach(assignBlockchainWork)
      }
    }

    def assignBlockchainWork(peerWithInfo: PeerWithInfo): Unit = {
      val PeerWithInfo(peer, peerInfo) = peerWithInfo
      log.debug(s"Assigning blockchain work for peer [{}]", peer.id.value)
      if (syncState.receiptsQueue.nonEmpty) {
        requestReceipts(peer)
      } else if (syncState.blockBodiesQueue.nonEmpty) {
        requestBlockBodies(peer)
      } else if (blockHeadersQueue.nonEmpty) {
        requestBlockHeaders(peer)
      } else if (shouldRequestNewSkeleton(peerInfo)) {
        requestSkeletonHeaders(peer)
      } else {
        log.debug(
          "Nothing to request. Waiting for responses from: {} and/or {}",
          assignedHandlers.keys,
          skeletonHandler
        )
      }
    }

    private def shouldRequestNewSkeleton(peerInfo: PeerInfo): Boolean =
      currentSkeletonState.isEmpty &&
        skeletonHandler.isEmpty &&
        syncState.bestBlockHeaderNumber < syncState.safeDownloadTarget

    private def requestReceipts(peer: Peer): Unit = {
      val (receiptsToGet, remainingReceipts) = syncState.receiptsQueue.splitAt(receiptsPerRequest)

      log.debug("Requesting [{}] receipts from peer [{}]", receiptsToGet.size, peer.id.value)

      val handler = context.actorOf(
        PeerRequestHandler.props[GetReceipts, Receipts](
          peer,
          peerResponseTimeout,
          etcPeerManager,
          peerEventBus,
          requestMsg = GetReceipts(receiptsToGet),
          responseMsgCode = Codes.ReceiptsCode
        ),
        s"$countActor-peer-request-handler-receipts"
      )

      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
      syncState = syncState.copy(receiptsQueue = remainingReceipts)
      requestedReceipts += handler -> receiptsToGet
    }

    private def requestBlockBodies(peer: Peer): Unit = {
      val (blockBodiesToGet, remainingBlockBodies) = syncState.blockBodiesQueue.splitAt(blockBodiesPerRequest)

      log.debug("Requesting [{}] block bodies from peer [{}]", blockBodiesToGet.size, peer.id.value)

      val handler = context.actorOf(
        PeerRequestHandler.props[GetBlockBodies, BlockBodies](
          peer,
          peerResponseTimeout,
          etcPeerManager,
          peerEventBus,
          requestMsg = GetBlockBodies(blockBodiesToGet),
          responseMsgCode = Codes.BlockBodiesCode
        ),
        s"$countActor-peer-request-handler-block-bodies"
      )

      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
      syncState = syncState.copy(blockBodiesQueue = remainingBlockBodies)
      requestedBlockBodies += handler -> blockBodiesToGet
    }

    private def requestBlockHeaders(peer: Peer): Unit = {
      Try(blockHeadersQueue.dequeue()) match {
        case Success(toRequest) =>
          log.debug(
            "Requesting [{}] block headers starting at block header [{}] from peer [{}]",
            toRequest.limit,
            toRequest.from,
            peer.id.value
          )

          val handler = context.actorOf(
            PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
              peer,
              peerResponseTimeout,
              etcPeerManager,
              peerEventBus,
              requestMsg = GetBlockHeaders(Left(toRequest.from), toRequest.limit, skip = 0, reverse = false),
              responseMsgCode = Codes.BlockHeadersCode
            ),
            s"$countActor-peer-request-handler-block-headers"
          )

          context watch handler
          assignedHandlers += (handler -> peer)
          requestedHeaders += (peer -> toRequest)
          peerRequestsTime += (peer -> Instant.now())
        case _ => log.warning("Tried to request more block headers but work queue was empty.")
      }

    }

    private def requestSkeletonHeaders(peerCandidate: Peer): Unit = {
      val skeleton =
        HeaderSkeleton(syncState.bestBlockHeaderNumber + 1, syncState.safeDownloadTarget, blockHeadersPerRequest)

      val masterPeerBestBlock =
        masterPeer.flatMap(mp => peersToDownloadFrom.get(mp.id).map(_.peerInfo.maxBlockNumber))

      val (peerToRequestFrom, peerBestBlock) = (masterPeer, masterPeerBestBlock) match {
        case (Some(mp), Some(bestBlock)) if bestBlock >= syncState.safeDownloadTarget =>
          (Some(mp), Some(bestBlock))
        case _ => // switch to new peer as master peer if best block is high enough
          val peerBestBlock = peersToDownloadFrom.get(peerCandidate.id).map(_.peerInfo.maxBlockNumber)
          peerBestBlock match {
            case Some(bestBlock) if bestBlock >= syncState.safeDownloadTarget =>
              masterPeer = Some(peerCandidate)
              (Some(peerCandidate), Some(bestBlock))
            case _ => (None, peerBestBlock)
          }
      }

      log.debug(
        "Attempting to request header skeleton for range [{}-{}] from master peer [{}] with best known block [{}]",
        skeleton.from,
        skeleton.lastSkeletonHeaderNumber,
        peerToRequestFrom.map(_.id.value),
        peerBestBlock
      )
      log.debug(
        "Request details: [firstSkeletonHeader={}], [limit={}], [gapSize={}] and [safeDownloadTarget={}]",
        skeleton.firstSkeletonHeaderNumber,
        skeleton.limit,
        skeleton.gapSize,
        syncState.safeDownloadTarget
      )

      peerToRequestFrom match {
        case Some(peer) =>
          val msg = GetBlockHeaders(
            Left(skeleton.firstSkeletonHeaderNumber),
            skeleton.limit,
            skeleton.gapSize,
            reverse = false
          )

          val handler = context.actorOf(
            PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
              peer,
              peerResponseTimeout,
              etcPeerManager,
              peerEventBus,
              requestMsg = msg,
              responseMsgCode = Codes.BlockHeadersCode
            ),
            s"$countActor-peer-request-handler-block-headers-skeleton"
          )

          context watch handler
          skeletonHandler = Some(handler)
          currentSkeletonState = Some(skeleton)
          peerRequestsTime += (peer -> Instant.now())
        case None =>
          log.warning(
            "Attempted to download new skeleton headers but neither master peer [{}] nor peer candidate [{}] had a high enough best block.",
            masterPeer.map(_.id),
            peerCandidate.id
          )
      }
    }

    private def unassignedPeers: List[PeerWithInfo] = {
      val assignedPeers = assignedHandlers.values.map(_.id).toList
      peersToDownloadFrom.removedAll(assignedPeers).values.toList
    }

    private def blockchainDataToDownload: Boolean =
      syncState.blockChainWorkQueued || syncState.bestBlockHeaderNumber < syncState.safeDownloadTarget

    private def fullySynced: Boolean = {
      syncState.isBlockchainWorkFinished && assignedHandlers.isEmpty && syncState.stateSyncFinished
    }

    private def updateBestBlockIfNeeded(receivedHashes: Seq[ByteString]): Unit = {
      val fullBlocks = receivedHashes.flatMap { hash =>
        for {
          header <- blockchainReader.getBlockHeaderByHash(hash)
          _ <- blockchain.getBlockBodyByHash(hash)
          _ <- blockchain.getReceiptsByHash(hash)
        } yield header
      }

      if (fullBlocks.nonEmpty) {
        val bestReceivedBlock = fullBlocks.maxBy(_.number)
        val lastStoredBestBlockNumber = appStateStorage.getBestBlockNumber()
        if (lastStoredBestBlockNumber < bestReceivedBlock.number) {
          blockchain.saveBestKnownBlocks(bestReceivedBlock.number)
          appStateStorage.putBestBlockNumber(bestReceivedBlock.number).commit()
        }
        syncState = syncState.copy(lastFullBlockNumber = bestReceivedBlock.number.max(lastStoredBestBlockNumber))
      }

    }
  }
}

object FastSync {

  // scalastyle:off parameter.number
  def props(
      fastSyncStateStorage: FastSyncStateStorage,
      appStateStorage: AppStateStorage,
      blockchain: Blockchain,
      blockchainReader: BlockchainReader,
      evmCodeStorage: EvmCodeStorage,
      nodeStorage: NodeStorage,
      validators: Validators,
      peerEventBus: ActorRef,
      etcPeerManager: ActorRef,
      blacklist: Blacklist,
      syncConfig: SyncConfig,
      scheduler: Scheduler
  ): Props =
    Props(
      new FastSync(
        fastSyncStateStorage,
        appStateStorage,
        blockchain,
        blockchainReader,
        evmCodeStorage,
        nodeStorage,
        validators,
        peerEventBus,
        etcPeerManager,
        blacklist,
        syncConfig,
        scheduler
      )
    )

  private case class UpdatePivotBlock(reason: PivotBlockUpdateReason)
  private case object ProcessSyncing
  private case object PersistSyncState
  private case object PrintStatus

  /**
    * Sync state that should be persisted.
    */
  final case class SyncState(
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

    def blockChainWorkQueued: Boolean =
      blockBodiesQueue.nonEmpty || receiptsQueue.nonEmpty

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

  sealed trait HashType {
    def v: ByteString
  }

  case class StateMptNodeHash(v: ByteString) extends HashType
  case class ContractStorageMptNodeHash(v: ByteString) extends HashType
  case class EvmCodeHash(v: ByteString) extends HashType
  case class StorageRootHash(v: ByteString) extends HashType

  case object Done

  sealed abstract class HeaderProcessingResult
  case object HeadersProcessingFinished extends HeaderProcessingResult
  case class ParentChainWeightNotFound(header: BlockHeader) extends HeaderProcessingResult
  case class ValidationFailed(header: BlockHeader, peer: Peer) extends HeaderProcessingResult
  case object ImportedPivotBlock extends HeaderProcessingResult

  sealed abstract class PivotBlockUpdateReason {
    def isSyncRestart: Boolean = this match {
      case ImportedLastBlock => false
      case LastBlockValidationFailed => false
      case SyncRestart => true
    }
  }
  case object ImportedLastBlock extends PivotBlockUpdateReason
  case object LastBlockValidationFailed extends PivotBlockUpdateReason
  case object SyncRestart extends PivotBlockUpdateReason

  private[fast] final case class HeaderRange(from: BigInt, limit: BigInt)
}
