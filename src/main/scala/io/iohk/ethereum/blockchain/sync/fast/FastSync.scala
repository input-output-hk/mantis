package io.iohk.ethereum.blockchain.sync.fast

import java.time.Instant

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync._
import io.iohk.ethereum.blockchain.sync.fast.FastSync._
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.db.storage.{ AppStateStorage, FastSyncStateStorage }
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{ FiniteDuration, _ }

class FastSync(
  val fastSyncStateStorage: FastSyncStateStorage,
  val appStateStorage: AppStateStorage,
  val blockchain: Blockchain,
  val validators: Validators,
  val peerEventBus: ActorRef,
  val etcPeerManager: ActorRef,
  val syncConfig: SyncConfig,
  implicit val scheduler: Scheduler
) extends Actor
  with ActorLogging
  with PeerListSupport
  with BlacklistSupport
  with FastSyncNodeHandler
  with FastSyncBlockHeadersHandler
  with FastSyncBlockBodiesHandler
  with FastSyncReceiptsHandler {

  import syncConfig._

  val syncController: ActorRef = context.parent

  private[sync] val TargetBlockSelectorName = "target-block-selector"
  private[sync] val BlockHeadersHandlerName = "block-headers-request-handler"
  private[sync] val StateStorageName = "state-storage"

  override def receive: Receive = idle

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  def idle: Receive = handleCommonMessages orElse {
    case Start =>
      log.info("Trying to start block synchronization (fast mode)")
      fastSyncStateStorage.getSyncState().fold(startFromScratch())(startWithState)
  }

  def startWithState(syncState: FastSyncState): Unit = {
    val syncingHandler = new SyncingHandler
    val handlerState = FastSyncHandlerState(syncState)

    if (syncState.updatingTargetBlock) {
      log.info("FastSync interrupted during targetBlock update, choosing new target block")
      callTargetBlockSelector()
      context become syncingHandler.waitingForTargetBlockUpdate(ImportedLastBlock, handlerState)
    } else {
      log.info("Starting block synchronization (fast mode), target block {}, block to download to {}",
        syncState.targetBlock.number, syncState.safeDownloadTarget)
      context become syncingHandler.receive(handlerState)
      syncingHandler.processSyncing(handlerState)
    }
  }

  private def callTargetBlockSelector(): Unit = {
    val targetBlockSelector = context.actorOf(
      FastSyncTargetBlockSelector.props(etcPeerManager, peerEventBus, syncConfig, scheduler),
      TargetBlockSelectorName
    )
    targetBlockSelector ! FastSyncTargetBlockSelector.ChooseTargetBlock
  }

  def startFromScratch(): Unit = {
    callTargetBlockSelector()
    context become waitingForTargetBlock
  }

  def waitingForTargetBlock: Receive = handleCommonMessages orElse {
    case FastSyncTargetBlockSelector.Result(targetBlockHeader) =>
      if (targetBlockHeader.number < 1) {
        log.info("Unable to start block synchronization in fast mode: target block is less than 1")
        doneFastSync()
      } else {
        val initialSyncState =
          FastSyncState(targetBlock = targetBlockHeader, safeDownloadTarget = targetBlockHeader.number + fastSyncBlockValidationX)
        startWithState(initialSyncState)
      }
  }

  private def doneFastSync(): Unit = {
    appStateStorage.fastSyncDone()
    context become idle
    syncController ! Done
  }

  // scalastyle:off number.of.methods
  private class SyncingHandler {

    private val syncStateStorageActor = context.actorOf(Props[FastSyncStateStorageActor], StateStorageName)
    syncStateStorageActor ! fastSyncStateStorage

    // Delay before starting to persist snapshot. It should be 0, as the presence of it marks that fast sync was started
    private val persistStateSnapshotDelay: FiniteDuration = 0.seconds
    private val syncStatePersistCancellable =
      scheduler.schedule(persistStateSnapshotDelay, persistStateSnapshotInterval, self, PersistSyncState)
    private val printStatusCancellable = scheduler.schedule(printStatusInterval, printStatusInterval, self, PrintStatus)
    private val heartBeat = scheduler.schedule(syncRetryInterval, syncRetryInterval * 2, self, ProcessSyncing)

    def receive(handlerState: FastSyncHandlerState): Receive =
      handleCommonMessages orElse
        handleTargetBlockUpdate(handlerState) orElse
        handleSyncing(handlerState) orElse
        handleReceivedResponses(handlerState) orElse {
        case Terminated(ref) if handlerState.assignedHandlers.contains(ref) =>
          handleRequestFailure(handlerState.assignedHandlers(ref), ref, "unexpected error", handlerState)
      }

    def handleSyncing(handlerState: FastSyncHandlerState): Receive = {
      case ProcessSyncing => processSyncing(handlerState)
      case PrintStatus => printStatus(handlerState)
      case PersistSyncState =>
        val persistedHandlerState = handlerState.persistSyncState()
        context become receive(persistedHandlerState)
        syncStateStorageActor ! persistedHandlerState.syncState
    }

    // scalastyle:off method.length
    def handleReceivedResponses(handlerState: FastSyncHandlerState): Receive = {
      case PeerRequestHandler.ResponseReceived(peer, BlockHeaders(blockHeaders), timeTaken) =>
        log.info("*** Received {} block headers in {} ms ***", blockHeaders.size, timeTaken)
        val headers = handlerState.requestedHeaders
        headers.get(peer).foreach{requestedNum =>
          context unwatch sender()
          val newHandlerState = handlerState.withRequestedHeaders(headers - peer).removeHandler(sender())

          if (blockHeaders.nonEmpty && blockHeaders.size <= requestedNum && blockHeaders.head.number == handlerState.nextBestBlockNumber) {
            val (finalHandlerState, msg) = handleBlockHeaders(peer, blockHeaders, newHandlerState, discardLastBlocks, blacklist)
            context become receive(finalHandlerState)
            msg match {
              case ProcessSyncing => processSyncing(finalHandlerState)
              case UpdateTargetBlock(withState) => updateTargetBlock(withState, finalHandlerState)
            }
          } else {
            blacklist(peer.id, blacklistDuration, "wrong blockHeaders response (empty or not chain forming)")
            context become receive(newHandlerState)
          }
        }

      case PeerRequestHandler.ResponseReceived(peer, BlockBodies(blockBodies), timeTaken) =>
        log.info("Received {} block bodies in {} ms", blockBodies.size, timeTaken)
        val bodies = handlerState.requestedBlockBodies
        context unwatch sender()
        val newHandlerState = handlerState.withRequestedBlockBodies(bodies - sender()).removeHandler(sender())
        val bodiesToQueue =
          handleBlockBodies(peer, bodies.getOrElse(sender(), Nil), blockBodies, blacklist, updateBestBlockIfNeeded)
        val finalHandlerState = bodiesToQueue
          .fold(newHandlerState.reduceQueuesAndBestBlock(blockHeadersPerRequest))(newHandlerState.withEnqueueBlockBodies)
        context become receive(finalHandlerState)
        processSyncing(finalHandlerState)

      case PeerRequestHandler.ResponseReceived(peer, Receipts(receipts), timeTaken) =>
        log.info("Received {} receipts in {} ms", receipts.size, timeTaken)
        val baseReceipts = handlerState.requestedReceipts
        context unwatch sender()
        val newHandlerState = handlerState.withRequestedReceipts(baseReceipts - sender()).removeHandler(sender())
        val receiptsToQueue =
          handleReceipts(peer, baseReceipts.getOrElse(sender(), Nil), receipts, blacklist, updateBestBlockIfNeeded)
        val finalHandlerState = receiptsToQueue
          .fold(newHandlerState.reduceQueuesAndBestBlock(blockHeadersPerRequest))(newHandlerState.withEnqueueReceipts)
        context become receive(finalHandlerState)
        processSyncing(finalHandlerState)

      case PeerRequestHandler.ResponseReceived(peer, nodeData: NodeData, timeTaken) =>
        log.info("Received {} state nodes in {} ms", nodeData.values.size, timeTaken)
        context unwatch sender()
        val (pending, downloaded, total) = handleNodeData(
          peer,
          handlerState.getRequestedNodes(sender()),
          nodeData,
          handlerState.syncState.downloadedNodesCount,
          handlerState.syncState.targetBlock.number,
          blacklist
        )
        val finalHandlerState =
          handlerState.removeFromNodes(sender()).removeHandler(sender()).withNodeData(pending, downloaded, total)
        context become receive(finalHandlerState)
        processSyncing(finalHandlerState)

      case PeerRequestHandler.RequestFailed(peer, reason) =>
        handleRequestFailure(peer, sender(), reason, handlerState)

    }

    def handleTargetBlockUpdate(handlerState: FastSyncHandlerState): Receive = {
      case UpdateTargetBlock(state) => updateTargetBlock(state, handlerState)
    }

    private def updateBestBlockIfNeeded(receivedHashes: Seq[ByteString]): Unit = {
      val headers = for {
        hash <- receivedHashes
        header <- blockchain.getBlockHeaderByHash(hash)
        _ <- blockchain.getBlockBodyByHash(hash)
        _ <- blockchain.getReceiptsByHash(hash)
      } yield header

      if (headers.nonEmpty) {
        val bestReceivedBlock = headers.maxBy(_.number).number
        if (appStateStorage.getBestBlockNumber() < bestReceivedBlock) appStateStorage.putBestBlockNumber(bestReceivedBlock)
      }
    }

    def waitingForTargetBlockUpdate(processState: FinalBlockProcessingResult, handlerState: FastSyncHandlerState): Receive =
      handleCommonMessages orElse
        handleTargetBlockUpdate(handlerState) orElse {
        case FastSyncTargetBlockSelector.Result(targetBlockHeader) =>
          handleNewTargetBlock(processState, handlerState, targetBlockHeader)

        case PersistSyncState =>
          val persistedHandlerState = handlerState.persistSyncState()
          context become waitingForTargetBlockUpdate(processState, persistedHandlerState)
          syncStateStorageActor ! persistedHandlerState.syncState
      }

    private def handleNewTargetBlock(
      processState: FinalBlockProcessingResult,
      handlerState: FastSyncHandlerState,
      targetBlockHeader: BlockHeader
    ): Unit = {
      log.info("Received new target block with number: {}", targetBlockHeader.number)
      if (targetBlockHeader.number >= handlerState.syncState.targetBlock.number) {
        val (newHandlerState, msg) = handlerState
          .withUpdatingTargetBlock(false)
          .updateTargetSyncState(processState, targetBlockHeader, syncConfig)

        log.info(msg)
        context become receive(newHandlerState)
        processSyncing(newHandlerState)
      } else {
        context become waitingForTargetBlockUpdate(processState, handlerState.increaseUpdateFailures())
        scheduler.scheduleOnce(syncRetryInterval, self, UpdateTargetBlock(processState))
      }
    }

    private def updateTargetBlock(processState: FinalBlockProcessingResult, handlerState: FastSyncHandlerState): Unit = {
      if (handlerState.syncState.targetBlockUpdateFailures <= maximumTargetUpdateFailures) {
        val newHandlerState = handlerState.withUpdatingTargetBlock(true)
        if (handlerState.assignedHandlers.nonEmpty) {
          log.info("Still waiting for some responses, rescheduling target block update")
          scheduler.scheduleOnce(syncRetryInterval, self, UpdateTargetBlock(processState))
          context become receive(newHandlerState)
        } else {
          log.info("Asking for new target block")
          callTargetBlockSelector()
          context become waitingForTargetBlockUpdate(processState, newHandlerState)
        }
      } else {
        log.warning("Sync failure! Number of targetBlock updates failures reached maximum ({})", maximumTargetUpdateFailures)
        sys.exit(1)
      }
    }

    private def discardLastBlocks(startBlock: BigInt, blocksToDiscard: Int): AppStateStorage = {
      val targetBlock = (startBlock - blocksToDiscard) max 1
      (startBlock to targetBlock by -1)
        .flatMap(blockchain.getBlockHeaderByNumber)
        .foreach(headerToRemove => blockchain.removeBlock(headerToRemove.hash, withState = false))

      appStateStorage.putBestBlockNumber((startBlock - blocksToDiscard - 1) max 0)
    }

    private def handleRequestFailure(peer: Peer, handler: ActorRef, reason: String, handlerState: FastSyncHandlerState): Unit = {
      context unwatch handler

      if (handshakedPeers.contains(peer)) blacklist(peer.id, blacklistDuration, reason)

      context become receive(handlerState.withPeerAndHandlerRemoved(peer, handler))
    }

    private def printStatus(handlerState: FastSyncHandlerState): Unit = {
      val formatPeer: Peer => String = peer => s"${ peer.remoteAddress.getAddress.getHostAddress }:${ peer.remoteAddress.getPort }"
      val handlers = handlerState.assignedHandlers
      val state = handlerState.syncState

      log.info(
        s"""|Block: ${ appStateStorage.getBestBlockNumber() }/${ state.targetBlock.number }.
            |Peers waiting_for_response/connected: ${ handlers.size }/${ handshakedPeers.size } (${ blacklistedPeers.size } blacklisted).
            |State: ${ state.downloadedNodesCount }/${ state.totalNodesCount } nodes.
            |""".stripMargin.replace("\n", " ")
      )

      lazy val connected = handlers.values.map(formatPeer).toSeq.sorted.mkString(", ")
      lazy val handshaked = handshakedPeers.keys.map(formatPeer).toSeq.sorted.mkString(", ")
      lazy val blacklisted = blacklistedPeers.keys.map(_.value).mkString(", ")
      log.debug("Connection status: connected[{}], handshaked[{}], blacklisted [{}]", connected, handshaked, blacklisted)
    }

    private[fast] def processSyncing(handlerState: FastSyncHandlerState): Unit = {
      if (handlerState.isFullySynced) {
        finish(handlerState)
      } else {
        if (handlerState.syncState.shouldDownloadMoreItems) {
          processDownloads(handlerState)
        } else {
          log.info("No more items to request, waiting for {} responses", handlerState.assignedHandlers.size)
          context become receive(handlerState)
        }
      }
    }

    private def finish(handlerState: FastSyncHandlerState): Unit = {
      log.info("Block synchronization in fast mode finished, switching to regular mode")
      // We have downloaded to target + fastSyncBlockValidationX, so we must discard those last blocks
      discardLastBlocks(handlerState.syncState.safeDownloadTarget, fastSyncBlockValidationX - 1)
      cleanup()
      doneFastSync()
    }

    private def cleanup(): Unit = {
      heartBeat.cancel()
      syncStatePersistCancellable.cancel()
      printStatusCancellable.cancel()
      syncStateStorageActor ! PoisonPill
      fastSyncStateStorage.purge()
    }

    private def processDownloads(handlerState: FastSyncHandlerState): Unit = {
      val handlers = handlerState.assignedHandlers
      val peers = peersToDownloadFrom.keySet diff handlers.values.toSet
      if (peers.isEmpty) {
        if (handlers.nonEmpty) {
          log.debug("There are no available peers, waiting for responses")
        } else {
          log.debug("There are no peers to download from, scheduling a retry in {}", syncRetryInterval)
          scheduler.scheduleOnce(syncRetryInterval, self, ProcessSyncing)
        }
      } else {

        def isPeerRequestTimeConsistentWithFastSyncThrottle(peer: Peer): Boolean = {
          handlerState.peerRequestsTime.get(peer).forall(t => t.plusMillis(fastSyncThrottle.toMillis).isBefore(Instant.now()))
        }

        peers
          .filter(isPeerRequestTimeConsistentWithFastSyncThrottle)
          .take(maxConcurrentRequests - handlers.size)
          .toSeq
          .sortBy(_.ref.toString)
          .foreach(peer => assignWork(peer, handlerState))
      }
    }

    private def assignWork(peer: Peer, handlerState: FastSyncHandlerState): Unit = {
      if (handlerState.syncState.shouldAssignWork) {
        assignBlockchainWork(peer, handlerState)
      } else {
        val pendingNodes = handlerState.getPendingNodes(nodesPerRequest)
        val (mptToGet, nonMptToGet) = pendingNodes.toGet
        val nodesToGet = (nonMptToGet ++ mptToGet).map(_.v).distinct

        val handler = requestNodes(peer, nodesToGet)
        val newHandlerState = handlerState.withNodes(handler, nodesPerRequest, pendingNodes).withHandlerAndPeer(handler, peer)
        context become receive(newHandlerState)
      }
    }

    private def assignBlockchainWork(peer: Peer, handlerState: FastSyncHandlerState): Unit = {
      if (handlerState.syncState.receiptsQueue.nonEmpty) {
        val (receiptsToGet, remainingReceipts) = handlerState.syncState.receiptsQueue.splitAt(receiptsPerRequest)
        val handler = requestReceipts(peer, receiptsToGet)
        val newHandlerState =
          handlerState.withReceipts(handler, remainingReceipts, receiptsToGet).withHandlerAndPeer(handler, peer)
        context become receive(newHandlerState)
      } else if (handlerState.syncState.blockBodiesQueue.nonEmpty) {
        val (bodiesToGet, remainingBodies) = handlerState.syncState.blockBodiesQueue.splitAt(blockBodiesPerRequest)
        val handler = requestBlockBodies(peer, bodiesToGet)
        val newHandlerState =
          handlerState.withBlockBodies(handler, remainingBodies, bodiesToGet).withHandlerAndPeer(handler, peer)
        context become receive(newHandlerState)
      } else if (handlerState.shouldRequestBlockHeaders && context.child(BlockHeadersHandlerName).isEmpty) {
        val bestBlockOffset = handlerState.syncState.safeDownloadTarget - handlerState.syncState.bestBlockHeaderNumber
        val limit: BigInt = if (blockHeadersPerRequest < bestBlockOffset) blockHeadersPerRequest else bestBlockOffset
        val handler = requestBlockHeaders(peer, handlerState.nextBestBlockNumber, limit)
        val newHandlerState =
          handlerState.withRequestedHeaders(handlerState.requestedHeaders + (peer -> limit)).withHandlerAndPeer(handler, peer)
        context become receive(newHandlerState)
      }
    }

    private def requestReceipts(peer: Peer, receiptsToGet: Seq[ByteString]): ActorRef = {
      val handler = context.actorOf(PeerRequestHandler.props[GetReceipts, Receipts](
        peer = peer,
        responseTimeout = peerResponseTimeout,
        etcPeerManager = etcPeerManager,
        peerEventBus = peerEventBus,
        requestMsg = GetReceipts(receiptsToGet),
        responseMsgCode = Receipts.code
      ))

      context watch handler
      handler
    }

    private def requestBlockBodies(peer: Peer, bodiesToGet: Seq[ByteString]): ActorRef = {
      val handler = context.actorOf(PeerRequestHandler.props[GetBlockBodies, BlockBodies](
        peer = peer,
        responseTimeout = peerResponseTimeout,
        etcPeerManager = etcPeerManager,
        peerEventBus = peerEventBus,
        requestMsg = GetBlockBodies(bodiesToGet),
        responseMsgCode = BlockBodies.code
      ))

      context watch handler
      handler
    }

    private def requestBlockHeaders(peer: Peer, headerToGet: BigInt, limit: BigInt): ActorRef = {
      val handler = context.actorOf(
        PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
          peer = peer,
          responseTimeout = peerResponseTimeout,
          etcPeerManager = etcPeerManager,
          peerEventBus = peerEventBus,
          requestMsg = GetBlockHeaders(Left(headerToGet), limit, skip = 0, reverse = false),
          responseMsgCode = BlockHeaders.code
        ),
        BlockHeadersHandlerName
      )

      context watch handler
      handler
    }

    private def requestNodes(peer: Peer, nodesToGet: Seq[ByteString]): ActorRef = {
      val handler = context.actorOf(PeerRequestHandler.props[GetNodeData, NodeData](
        peer = peer,
        responseTimeout = peerResponseTimeout,
        etcPeerManager = etcPeerManager,
        peerEventBus = peerEventBus,
        requestMsg = GetNodeData(nodesToGet),
        responseMsgCode = NodeData.code
      ))

      context watch handler
      handler
    }
  }

}

object FastSync {
  def props(
    fastSyncStateStorage: FastSyncStateStorage,
    appStateStorage: AppStateStorage,
    blockchain: Blockchain,
    validators: Validators,
    peerEventBus: ActorRef,
    etcPeerManager: ActorRef,
    syncConfig: SyncConfig,
    scheduler: Scheduler
  ): Props = Props(new FastSync(fastSyncStateStorage, appStateStorage, blockchain, validators, peerEventBus, etcPeerManager, syncConfig, scheduler))

  sealed trait FastSyncMsg

  private[sync] case class UpdateTargetBlock(state: FinalBlockProcessingResult) extends FastSyncMsg
  private[sync] case object ProcessSyncing extends FastSyncMsg
  private[sync] case object PersistSyncState
  private case object PrintStatus

  sealed trait HashType {
    def v: ByteString
  }

  case class StateMptNodeHash(v: ByteString) extends HashType
  case class ContractStorageMptNodeHash(v: ByteString) extends HashType
  case class EvmCodeHash(v: ByteString) extends HashType
  case class StorageRootHash(v: ByteString) extends HashType

  case object Start
  case object Done

  sealed trait FinalBlockProcessingResult
  case object ImportedLastBlock extends FinalBlockProcessingResult
  case object LastBlockValidationFailed extends FinalBlockProcessingResult

}
