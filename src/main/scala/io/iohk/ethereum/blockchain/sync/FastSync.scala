package io.iohk.ethereum.blockchain.sync

import java.time.Instant

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSyncReceiptsValidator.ReceiptsValidationResult
import io.iohk.ethereum.blockchain.sync.SyncBlocksValidator.BlockBodyValidationResult
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.storage.{ AppStateStorage, FastSyncStateStorage }
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.{ BranchNode, ExtensionNode, HashNode, LeafNode, MptNode }
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.MptNodeEncoders._
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.utils.Config.SyncConfig
import org.bouncycastle.util.encoders.Hex

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{ FiniteDuration, _ }
import scala.util.{ Failure, Random, Success, Try }

// scalastyle:off file.size.limit
class FastSync(
  val fastSyncStateStorage: FastSyncStateStorage,
  val appStateStorage: AppStateStorage,
  val blockchain: Blockchain,
  val validators: Validators,
  val peerEventBus: ActorRef,
  val etcPeerManager: ActorRef,
  val syncConfig: SyncConfig,
  implicit val scheduler: Scheduler
) extends Actor with ActorLogging with PeerListSupport with BlacklistSupport with FastSyncReceiptsValidator with SyncBlocksValidator {

  import FastSync._
  import syncConfig._

  val syncController: ActorRef = context.parent

  private[sync] val TargetBlockSelectorName = "target-block-selector"
  private[sync] val BlockHeadersHandlerName = "block-headers-request-handler"
  private[sync] val StateStorageName        = "state-storage"

  override def receive: Receive = idle

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  def idle: Receive = handleCommonMessages orElse {
    case Start =>
      log.info("Trying to start block synchronization (fast mode)")
      fastSyncStateStorage.getSyncState() match {
        case Some(syncState) => startWithState(syncState)
        case None            => startFromScratch()
      }
  }

  def startWithState(syncState: SyncState): Unit = {
    val syncingHandler = new SyncingHandler(syncState)
    val handlerState = HandlerState(syncState)

    if (syncState.updatingTargetBlock) {
      log.info("FastSync interrupted during targetBlock update, choosing new target block")
      context become syncingHandler.waitingForTargetBlockUpdate(ImportedLastBlock, handlerState)
      callTargetBlockSelector()
    } else {
      log.info(s"Starting block synchronization (fast mode), target block ${syncState.targetBlock.number}, " +
        s"block to download to ${syncState.safeDownloadTarget}")
      context become syncingHandler.receive(handlerState)
      self ! ProcessSyncing
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
    context become waitingForTargetBlock
    callTargetBlockSelector()
  }

  def waitingForTargetBlock: Receive = handleCommonMessages orElse {
    case FastSyncTargetBlockSelector.Result(targetBlockHeader) =>
      if (targetBlockHeader.number < 1) {
        log.info("Unable to start block synchronization in fast mode: target block is less than 1")
        doneFastSync()
      } else {
        val initialSyncState =
          SyncState(targetBlock = targetBlockHeader, safeDownloadTarget = targetBlockHeader.number + syncConfig.fastSyncBlockValidationX)
        startWithState(initialSyncState)
      }
  }

  private def doneFastSync(): Unit = {
    appStateStorage.fastSyncDone()
    context become idle
    syncController ! Done
  }

  // scalastyle:off number.of.methods
  private[sync] class SyncingHandler(initialSyncState: SyncState) {

    private val syncStateStorageActor = context.actorOf(Props[FastSyncStateStorageActor], StateStorageName)
    syncStateStorageActor ! fastSyncStateStorage

    // Delay before starting to persist snapshot. It should be 0, as the presence of it marks that fast sync was started
    private val persistStateSnapshotDelay: FiniteDuration = 0.seconds
    private val syncStatePersistCancellable =
      scheduler.schedule(persistStateSnapshotDelay, persistStateSnapshotInterval, self, PersistSyncState)
    private val printStatusCancellable = scheduler.schedule(printStatusInterval, printStatusInterval, self, PrintStatus)
    private val heartBeat = scheduler.schedule(syncRetryInterval, syncRetryInterval * 2, self, ProcessSyncing)

    def receive(handlerState: HandlerState): Receive =
      handleCommonMessages orElse
      handleSyncing(handlerState) orElse
      handleReceivedResponses(handlerState) orElse
      handleWorkAssignment(handlerState) orElse
      handleTargetBlockUpdate(handlerState) orElse {
        case Terminated(ref) if handlerState.assignedHandlers.contains(ref) =>
          handleRequestFailure(handlerState.assignedHandlers(ref), ref, "unexpected error", handlerState)
      }

    def handleSyncing(handlerState: HandlerState): Receive =
      handlePersistSyncState(handlerState) orElse {
        case ProcessSyncing =>
          processSyncing(handlerState)

        case PrintStatus =>
          printStatus(handlerState)
      }

    def handleReceivedResponses(handlerState: HandlerState): Receive = {
      case PeerRequestHandler.ResponseReceived(peer, BlockHeaders(blockHeaders), timeTaken) =>
        log.info("*** Received {} block headers in {} ms ***", blockHeaders.size, timeTaken)
        val requestSender = sender()
        val headers = handlerState.requestedHeaders
        headers.get(peer).foreach { requestedNum =>
          context unwatch requestSender
          val newHandlerState = handlerState.withRequestedHeaders(headers - peer).removeHandler(requestSender)

          if (blockHeaders.nonEmpty && blockHeaders.size <= requestedNum && blockHeaders.head.number == handlerState.nextBestBlockNumber) {
            val (state, msg) = handleBlockHeaders(peer, blockHeaders, newHandlerState)
            context become receive(state)
            self ! msg
          } else {
            blacklist(peer.id, blacklistDuration, "wrong blockHeaders response (empty or not chain forming)")
            context become receive(newHandlerState)
          }
        }

      case PeerRequestHandler.ResponseReceived(peer, BlockBodies(blockBodies), timeTaken) =>
        log.info("Received {} block bodies in {} ms", blockBodies.size, timeTaken)
        val bodies = handlerState.requestedBlockBodies
        context unwatch sender()
        val newState = handlerState.withRequestedBlockBodies(bodies - sender()).removeHandler(sender())
        val finalState = handleBlockBodies(peer, bodies.getOrElse(sender(), Nil), blockBodies, newState)
        context become receive(finalState)
        self ! ProcessSyncing

      case PeerRequestHandler.ResponseReceived(peer, Receipts(receipts), timeTaken) =>
        log.info("Received {} receipts in {} ms", receipts.size, timeTaken)
        val baseReceipts = handlerState.requestedReceipts
        context unwatch sender()
        val newState = handlerState.withRequestedReceipts(baseReceipts - sender()).removeHandler(sender())
        val finalState = handleReceipts(peer, baseReceipts.getOrElse(sender(), Nil), receipts, newState)
        context become receive(finalState)
        self ! ProcessSyncing

      case PeerRequestHandler.ResponseReceived(peer, nodeData: NodeData, timeTaken) =>
        log.info("Received {} state nodes in {} ms", nodeData.values.size, timeTaken)
        context unwatch sender()
        lazy val newState = handlerState.removeNodes(sender()).removeHandler(sender())
        val finalState = handleNodeData(peer, handlerState.getRequestedNodes(sender()), nodeData, newState)
        context become receive(finalState)
        self ! ProcessSyncing

      case PeerRequestHandler.RequestFailed(peer, reason) =>
        handleRequestFailure(peer, sender(), reason, handlerState)

    }

    def waitingForTargetBlockUpdate(processState: FinalBlockProcessingResult, handlerState: HandlerState): Receive =
      handleCommonMessages orElse
      handleTargetBlockUpdate(handlerState) orElse
      handlePersistSyncState(handlerState) orElse {
        case FastSyncTargetBlockSelector.Result(targetBlockHeader) =>
          handleNewTargetBlock(processState, handlerState, targetBlockHeader)
      }

    def handleTargetBlockUpdate(handlerState: HandlerState): Receive = {
      case UpdateTargetBlock(state) => updateTargetBlock(state, handlerState)
    }

    def handlePersistSyncState(handlerState: HandlerState): Receive = {
      case PersistSyncState => persistSyncState(handlerState)
    }

    def handleWorkAssignment(handlerState: HandlerState): Receive = {
      case AssignWorkToPeer(peer) =>
        val newHandlerState = assignWork(peer, handlerState)
        context become receive(newHandlerState)
    }

    private def handleNewTargetBlock(state: FinalBlockProcessingResult, handlerState: HandlerState, targetBlockHeader: BlockHeader): Unit = {
      log.info(s"New target block with number ${targetBlockHeader.number} received")
      if (targetBlockHeader.number >= handlerState.syncState.targetBlock.number) {
        val (newHandlerState, msg) = handlerState
          .withUpdatingTargetBlock(false)
          .updateTargetSyncState(state, targetBlockHeader, syncConfig)

        log.info(msg)
        context become receive(newHandlerState)
        self ! ProcessSyncing
      } else {
        context become receive(handlerState.increaseUpdateFailures())
        scheduler.scheduleOnce(syncRetryInterval, self, UpdateTargetBlock(state))
      }
    }

    private def updateTargetBlock(state: FinalBlockProcessingResult, handlerState: HandlerState): Unit = {
      val failuresLimit = syncConfig.maximumTargetUpdateFailures
      if (handlerState.updateFailuresNotReachedTheLimit(failuresLimit)) {
      val newState = handlerState.withUpdatingTargetBlock(true)
        if (!handlerState.noAssignedHandlers) {
          log.info("Still waiting for some responses, rescheduling target block update")
          scheduler.scheduleOnce(syncRetryInterval, self, UpdateTargetBlock(state))
          context become receive(newState)
        } else {
          log.info("Asking for new target block")
          val targetBlockSelector =
            context.actorOf(FastSyncTargetBlockSelector.props(etcPeerManager, peerEventBus, syncConfig, scheduler))
          targetBlockSelector ! FastSyncTargetBlockSelector.ChooseTargetBlock
          context become waitingForTargetBlockUpdate(state, newState)
        }
      } else {
        log.warning("Sync failure! Number of targetBlock updates failures reached maximum ({})", failuresLimit)
        sys.exit(1)
      }
    }

    private def discardLastBlocks(startBlock: BigInt, blocksToDiscard: Int): AppStateStorage = {
      (startBlock to ((startBlock - blocksToDiscard) max 1) by -1).foreach { n =>
        blockchain.getBlockHeaderByNumber(n).foreach { headerToRemove =>
          blockchain.removeBlock(headerToRemove.hash, withState = false)
        }
      }
      appStateStorage.putBestBlockNumber((startBlock - blocksToDiscard - 1) max 0)
    }

    @tailrec
    private def processHeaders(peer: Peer, headers: Seq[BlockHeader], handlerState: HandlerState): (HandlerState, HeaderProcessingResult) = {
      if (headers.nonEmpty) {
        val header = headers.head
        processHeader(header, peer, handlerState) match {
          case Left(result) =>
            (handlerState, result)

          case Right((validHeader: BlockHeader, shouldUpdate: Boolean, parentDifficulty: BigInt)) =>
            blockchain.save(header)
            blockchain.save(header.hash, parentDifficulty + header.difficulty)

            val newHandlerState =
              handlerState.updateBestBlockNumber(validHeader, parentDifficulty, shouldUpdate, syncConfig)

            if (header.number == newHandlerState.syncState.safeDownloadTarget){
              (newHandlerState, ImportedTargetBlock)
            } else {
              processHeaders(peer, headers.tail, newHandlerState)
            }
        }
      } else {
        (handlerState, HeadersProcessingFinished)
      }
    }

    private def processHeader(
      header: BlockHeader,
      peer: Peer,
      handlerState: HandlerState
    ): Either[HeaderProcessingResult, (BlockHeader, Boolean, BigInt)] = for {
      validationResult <- validateHeader(header, peer, handlerState)
      (validatedHeader, shouldUpdate) = validationResult
      parentDifficulty <- getParentDifficulty(header)
    } yield (validatedHeader, shouldUpdate, parentDifficulty)

    private def validateHeader(header: BlockHeader, peer: Peer, handlerState: HandlerState): Either[HeaderProcessingResult, (BlockHeader, Boolean)] = {
      val shouldValidate = header.number >= handlerState.syncState.nextBlockToFullyValidate

      if (shouldValidate) {
        validators.blockHeaderValidator.validate(header, blockchain.getBlockHeaderByHash) match {
          case Right(_) =>
            Right((header, true))

          case Left(error) =>
            log.warning(s"Block header validation failed during fast sync at block ${header.number}: $error")
            Left(ValidationFailed(header, peer))
        }
      } else {
        Right((header, false))
      }
    }

    private def getParentDifficulty(header: BlockHeader): Either[ParentDifficultyNotFound, BigInt] = {
      blockchain.getTotalDifficultyByHash(header.parentHash).toRight(ParentDifficultyNotFound(header))
    }

    private def handleBlockValidationError(header: BlockHeader, peer: Peer, N: Int, handlerState: HandlerState): (HandlerState, FastSyncMsg) = {
      blacklist(peer.id, blacklistDuration, "block header validation failed")
      val currentSyncState = handlerState.syncState
      val headerNumber = header.number
      if (headerNumber <= currentSyncState.safeDownloadTarget) {
        discardLastBlocks(headerNumber, N)
        val newHandlerState = handlerState.withSyncState(currentSyncState.updateDiscardedBlocks(header, N))

        if (headerNumber >= currentSyncState.targetBlock.number) {
          (newHandlerState, UpdateTargetBlock(LastBlockValidationFailed))
        } else {
          (newHandlerState, ProcessSyncing)
        }
      } else {
        (handlerState, ProcessSyncing)
      }
    }

    private def handleBlockHeaders(peer: Peer, headers: Seq[BlockHeader], handlerState: HandlerState): (HandlerState, FastSyncMsg) = {
      if (checkHeadersChain(headers)) {
        processHeaders(peer, headers, handlerState) match {
          case (newHandlerState, ParentDifficultyNotFound(header)) =>
            log.debug("Parent difficulty not found for block {}, not processing rest of headers", header.number)
            (newHandlerState, ProcessSyncing)

          case (newHandlerState, HeadersProcessingFinished) =>
            (newHandlerState, ProcessSyncing)

          case (newHandlerState, ImportedTargetBlock) =>
            (newHandlerState, UpdateTargetBlock(ImportedLastBlock))

          case (newHandlerState, ValidationFailed(header, peerToBlackList)) =>
            handleBlockValidationError(header, peerToBlackList, syncConfig.fastSyncBlockValidationN, newHandlerState)
        }
      } else {
        blacklist(peer.id, blacklistDuration, "error in block headers response")
        (handlerState, ProcessSyncing)
      }
    }

    private def handleBlockBodies(peer: Peer, requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody], handlerState: HandlerState): HandlerState = {
      if (blockBodies.isEmpty) {
        val reason = s"got empty block bodies response for known hashes: ${hashes2strings(requestedHashes)}"
        blacklist(peer.id, blacklistDuration, reason)
        handlerState.withEnqueueBlockBodies(requestedHashes)
      } else {
        validateBlocks(requestedHashes, blockBodies) match {
          case BlockBodyValidationResult.Valid   =>
            insertBlocks(requestedHashes, blockBodies, handlerState)

          case BlockBodyValidationResult.Invalid =>
            val reason = s"responded with block bodies not matching block headers, blacklisting for $blacklistDuration"
            blacklist(peer.id, blacklistDuration, reason)
            handlerState.withEnqueueBlockBodies(requestedHashes)

          case BlockBodyValidationResult.DbError =>
            redownloadBlockchain(handlerState)
        }
      }
    }

    private def insertBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody], handlerState: HandlerState): HandlerState = {
      (requestedHashes zip blockBodies).foreach { case (hash, body) => blockchain.save(hash, body) }

      val (toUpdate, remaining) = requestedHashes.splitAt(blockBodies.size)
      updateBestBlockIfNeeded(toUpdate)
      if (remaining.nonEmpty) {
        handlerState.withEnqueueBlockBodies(remaining)
      } else {
        handlerState
      }
    }

    private def updateBestBlockIfNeeded(receivedHashes: Seq[ByteString]): Unit = {
      val fullBlocks = receivedHashes.flatMap { hash =>
        for {
          header <- blockchain.getBlockHeaderByHash(hash)
          _ <- blockchain.getBlockBodyByHash(hash)
          _ <- blockchain.getReceiptsByHash(hash)
        } yield header
      }

      if (fullBlocks.nonEmpty) {
        val bestReceivedBlock = fullBlocks.maxBy(_.number).number
        if (appStateStorage.getBestBlockNumber() < bestReceivedBlock) appStateStorage.putBestBlockNumber(bestReceivedBlock)
      }
    }

    /** Restarts download from a few blocks behind the current best block header, as an unexpected DB error happened */
    private def redownloadBlockchain(handlerState: HandlerState): HandlerState = {
      log.debug("Missing block header for known hash")
      val syncState = handlerState.syncState
      handlerState.withSyncState(syncState.copy(
        blockBodiesQueue = Nil,
        receiptsQueue = Nil,
        //todo adjust the formula to minimize redownloaded block headers
        bestBlockHeaderNumber = (syncState.bestBlockHeaderNumber - 2 * blockHeadersPerRequest).max(0)
      ))
    }

    private def hashes2strings(requestedHashes: Seq[ByteString]): Seq[String] =
      requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))

    private def handleReceipts(peer: Peer, requestedHashes: Seq[ByteString], receipts: Seq[Seq[Receipt]], handlerState: HandlerState): HandlerState = {
      lazy val knownHashes = hashes2strings(requestedHashes)
      validateReceipts(requestedHashes, receipts) match {
        case ReceiptsValidationResult.Valid(blockHashesWithReceipts) =>
          blockHashesWithReceipts.foreach { case (hash, receiptsForBlock) =>
            blockchain.save(hash, receiptsForBlock)
          }

          val (receivedHashes, _) = blockHashesWithReceipts.unzip
          updateBestBlockIfNeeded(receivedHashes)

          if (receipts.isEmpty) {
            val reason = s"got empty receipts for known hashes: $knownHashes"
            blacklist(peer.id, blacklistDuration, reason)
          }

          val remainingReceipts = requestedHashes.drop(receipts.size)
          if (remainingReceipts.nonEmpty) {
            handlerState.withEnqueueReceipts(remainingReceipts)
          } else {
            handlerState
          }

        case ReceiptsValidationResult.Invalid(error) =>
          val reason = s"got invalid receipts for known hashes: $knownHashes due to: $error"
          blacklist(peer.id, blacklistDuration, reason)
          handlerState.withEnqueueReceipts(requestedHashes)

        case ReceiptsValidationResult.DbError =>
          redownloadBlockchain(handlerState)
      }
    }

    private def handleNodeData(peer: Peer, requestedHashes: Seq[HashType], nodeData: NodeData, handlerState: HandlerState): HandlerState = {
      val nodeValues = nodeData.values
      if (nodeValues.isEmpty) {
        val hashes = requestedHashes.map(h => Hex.toHexString(h.v.toArray[Byte]))
        log.debug(s"Got empty mpt node response for known hashes switching to blockchain only: $hashes")
        blacklist(peer.id, blacklistDuration, "empty mpt node response for known hashes")
      }

      val receivedHashes = nodeValues.map(v => ByteString(kec256(v.toArray[Byte])))
      val remainingHashes = requestedHashes.filterNot(h => receivedHashes.contains(h.v))

      val pendingNodes = collectPendingNodes(nodeData, requestedHashes, receivedHashes, handlerState.syncState.targetBlock.number)
      val downloadedNodes = handlerState.syncState.downloadedNodesCount + nodeValues.size
      val newKnownNodes = downloadedNodes + pendingNodes.size

      handlerState.withNodeData(remainingHashes ++ pendingNodes, downloadedNodes, newKnownNodes)
    }

    private def collectPendingNodes(nodeData: NodeData, requested: Seq[HashType], received: Seq[ByteString], targetNumber: BigInt): Seq[HashType] = {
      val nodeValues = nodeData.values
      (nodeValues.indices zip received) flatMap { case (idx, valueHash) =>
        requested.filter(_.v == valueHash) flatMap {
          case _: StateMptNodeHash =>
            tryToDecodeNodeData(nodeData, idx, targetNumber, handleMptNode)

          case _: ContractStorageMptNodeHash | _: StorageRootHash =>
            tryToDecodeNodeData(nodeData, idx, targetNumber, handleContractMptNode)

          case EvmCodeHash(hash) =>
            blockchain.save(hash, nodeValues(idx))
            Nil
        }
      }
    }

    private def tryToDecodeNodeData(nodeData: NodeData, idx: Int, targetNumber: BigInt, func: (MptNode, BigInt) => Seq[HashType]): Seq[HashType] = {
      // getMptNode throws RLPException
      Try(nodeData.getMptNode(idx)) match {
        case Success(node) =>
          func(node, targetNumber)

        case Failure(msg) =>
          log.warning(s"Cannot decode $nodeData due to: ${msg.getMessage}")
          Nil
      }
    }

    private def handleMptNode(mptNode: MptNode, targetBlock: BigInt): Seq[HashType] = mptNode match {
      case node: LeafNode =>
        blockchain.saveFastSyncNode(ByteString(node.hash), node.toBytes, targetBlock)
        tryToDecodeLeafNode(node)

      case node: BranchNode =>
        blockchain.saveFastSyncNode(ByteString(node.hash), node.toBytes, targetBlock)
        collectChildrenHashes(node).map(hash => StateMptNodeHash(hash))

      case node: ExtensionNode =>
        blockchain.saveFastSyncNode(ByteString(node.hash), node.toBytes, targetBlock)
        node.next match {
          case HashNode(hashNode) => Seq(StateMptNodeHash(hashNode))
          case _ => Nil
        }

      case _ => Nil
    }

    private def tryToDecodeLeafNode(node: LeafNode): Seq[HashType] = {
      import AccountImplicits._
      // If this fails it means that we have LeafNode which is part of MPT that do not stores account
      // We verify if node is part of the tree by checking its hash before we call this method in collectPendingNodes
      Try(node.value.toArray[Byte].toAccount) match {
        case Success(Account(_, _, storageRoot, codeHash)) =>
          val evm = if (codeHash == Account.EmptyCodeHash) Nil else Seq(EvmCodeHash(codeHash))
          val storage = if (storageRoot == Account.EmptyStorageRootHash) Nil else Seq(StorageRootHash(storageRoot))
          evm ++ storage

        case Failure(e) =>
          log.debug(s"Leaf node without account, error while trying to decode account: ${e.getMessage}")
          Nil
      }
    }

    private def collectChildrenHashes(node: BranchNode): Array[ByteString] = {
      node.children.collect { case HashNode(childHash) => childHash }
    }

    private def handleContractMptNode(mptNode: MptNode, targetBlock: BigInt): Seq[HashType] = {
      mptNode match {
        case node: LeafNode =>
          blockchain.saveFastSyncNode(ByteString(node.hash), node.toBytes, targetBlock)
          Nil

        case node: BranchNode =>
          blockchain.saveFastSyncNode(ByteString(node.hash), node.toBytes, targetBlock)
          collectChildrenHashes(node).map(hash => ContractStorageMptNodeHash(hash))

        case node: ExtensionNode =>
          blockchain.saveFastSyncNode(ByteString(node.hash), node.toBytes, targetBlock)
          node.next match {
            case HashNode(hashNode) => Seq(ContractStorageMptNodeHash(hashNode))

            case _ => Nil
          }

        case _ => Nil
      }
    }

    private def handleRequestFailure(peer: Peer, handler: ActorRef, reason: String, handlerState: HandlerState): Unit = {
      context unwatch handler

      if (handshakedPeers.contains(peer)) blacklist(peer.id, blacklistDuration, reason)

      context become receive(handlerState.withPeerAndHandlerRemoved(peer, handler))
    }

    private def persistSyncState(handlerState: HandlerState): Unit = {
      val persistedState = handlerState.persistSyncState()
      context become receive(persistedState)
      syncStateStorageActor ! persistedState.syncState
    }

    private def printStatus(handlerState: HandlerState): Unit = {
      val formatPeer: (Peer) => String = peer => s"${peer.remoteAddress.getAddress.getHostAddress}:${peer.remoteAddress.getPort}"
      val handlers = handlerState.assignedHandlers
      val state = handlerState.syncState
      log.info(
        s"""|Block: ${appStateStorage.getBestBlockNumber()}/${state.targetBlock.number}.
            |Peers waiting_for_response/connected: ${handlers.size}/${handshakedPeers.size} (${blacklistedPeers.size} blacklisted).
            |State: ${state.downloadedNodesCount}/${state.totalNodesCount} nodes.
            |""".stripMargin.replace("\n", " "))
      log.debug(
        s"""|Connection status: connected(${handlers.values.map(formatPeer).toSeq.sorted.mkString(", ")})/
            |handshaked(${handshakedPeers.keys.map(formatPeer).toSeq.sorted.mkString(", ")})
            | blacklisted(${blacklistedPeers.map { case (id, _) => id.value }.mkString(", ")})
            |""".stripMargin.replace("\n", " ")
      )
    }

    private def processSyncing(handlerState: HandlerState): Unit = {
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

    private def finish(handlerState: HandlerState): Unit = {
      log.info("Block synchronization in fast mode finished, switching to regular mode")
      // We have downloaded to target + fastSyncBlockValidationX, se we must discard those last blocks
      discardLastBlocks(handlerState.syncState.safeDownloadTarget, syncConfig.fastSyncBlockValidationX - 1)
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

    private def processDownloads(handlerState: HandlerState): Unit = {
      lazy val handlers = handlerState.assignedHandlers
      val peers = unassignedPeers(handlers)
      if (peers.isEmpty) {
        if (handlers.nonEmpty) {
          log.debug("There are no available peers, waiting for responses")
        } else {
          log.debug("There are no peers to download from, scheduling a retry in {}", syncRetryInterval)
          scheduler.scheduleOnce(syncRetryInterval, self, ProcessSyncing)
        }
        context become receive(handlerState)
      } else {

        peers
          .filter(peer => isPeerRequestTimeConsistentWithFastSyncThrottle(peer, handlerState))
          .take(maxConcurrentRequests - handlers.size)
          .toSeq
          .sortBy(_.ref.toString)
          .foreach(peer => self ! AssignWorkToPeer(peer))
      }
    }

    private def unassignedPeers(assignedHandlers: Map[ActorRef, Peer]): Set[Peer] = {
      peersToDownloadFrom.keySet diff assignedHandlers.values.toSet
    }

    private def isPeerRequestTimeConsistentWithFastSyncThrottle(peer: Peer, handlerState: HandlerState): Boolean = {
      handlerState.peerRequestsTime.get(peer).forall(t => t.plusMillis(fastSyncThrottle.toMillis).isBefore(Instant.now()))
    }

    private def assignWork(peer: Peer, handlerState: HandlerState): HandlerState = {
      if (handlerState.syncState.shouldAssignWork) {
        assignBlockchainWork(peer, handlerState)
      } else {
        requestNodes(peer, handlerState)
      }
    }

    private def assignBlockchainWork(peer: Peer, handlerState: HandlerState): HandlerState = {
      if (handlerState.notEmptyReceiptsQueue) {
        requestReceipts(peer, handlerState)
      } else if (handlerState.notEmptyBodiesQueue) {
        requestBlockBodies(peer, handlerState)
      } else if (handlerState.shouldRequestBlockHeaders && context.child(BlockHeadersHandlerName).isEmpty) {
        requestBlockHeaders(peer, handlerState)
      } else {
        handlerState
      }
    }

    private def requestReceipts(peer: Peer, handlerState: HandlerState): HandlerState = {
      val (receiptsToGet, remainingReceipts) = handlerState.syncState.receiptsQueue.splitAt(receiptsPerRequest)

      val handler = context.actorOf(PeerRequestHandler.props[GetReceipts, Receipts](
        peer = peer,
        responseTimeout = peerResponseTimeout,
        etcPeerManager = etcPeerManager,
        peerEventBus = peerEventBus,
        requestMsg = GetReceipts(receiptsToGet),
        responseMsgCode = Receipts.code
      ))

      context watch handler

      handlerState.withReceipts(handler, remainingReceipts, receiptsToGet).withHandlerAndPeer(handler, peer)
    }

    private def requestBlockBodies(peer: Peer, handlerState: HandlerState): HandlerState = {
      val (bodiesToGet, remainingBodies) = handlerState.syncState.blockBodiesQueue.splitAt(blockBodiesPerRequest)

      val handler = context.actorOf(PeerRequestHandler.props[GetBlockBodies, BlockBodies](
        peer = peer,
        responseTimeout = peerResponseTimeout,
        etcPeerManager = etcPeerManager,
        peerEventBus = peerEventBus,
        requestMsg = GetBlockBodies(bodiesToGet),
        responseMsgCode = BlockBodies.code
      ))

      context watch handler

      handlerState.withBlockBodies(handler, remainingBodies, bodiesToGet).withHandlerAndPeer(handler, peer)
    }

    private def requestBlockHeaders(peer: Peer, handlerState: HandlerState): HandlerState = {
      val bestBlockOffset = handlerState.bestBlockOffset
      val limit: BigInt = if (blockHeadersPerRequest < bestBlockOffset) {
        blockHeadersPerRequest
      } else {
        bestBlockOffset
      }

      val headersToGet = Left(handlerState.nextBestBlockNumber)
      val handler = context.actorOf(
        PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
          peer = peer,
          responseTimeout = peerResponseTimeout,
          etcPeerManager = etcPeerManager,
          peerEventBus = peerEventBus,
          requestMsg = GetBlockHeaders(headersToGet, limit, skip = 0, reverse = false),
          responseMsgCode = BlockHeaders.code
        ),
        BlockHeadersHandlerName
      )

      context watch handler

      handlerState.withRequestedHeaders(handlerState.requestedHeaders + (peer -> limit)).withHandlerAndPeer(handler, peer)
    }

    private def requestNodes(peer: Peer, handlerState: HandlerState): HandlerState = {
      val pendingNodes = handlerState.getPendingNodes(nodesPerRequest)
      val (mptToGet, nonMptToGet)= pendingNodes.toGet
      val nodesToGet = (nonMptToGet ++ mptToGet).map(_.v)

      val handler = context.actorOf(PeerRequestHandler.props[GetNodeData, NodeData](
        peer = peer,
        responseTimeout = peerResponseTimeout,
        etcPeerManager = etcPeerManager,
        peerEventBus = peerEventBus,
        requestMsg = GetNodeData(nodesToGet),
        responseMsgCode = NodeData.code
      ))

      context watch handler

      handlerState.withNodes(handler, nodesPerRequest, pendingNodes).withHandlerAndPeer(handler, peer)
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

  trait FastSyncMsg

  private case class UpdateTargetBlock(state: FinalBlockProcessingResult) extends FastSyncMsg
  private case object ProcessSyncing extends FastSyncMsg
  private[sync] case object PersistSyncState
  private case object PrintStatus
  private case class AssignWorkToPeer(peer: Peer)

  case class SyncState(
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

    def enqueueBlockBodies(blockBodies: Seq[ByteString]): SyncState = copy(blockBodiesQueue = blockBodiesQueue ++ blockBodies)

    def enqueueReceipts(receipts: Seq[ByteString]): SyncState = copy(receiptsQueue = receiptsQueue ++ receipts)

    def setBestBlockNumber(block: BigInt): SyncState = copy(bestBlockHeaderNumber = block)

    def addPendingNodes(hashes: Seq[HashType]): SyncState = {
      val (mpt, nonMpt) = hashes.partition {
        case _: StateMptNodeHash | _: ContractStorageMptNodeHash => true
        case _: EvmCodeHash | _: StorageRootHash => false
      }
      // Nodes are prepended in order to traverse mpt in-depth.
      // For mpt nodes is not needed but to keep it consistent, it was applied too
      copy(pendingMptNodes = mpt ++ pendingMptNodes, pendingNonMptNodes = nonMpt ++ pendingNonMptNodes)
    }

    def notEmptyBodiesQueue: Boolean = blockBodiesQueue.nonEmpty

    def notEmptyReceiptsQueue: Boolean = receiptsQueue.nonEmpty

    def anythingQueued: Boolean = pendingNonMptNodes.nonEmpty || pendingMptNodes.nonEmpty || blockChainWorkQueued

    def blockChainWorkQueued: Boolean =  notEmptyBodiesQueue || notEmptyReceiptsQueue

    def bestBlockDoesNotReachDownloadTarget: Boolean = bestBlockHeaderNumber < safeDownloadTarget

    def shouldAssignWork: Boolean = bestBlockDoesNotReachDownloadTarget || blockChainWorkQueued

    def shouldDownloadMoreItems: Boolean = anythingQueued || bestBlockDoesNotReachDownloadTarget && !updatingTargetBlock

    def updateNextBlockToValidate(header: BlockHeader, K: Int, X: Int): SyncState = {
      val headerNumber = header.number
      val targetOffset = targetBlock.number - X
      val newNextBlock = if (bestBlockHeaderNumber >= targetOffset) {
        headerNumber + 1
      } else {
        (headerNumber + K / 2 + Random.nextInt(K)).min(targetOffset)
      }
      copy(nextBlockToFullyValidate = newNextBlock)
    }

    def updateDiscardedBlocks(header: BlockHeader, N: Int): SyncState = copy(
      blockBodiesQueue = Nil,
      receiptsQueue = Nil,
      bestBlockHeaderNumber = (header.number - N - 1) max 0,
      nextBlockToFullyValidate = (header.number - N) max 1
    )

    def updateTargetBlock(newTarget: BlockHeader, numberOfSafeBlocks: BigInt, updateFailures: Boolean): SyncState = copy(
      targetBlock = newTarget,
      safeDownloadTarget = newTarget.number + numberOfSafeBlocks,
      targetBlockUpdateFailures = if (updateFailures) targetBlockUpdateFailures + 1 else targetBlockUpdateFailures
    )
  }

  case class HandlerState(
    syncState: SyncState,
    requestedHeaders: Map[Peer, BigInt] = Map.empty,
    assignedHandlers: Map[ActorRef, Peer] = Map.empty,
    peerRequestsTime: Map[Peer, Instant] = Map.empty,
    requestedMptNodes: Map[ActorRef, Seq[HashType]] = Map.empty,
    requestedNonMptNodes: Map[ActorRef, Seq[HashType]] = Map.empty,
    requestedBlockBodies: Map[ActorRef, Seq[ByteString]] = Map.empty,
    requestedReceipts: Map[ActorRef, Seq[ByteString]] = Map.empty
  ) {

    def withSyncState(state: SyncState): HandlerState = copy(syncState = state)

    def withRequestedHeaders(headers: Map[Peer, BigInt]): HandlerState = copy(requestedHeaders = headers)

    def removeHandler(handler: ActorRef): HandlerState = copy(assignedHandlers = assignedHandlers - handler)

    def removeNodes(requester: ActorRef): HandlerState =
      copy(requestedMptNodes = requestedMptNodes - requester, requestedNonMptNodes = requestedNonMptNodes - requester)

    def withRequestedBlockBodies(bodies: Map[ActorRef, Seq[ByteString]]): HandlerState = copy(requestedBlockBodies = bodies)

    def withRequestedReceipts(receipts: Map[ActorRef, Seq[ByteString]]): HandlerState = copy(requestedReceipts = receipts)

    def noAssignedHandlers: Boolean = assignedHandlers.isEmpty

    def notEmptyReceiptsQueue: Boolean = syncState.notEmptyReceiptsQueue

    def notEmptyBodiesQueue: Boolean = syncState.notEmptyBodiesQueue

    def shouldRequestBlockHeaders: Boolean = requestedHeaders.isEmpty && syncState.bestBlockDoesNotReachDownloadTarget

    def updateTargetBlock(target: BlockHeader, safeBlocksCount: Int, failures: Boolean): HandlerState =
      withSyncState(syncState.updateTargetBlock(target, safeBlocksCount, updateFailures = failures))

    def updateValidationState(header: BlockHeader, syncConfig: SyncConfig): HandlerState = {
      import syncConfig.{ fastSyncBlockValidationK => K, fastSyncBlockValidationX => X }
      withSyncState(syncState.updateNextBlockToValidate(header, K, X))
    }

    def updateFailuresNotReachedTheLimit(limit: Int): Boolean = syncState.targetBlockUpdateFailures <= limit

    def withUpdatingTargetBlock(updating: Boolean): HandlerState = withSyncState(syncState.copy(updatingTargetBlock = updating))

    def increaseUpdateFailures(): HandlerState =
      withSyncState(syncState.copy(targetBlockUpdateFailures = syncState.targetBlockUpdateFailures + 1))

    def updateTargetSyncState(state: FinalBlockProcessingResult, target: BlockHeader, syncConfig: SyncConfig): (HandlerState, String) = {
      lazy val downloadTarget = syncState.safeDownloadTarget
      lazy val safeBlocksCount = syncConfig.fastSyncBlockValidationX

      state match {
        case ImportedLastBlock =>
          val block = syncState.targetBlock
          if (target.number - block.number <= syncConfig.maxTargetDifference) {
            val logMsg = "Current target block is fresh enough, starting state download"
            (withSyncState(syncState.copy(pendingMptNodes = Seq(StateMptNodeHash(block.stateRoot)))), logMsg)
          } else {
            val logMsg = s"Changing target block to ${target.number}, new safe target is $downloadTarget"
            (updateTargetBlock(target, safeBlocksCount, failures = false), logMsg)
          }

        case LastBlockValidationFailed =>
          val logMsg = s"Changing target block after failure, to ${target.number}, new safe target is $downloadTarget"
          (updateTargetBlock(target, safeBlocksCount, failures = true), logMsg)
      }
    }

    def updateBestBlockNumber(header: BlockHeader, parentTd: BigInt, shouldUpdate: Boolean, syncConfig: SyncConfig): HandlerState = {
      val hashes = Seq(header.hash)
      val newSyncState = syncState.enqueueBlockBodies(hashes).enqueueReceipts(hashes)

      val withBestBlockNumber = if (header.number > newSyncState.bestBlockHeaderNumber) {
        withSyncState(newSyncState.setBestBlockNumber(header.number))
      } else {
        withSyncState(newSyncState)
      }

      if (shouldUpdate) {
        withBestBlockNumber.updateValidationState(header, syncConfig)
      } else {
        withBestBlockNumber
      }
    }

    def withEnqueueBlockBodies(bodies: Seq[ByteString]): HandlerState = withSyncState(syncState.enqueueBlockBodies(bodies))

    def withEnqueueReceipts(receipts: Seq[ByteString]): HandlerState = withSyncState(syncState.enqueueReceipts(receipts))

    def isFullySynced: Boolean =
      syncState.bestBlockHeaderNumber >= syncState.safeDownloadTarget && !syncState.anythingQueued && noAssignedHandlers

    def bestBlockOffset: BigInt = syncState.safeDownloadTarget - syncState.bestBlockHeaderNumber

    def nextBestBlockNumber: BigInt = syncState.bestBlockHeaderNumber + 1

    def getRequestedNodes(requester: ActorRef): Seq[HashType] =
      requestedMptNodes.getOrElse(requester, Nil) ++ requestedNonMptNodes.getOrElse(requester, Nil)

    def getPendingNodes(nodesPerRequest: Int): PendingNodes = {
      val (nonMptNodesToGet, remainingNonMptNodes) = syncState.pendingNonMptNodes.splitAt(nodesPerRequest)
      val (mptNodesToGet, remainingMptNodes) = syncState.pendingMptNodes.splitAt(nodesPerRequest - nonMptNodesToGet.size)
      PendingNodes((mptNodesToGet, nonMptNodesToGet),(remainingMptNodes, remainingNonMptNodes))
    }

    def withNodes(handler: ActorRef, nodesPerRequest: Int, pendingNodes: PendingNodes): HandlerState = {
      val PendingNodes((mptNodesToGet, nonMptNodesToGet), (remainingMptNodes, remainingNonMptNodes)) = pendingNodes

      copy(
        syncState = syncState.copy(pendingNonMptNodes = remainingNonMptNodes, pendingMptNodes = remainingMptNodes),
        requestedMptNodes = requestedMptNodes + (handler -> mptNodesToGet),
        requestedNonMptNodes = requestedNonMptNodes + (handler -> nonMptNodesToGet)
      )
    }

    def withBlockBodies(handler: ActorRef, remaining: Seq[ByteString], toGet: Seq[ByteString]): HandlerState = copy(
      syncState = syncState.copy(blockBodiesQueue = remaining),
      requestedBlockBodies = requestedBlockBodies + (handler -> toGet)
    )

    def withReceipts(handler: ActorRef, remaining: Seq[ByteString], toGet: Seq[ByteString]): HandlerState = copy(
      syncState = syncState.copy(receiptsQueue = remaining),
      requestedReceipts = requestedReceipts + (handler -> toGet)
    )

    def withHandlerAndPeer(handler: ActorRef, peer: Peer): HandlerState = copy(
      assignedHandlers = assignedHandlers + (handler -> peer),
      peerRequestsTime = peerRequestsTime + (peer -> Instant.now())
    )

    def withNodeData(pendingNodes: Seq[HashType], downloaded: Int, total: Int): HandlerState = {
      withSyncState(syncState.addPendingNodes(pendingNodes).copy(downloadedNodesCount = downloaded, totalNodesCount = total))
    }

    def persistSyncState(): HandlerState = {
      def mapValuesToHashes[K, HashType](map: Map[K, Seq[HashType]]): Seq[HashType] = map.values.flatten.toSeq.distinct

      withSyncState(syncState.copy(
        pendingMptNodes = mapValuesToHashes(requestedMptNodes) ++ syncState.pendingMptNodes,
        pendingNonMptNodes = mapValuesToHashes(requestedNonMptNodes) ++ syncState.pendingNonMptNodes,
        blockBodiesQueue = mapValuesToHashes(requestedBlockBodies) ++ syncState.blockBodiesQueue,
        receiptsQueue = mapValuesToHashes(requestedReceipts) ++ syncState.receiptsQueue
      ))
    }

    def withPeerAndHandlerRemoved(peer: Peer, handler: ActorRef): HandlerState = {
      val newSyncState = syncState
        .addPendingNodes(getRequestedNodes(handler))
        .enqueueBlockBodies(requestedBlockBodies.getOrElse(handler, Nil))
        .enqueueReceipts(requestedReceipts.getOrElse(handler, Nil))

      withSyncState(newSyncState)
        .removeHandler(handler)
        .removeNodes(handler)
        .withRequestedHeaders(requestedHeaders - peer)
        .withRequestedBlockBodies(requestedBlockBodies - handler)
        .withRequestedReceipts(requestedReceipts - handler)
    }
  }

  case class PendingNodes(toGet: (Seq[HashType], Seq[HashType]), remaining: (Seq[HashType], Seq[HashType]))

  sealed trait HashType {
    def v: ByteString
  }

  case class StateMptNodeHash(v: ByteString) extends HashType
  case class ContractStorageMptNodeHash(v: ByteString) extends HashType
  case class EvmCodeHash(v: ByteString) extends HashType
  case class StorageRootHash(v: ByteString) extends HashType

  case object Start
  case object Done

  sealed abstract class HeaderProcessingResult
  case object HeadersProcessingFinished                         extends HeaderProcessingResult
  case class  ParentDifficultyNotFound(header:BlockHeader)      extends HeaderProcessingResult
  case class  ValidationFailed(header:BlockHeader, peer: Peer)  extends HeaderProcessingResult
  case object ImportedTargetBlock                               extends HeaderProcessingResult

  sealed abstract class FinalBlockProcessingResult
  case object ImportedLastBlock         extends FinalBlockProcessingResult
  case object LastBlockValidationFailed extends FinalBlockProcessingResult
}
