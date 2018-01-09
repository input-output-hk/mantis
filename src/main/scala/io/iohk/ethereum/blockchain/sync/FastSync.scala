package io.iohk.ethereum.blockchain.sync

import java.time.Instant

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.SyncBlocksValidator.BlockBodyValidationResult
import io.iohk.ethereum.blockchain.sync.FastSyncReceiptsValidator.ReceiptsValidationResult
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.storage.{AppStateStorage, FastSyncStateStorage}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.{BranchNode, ExtensionNode, LeafNode, MptNode}
import io.iohk.ethereum.network.p2p.messages.PV63.MptNodeEncoders._
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.validators.Validators
import org.spongycastle.util.encoders.Hex

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

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
    log.info(s"Starting block synchronization (fast mode), target block ${syncState.targetBlock.number}")
    val syncingHandler = new SyncingHandler(syncState)
    context become syncingHandler.receive
    syncingHandler.processSyncing()
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
        val initialSyncState = SyncState(targetBlockHeader, pendingMptNodes = Seq(StateMptNodeHash(targetBlockHeader.stateRoot)))
        startWithState(initialSyncState)
      }
  }

  private class SyncingHandler(initialSyncState: SyncState) {

    private val BlockHeadersHandlerName = "block-headers-request-handler"

    private var syncState = initialSyncState

    private var assignedHandlers: Map[ActorRef, Peer] = Map.empty
    private var peerRequestsTime: Map[Peer, Instant] = Map.empty

    private var requestedMptNodes: Map[ActorRef, Seq[HashType]] = Map.empty
    private var requestedNonMptNodes: Map[ActorRef, Seq[HashType]] = Map.empty
    private var requestedBlockBodies: Map[ActorRef, Seq[ByteString]] = Map.empty
    private var requestedReceipts: Map[ActorRef, Seq[ByteString]] = Map.empty

    private val syncStateStorageActor = context.actorOf(Props[FastSyncStateStorageActor], "state-storage")
    syncStateStorageActor ! fastSyncStateStorage

    private var blockChainOnlyPeers: Seq[Peer] = Nil

    //Delay before starting to persist snapshot. It should be 0, as the presence of it marks that fast sync was started
    private val persistStateSnapshotDelay: FiniteDuration = 0.seconds
    private val syncStatePersistCancellable = scheduler.schedule(persistStateSnapshotDelay, persistStateSnapshotInterval, self, PersistSyncState)
    private val printStatusCancellable = scheduler.schedule(printStatusInterval, printStatusInterval, self, PrintStatus)
    private val heartBeat = scheduler.schedule(syncRetryInterval, syncRetryInterval * 2, self, ProcessSyncing)

    def receive: Receive = handleCommonMessages orElse {
      case ProcessSyncing => processSyncing()
      case PrintStatus => printStatus()
      case PersistSyncState => persistSyncState()

      case ResponseReceived(peer, BlockHeaders(blockHeaders), timeTaken) =>
        log.info("Received {} block headers in {} ms", blockHeaders.size, timeTaken)
        removeRequestHandler(sender())
        handleBlockHeaders(peer, blockHeaders)

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

      case ResponseReceived(peer, nodeData: NodeData, timeTaken) =>
        log.info("Received {} state nodes in {} ms", nodeData.values.size, timeTaken)
        val requestedHashes = requestedMptNodes.getOrElse(sender(), Nil) ++ requestedNonMptNodes.getOrElse(sender(), Nil)
        requestedMptNodes -= sender()
        requestedNonMptNodes -= sender()
        removeRequestHandler(sender())
        handleNodeData(peer, requestedHashes, nodeData)

      case PeerRequestHandler.RequestFailed(peer, reason) =>
        handleRequestFailure(peer, sender(), reason)

      case Terminated(ref) if assignedHandlers.contains(ref) =>
        handleRequestFailure(assignedHandlers(ref), ref, "Unexpected error")
    }

    private def removeRequestHandler(handler: ActorRef): Unit = {
      context unwatch handler
      assignedHandlers -= handler
    }

    private def insertHeader(header: BlockHeader): Boolean = {
      val parentTdOpt = blockchain.getTotalDifficultyByHash(header.parentHash)
      parentTdOpt foreach { parentTotalDifficulty =>
        blockchain.save(header)
        blockchain.save(header.hash, parentTotalDifficulty + header.difficulty)

        if (header.number > syncState.bestBlockHeaderNumber) {
          syncState = syncState.copy(bestBlockHeaderNumber = header.number)
        }

        syncState = syncState
          .enqueueBlockBodies(Seq(header.hash))
          .enqueueReceipts(Seq(header.hash))
      }

      parentTdOpt.isDefined
    }

    private def discardLastBlocks(startBlock: BigInt, blocksToDiscard: Int): Unit = {
      (startBlock to ((startBlock - blocksToDiscard) max 1) by -1).foreach { n =>
        blockchain.getBlockHeaderByNumber(n).foreach { headerToRemove =>
          blockchain.removeBlock(headerToRemove.hash, saveParentAsBestBlock = false)
        }
      }
      blockchain.saveBestBlockNumber((startBlock - blocksToDiscard - 1) max 0)
    }

    @tailrec
    private def processHeaders(peer: Peer, headers: Seq[BlockHeader]): Boolean = {
      import syncConfig.{fastSyncBlockValidationK => K, fastSyncBlockValidationN => N, fastSyncBlockValidationX => X}
      if (headers.nonEmpty) {
        val header = headers.head
        val remaining = headers.tail

        val shouldValidate = header.number >= syncState.nextBlockToFullyValidate
        if (shouldValidate) {
          syncState = syncState.copy(
            nextBlockToFullyValidate =
              if (syncState.bestBlockHeaderNumber >= syncState.targetBlock.number - X) header.number + 1
              else header.number + K / 2 + Random.nextInt(K))

          validators.blockHeaderValidator.validate(header, blockchain.getBlockHeaderByHash) match {
            case Right(_) =>
              if (insertHeader(header)) processHeaders(peer, remaining)
              else true

            case Left(error) =>
              log.warning(s"Block header validation failed during fast sync at block ${header.number}: $error")

              if (header.number == syncState.targetBlock.number) {
                // target validation failed, declare a failure and stop syncing
                log.warning(s"Sync failure! Block header validation failed at fast sync target block. Blockchain state may be invalid.")
                sys.exit(1)
                false
              } else {
                // validation failed at non-target block, discard N blocks and resume
                blacklist(peer.id, blacklistDuration, "block header validation failed")
                discardLastBlocks(header.number, N)
                syncState = syncState.copy(
                  blockBodiesQueue = Seq.empty,
                  receiptsQueue = Seq.empty,
                  bestBlockHeaderNumber = (header.number - N - 1) max 0,
                  nextBlockToFullyValidate = (header.number - N) max 1)
                true // do not process remaining headers and resume
              }
          }
        } else {
          if (insertHeader(header)) processHeaders(peer, remaining)
          else true // do not process remaining headers and resume
        }
      } else true
    }

    private def handleBlockHeaders(peer: Peer, headers: Seq[BlockHeader]) = {
      if (checkHeadersChain(headers)) {
        if (processHeaders(peer, headers)) processSyncing()
      } else {
        blacklist(peer.id, blacklistDuration, "error in block headers response")
        processSyncing()
      }
    }

    private def handleBlockBodies(peer: Peer, requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]) = {
      if (blockBodies.isEmpty) {
        val reason = s"got empty block bodies response for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}"
        blacklist(peer.id, blacklistDuration, reason)
        syncState = syncState.enqueueBlockBodies(requestedHashes)
      } else {
        validateBlocks(requestedHashes, blockBodies) match {
          case BlockBodyValidationResult.Valid =>
            insertBlocks(requestedHashes, blockBodies)
          case BlockBodyValidationResult.Invalid =>
            blacklist(peer.id, blacklistDuration, s"responded with block bodies not matching block headers, blacklisting for $blacklistDuration")
            syncState = syncState.enqueueBlockBodies(requestedHashes)
          case BlockBodyValidationResult.DbError =>
            redownloadBlockchain()
        }
      }

      processSyncing()
    }

    private def handleReceipts(peer: Peer, requestedHashes: Seq[ByteString], receipts: Seq[Seq[Receipt]]) = {
      validateReceipts(requestedHashes, receipts) match {
        case ReceiptsValidationResult.Valid(blockHashesWithReceipts) =>
          blockHashesWithReceipts.foreach { case (hash, receiptsForBlock) =>
            blockchain.save(hash, receiptsForBlock)
          }

          val receivedHashes = blockHashesWithReceipts.unzip._1
          updateBestBlockIfNeeded(receivedHashes)

          if (receipts.isEmpty) {
            val reason = s"got empty receipts for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}"
            blacklist(peer.id, blacklistDuration, reason)
          }

          val remainingReceipts = requestedHashes.drop(receipts.size)
          if (remainingReceipts.nonEmpty) {
            syncState = syncState.enqueueReceipts(remainingReceipts)
          }

        case ReceiptsValidationResult.Invalid(error) =>
          val reason =
            s"got invalid receipts for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}" +
              s" due to: $error"
          blacklist(peer.id, blacklistDuration, reason)
          syncState = syncState.enqueueReceipts(requestedHashes)

        case ReceiptsValidationResult.DbError =>
          redownloadBlockchain()
      }

      processSyncing()
    }

    private def handleNodeData(peer: Peer, requestedHashes: Seq[HashType], nodeData: NodeData) = {
      if (nodeData.values.isEmpty) {
        log.debug(s"got empty mpt node response for known hashes switching to blockchain only: ${requestedHashes.map(h => Hex.toHexString(h.v.toArray[Byte]))}")
        markPeerBlockchainOnly(peer)
      }

      val receivedHashes = nodeData.values.map(v => ByteString(kec256(v.toArray[Byte])))
      val remainingHashes = requestedHashes.filterNot(h => receivedHashes.contains(h.v))
      if (remainingHashes.nonEmpty) {
        syncState = syncState.addPendingNodes(remainingHashes)
      }

      val hashesToRequest = (nodeData.values.indices zip receivedHashes) flatMap { case (idx, valueHash) =>
        requestedHashes.find(_.v == valueHash) map {
          case _: StateMptNodeHash =>
            handleMptNode(nodeData.getMptNode(idx))

          case _: ContractStorageMptNodeHash =>
            handleContractMptNode(nodeData.getMptNode(idx))

          case EvmCodeHash(hash) =>
            val evmCode = nodeData.values(idx)
            blockchain.save(hash, evmCode)
            Nil

          case StorageRootHash(_) =>
            val rootNode = nodeData.getMptNode(idx)
            handleContractMptNode(rootNode)
        }
      }

      syncState = syncState
        .addPendingNodes(hashesToRequest.flatten)
        .copy(downloadedNodesCount = syncState.downloadedNodesCount + nodeData.values.size)

      processSyncing()
    }

    private def handleMptNode(mptNode: MptNode): Seq[HashType] = mptNode match {
      case n: LeafNode =>
        import AccountImplicits._
        //if this fails it means that we have leaf node which is part of MPT that do not stores account
        //we verify if node is paert of the tree by checking its hash before we call handleMptNode() in line 44
        val account = Try(n.value.toArray[Byte].toAccount) match {
          case Success(acc) => Some(acc)
          case Failure(e) =>
            log.debug(s"Leaf node without account, error while trying to decode account ${e.getMessage}")
            None
        }

        val evm = account.map(_.codeHash)
        val storage = account.map(_.storageRoot)

        blockchain.saveNode(ByteString(n.hash), n.toBytes, initialSyncState.targetBlock.number)

        val evmRequests = evm
          .filter(_ != Account.EmptyCodeHash)
          .map(c => Seq(EvmCodeHash(c))).getOrElse(Nil)

        val storageRequests = storage
          .filter(_ != Account.EmptyStorageRootHash)
          .map(s => Seq(StorageRootHash(s))).getOrElse(Nil)

        evmRequests ++ storageRequests

      case n: BranchNode =>
        val hashes = n.children.collect { case Some(Left(childHash)) => childHash }
        blockchain.saveNode(ByteString(n.hash), n.toBytes, initialSyncState.targetBlock.number)
        hashes.map(e => StateMptNodeHash(e))

      case n: ExtensionNode =>
        blockchain.saveNode(ByteString(n.hash), n.toBytes, initialSyncState.targetBlock.number)
        n.next.fold(
          mptHash => Seq(StateMptNodeHash(mptHash)),
          _ => Nil)
    }

    private def handleContractMptNode(mptNode: MptNode): Seq[HashType] = {
      mptNode match {
        case n: LeafNode =>
          blockchain.saveNode(ByteString(n.hash), n.toBytes, initialSyncState.targetBlock.number)
          Nil

        case n: BranchNode =>
          val hashes = n.children.collect { case Some(Left(childHash)) => childHash }
          blockchain.saveNode(ByteString(n.hash), n.toBytes, initialSyncState.targetBlock.number)
          hashes.map(e => ContractStorageMptNodeHash(e))

        case n: ExtensionNode =>
          blockchain.saveNode(ByteString(n.hash), n.toBytes, initialSyncState.targetBlock.number)
          n.next.fold(
            mptHash => Seq(ContractStorageMptNodeHash(mptHash)),
            _ => Nil)
      }
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
      syncStateStorageActor ! syncState.copy(
        pendingMptNodes = requestedMptNodes.values.flatten.toSeq.distinct ++ syncState.pendingMptNodes,
        pendingNonMptNodes = requestedNonMptNodes.values.flatten.toSeq.distinct ++ syncState.pendingNonMptNodes,
        blockBodiesQueue = requestedBlockBodies.values.flatten.toSeq.distinct ++ syncState.blockBodiesQueue,
        receiptsQueue = requestedReceipts.values.flatten.toSeq.distinct ++ syncState.receiptsQueue)
    }

    private def markPeerBlockchainOnly(peer: Peer): Unit = {
      if (!blockChainOnlyPeers.contains(peer)) {
        blockChainOnlyPeers = (peer +: blockChainOnlyPeers).take(blockChainOnlyPeersPoolSize)
      }
    }

    private def printStatus() = {
      val formatPeer: (Peer) => String = peer => s"${peer.remoteAddress.getAddress.getHostAddress}:${peer.remoteAddress.getPort}"
      log.info(
        s"""|Block: ${appStateStorage.getBestBlockNumber()}/${initialSyncState.targetBlock.number}.
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

    private def insertBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): Unit = {
      (requestedHashes zip blockBodies).foreach { case (hash, body) =>
        blockchain.save(hash, body)
      }

      val receivedHashes = requestedHashes.take(blockBodies.size)
      updateBestBlockIfNeeded(receivedHashes)
      val remainingBlockBodies = requestedHashes.drop(blockBodies.size)
      if (remainingBlockBodies.nonEmpty) {
        syncState = syncState.enqueueBlockBodies(remainingBlockBodies)
      }
    }

    def processSyncing(): Unit = {
      if (fullySynced) {
        finish()
      } else {
        if (anythingToDownload) processDownloads()
        else log.debug("No more items to request, waiting for {} responses", assignedHandlers.size)
      }
    }

    def finish(): Unit = {
      log.info("Block synchronization in fast mode finished, switching to regular mode")
      cleanup()
      appStateStorage.fastSyncDone()
      context become idle
      blockChainOnlyPeers = Seq.empty
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
        val blockChainPeers = blockChainOnlyPeers.toSet
        (peers -- blockChainPeers)
          .take(maxConcurrentRequests - assignedHandlers.size)
          .toSeq.sortBy(_.ref.toString())
          .foreach(assignWork)
        peers
          .intersect(blockChainPeers)
          .take(maxConcurrentRequests - assignedHandlers.size)
          .toSeq.sortBy(_.ref.toString())
          .foreach(assignBlockchainWork)
      }
    }

    def assignWork(peer: Peer): Unit = {
      if (syncState.pendingNonMptNodes.nonEmpty || syncState.pendingMptNodes.nonEmpty) {
        requestNodes(peer)
      } else {
        assignBlockchainWork(peer)
      }
    }

    def assignBlockchainWork(peer: Peer): Unit = {
      if (syncState.receiptsQueue.nonEmpty) {
        requestReceipts(peer)
      } else if (syncState.blockBodiesQueue.nonEmpty) {
        requestBlockBodies(peer)
      } else if (context.child(BlockHeadersHandlerName).isEmpty &&
        initialSyncState.targetBlock.number > syncState.bestBlockHeaderNumber) {
        requestBlockHeaders(peer)
      }
    }

    def requestReceipts(peer: Peer): Unit = {
      val (receiptsToGet, remainingReceipts) = syncState.receiptsQueue.splitAt(receiptsPerRequest)

      val handler = context.actorOf(
        PeerRequestHandler.props[GetReceipts, Receipts](
          peer, peerResponseTimeout, etcPeerManager, peerEventBus,
          requestMsg = GetReceipts(receiptsToGet),
          responseMsgCode = Receipts.code))

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
          peer, peerResponseTimeout, etcPeerManager, peerEventBus,
          requestMsg = GetBlockBodies(blockBodiesToGet),
          responseMsgCode = BlockBodies.code))

      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
      syncState = syncState.copy(blockBodiesQueue = remainingBlockBodies)
      requestedBlockBodies += handler -> blockBodiesToGet
    }

    def requestBlockHeaders(peer: Peer): Unit = {
      val limit: BigInt = if (blockHeadersPerRequest < (initialSyncState.targetBlock.number - syncState.bestBlockHeaderNumber))
        blockHeadersPerRequest
      else
        initialSyncState.targetBlock.number - syncState.bestBlockHeaderNumber

      val handler = context.actorOf(
        PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
          peer, peerResponseTimeout, etcPeerManager, peerEventBus,
          requestMsg = GetBlockHeaders(Left(syncState.bestBlockHeaderNumber + 1), limit, skip = 0, reverse = false),
          responseMsgCode = BlockHeaders.code), BlockHeadersHandlerName)

      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
    }

    def requestNodes(peer: Peer): Unit = {
      val (nonMptNodesToGet, remainingNonMptNodes) = syncState.pendingNonMptNodes.splitAt(nodesPerRequest)
      val (mptNodesToGet, remainingMptNodes) = syncState.pendingMptNodes.splitAt(nodesPerRequest - nonMptNodesToGet.size)
      val nodesToGet = nonMptNodesToGet ++ mptNodesToGet

      val handler = context.actorOf(
        PeerRequestHandler.props[GetNodeData, NodeData](
          peer, peerResponseTimeout, etcPeerManager, peerEventBus,
          requestMsg = GetNodeData(nodesToGet.map(_.v)),
          responseMsgCode = NodeData.code))

      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
      syncState = syncState.copy(
        pendingNonMptNodes = remainingNonMptNodes,
        pendingMptNodes = remainingMptNodes)
      requestedMptNodes += handler -> mptNodesToGet
      requestedNonMptNodes += handler -> nonMptNodesToGet
    }

    def unassignedPeers: Set[Peer] = peersToDownloadFrom.keySet diff assignedHandlers.values.toSet

    def anythingToDownload: Boolean =
      syncState.anythingQueued || syncState.bestBlockHeaderNumber < initialSyncState.targetBlock.number

    def fullySynced: Boolean = {
      syncState.bestBlockHeaderNumber >= initialSyncState.targetBlock.number &&
      !syncState.anythingQueued &&
      assignedHandlers.isEmpty
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
      val bestReceivedBlock = fullBlocks.maxBy(_.number)
      if (appStateStorage.getBestBlockNumber() < bestReceivedBlock.number) {
        appStateStorage.putBestBlockNumber(bestReceivedBlock.number)
      }
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

  case class SyncState(
    targetBlock: BlockHeader,
    pendingMptNodes: Seq[HashType] = Nil,
    pendingNonMptNodes: Seq[HashType] = Nil,
    blockBodiesQueue: Seq[ByteString] = Nil,
    receiptsQueue: Seq[ByteString] = Nil,
    downloadedNodesCount: Int = 0,
    bestBlockHeaderNumber: BigInt = 0,
    nextBlockToFullyValidate: BigInt = 1) {

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
      blockBodiesQueue.nonEmpty ||
      receiptsQueue.nonEmpty

    val totalNodesCount: Int = downloadedNodesCount + pendingMptNodes.size + pendingNonMptNodes.size
  }

  sealed trait HashType {
    def v: ByteString
  }

  case class StateMptNodeHash(v: ByteString) extends HashType
  case class ContractStorageMptNodeHash(v: ByteString) extends HashType
  case class EvmCodeHash(v: ByteString) extends HashType
  case class StorageRootHash(v: ByteString) extends HashType

  case object Start
  case object Done
}
