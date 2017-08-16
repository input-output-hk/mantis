package io.iohk.ethereum.blockchain.sync


import java.time.Instant
import java.util.Date

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSync.ReceiptsValidationResult.{ReceiptsDbError, ReceiptsInvalid, ReceiptsValid}
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{Account, Block, BlockHeader, Receipt}
import io.iohk.ethereum.mpt.NodesKeyValueStorage
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}
import io.iohk.ethereum.utils.Config.Sync._
import io.iohk.ethereum.validators.BlockValidator
import io.iohk.ethereum.validators.BlockValidator.BlockError
import org.spongycastle.util.encoders.Hex

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

    private var syncState = initialSyncState

    private var assignedHandlers: Map[ActorRef, Peer] = Map.empty
    private var peerRequestsTime: Map[Peer, Instant] = Map.empty

    private val syncStateStorageActor = context.actorOf(Props[FastSyncStateActor], "state-storage")

    private var requestedMptNodes: Map[ActorRef, Seq[HashType]] = Map.empty
    private var requestedNonMptNodes: Map[ActorRef, Seq[HashType]] = Map.empty
    private var requestedBlockBodies: Map[ActorRef, Seq[ByteString]] = Map.empty
    private var requestedReceipts: Map[ActorRef, Seq[ByteString]] = Map.empty

    syncStateStorageActor ! fastSyncStateStorage

    private var blockChainOnlyPeers: Seq[Peer] = Nil

    //Delay before starting to persist snapshot. It should be 0, as the presence of it marks that fast sync was started
    private val persistStateSnapshotDelay: FiniteDuration = 0.seconds

    private val syncStatePersistCancellable = scheduler.schedule(persistStateSnapshotDelay, persistStateSnapshotInterval, self, PersistSyncState)
    private val heartBeat = scheduler.schedule(syncRetryInterval, syncRetryInterval * 2, self, ProcessSyncing)

    def receive: Receive = handlePeerUpdates orElse {
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
      val peer = assignedHandlers(handler)
      peerRequestsTime -= peer
      assignedHandlers -= handler
    }

    private def handleBlockHeaders(peer: Peer, headers: Seq[BlockHeader]) = {
      if (checkHeaders(headers)) insertHeaders(headers)
      else blacklist(peer.id, blacklistDuration, "error in block headers response")

      processSyncing()
    }

    private def handleBlockBodies(peer: Peer, requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]) = {
      if (blockBodies.isEmpty) {
        val reason = s"got empty block bodies response for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}"
        blacklist(peer.id, blacklistDuration, reason)
        syncState = syncState.enqueueBlockBodies(requestedBlockBodies.getOrElse(sender(), Nil))
      } else {
        validateBlocks(requestedHashes, blockBodies) match {
          case Valid =>
            insertBlocks(requestedHashes, blockBodies)
          case Invalid =>
            blacklist(peer.id, blacklistDuration, s"responded with block bodies not matching block headers, blacklisting for $blacklistDuration")
            syncState = syncState.enqueueBlockBodies(requestedHashes)
          case DbError =>
            redownloadBlockchain()
        }
      }

      processSyncing()
    }

    private def handleReceipts(peer: Peer, requestedHashes: Seq[ByteString], receipts: Seq[Seq[Receipt]]) = {
      validateReceipts(requestedHashes, receipts) match {
        case ReceiptsValid(blockHashesWithReceipts) =>
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

        case ReceiptsInvalid(error) =>
          val reason =
            s"got invalid receipts for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}" +
              s" due to: $error"
          blacklist(peer.id, blacklistDuration, reason)
          syncState = syncState.enqueueReceipts(requestedHashes)

        case ReceiptsDbError =>
          redownloadBlockchain()
      }

      processSyncing()
    }

    /**
      * Validates whether the received receipts match the block headers stored on the blockchain,
      * returning the valid receipts
      *
      * @param requestedHashes hash of the blocks to which the requested receipts should belong
      * @param receipts received by the peer
      * @return the valid receipts or the error encountered while validating them
      */
    private def validateReceipts(requestedHashes: Seq[ByteString], receipts: Seq[Seq[Receipt]]): ReceiptsValidationResult = {
      val blockHashesWithReceipts = requestedHashes.zip(receipts)
      val blockHeadersWithReceipts = blockHashesWithReceipts.map{ case (hash, blockReceipts) =>
        blockchain.getBlockHeaderByHash(hash) -> blockReceipts }

      val receiptsValidationError = blockHeadersWithReceipts.collectFirst {
        case (Some(header), receipt) if validators.blockValidator.validateBlockAndReceipts(header, receipt).isLeft =>
          ReceiptsInvalid(validators.blockValidator.validateBlockAndReceipts(header, receipt).left.get)
        case (None, _) => ReceiptsDbError
      }
      receiptsValidationError match {
        case Some(error) => error
        case None => ReceiptsValid(blockHashesWithReceipts)
      }
    }

    private def handleNodeData(peer: Peer, requestedHashes: Seq[HashType], nodeData: NodeData) = {
      val nodesKvStorage = blockchainStorages.nodesKeyValueStorageFor(Some(initialSyncState.targetBlock.number))

      if (nodeData.values.isEmpty) {
        log.debug(s"got empty mpt node response for known hashes switching to blockchain only: ${requestedHashes.map(h => Hex.toHexString(h.v.toArray[Byte]))}")
        markPeerBlockchainOnly(peer)
      }

      val receivedHashes = nodeData.values.map(v => ByteString(kec256(v.toArray[Byte])))
      val remainingHashes = requestedHashes.filterNot(h => receivedHashes.contains(h.v))
      if (remainingHashes.nonEmpty) {
        syncState = syncState.enqueueNodes(remainingHashes)
      }

      val hashesToRequest = (nodeData.values.indices zip receivedHashes) flatMap { case (idx, valueHash) =>
        requestedHashes.find(_.v == valueHash) map {
          case _: StateMptNodeHash =>
            handleMptNode(nodesKvStorage, nodeData.getMptNode(idx))

          case _: ContractStorageMptNodeHash =>
            handleContractMptNode(nodesKvStorage, nodeData.getMptNode(idx))

          case EvmCodeHash(hash) =>
            val evmCode = nodeData.values(idx)
            blockchain.save(hash, evmCode)
            Nil

          case StorageRootHash(_) =>
            val rootNode = nodeData.getMptNode(idx)
            handleContractMptNode(nodesKvStorage, rootNode)
        }
      }

      syncState = syncState
        .enqueueNodes(hashesToRequest.flatten)
        .copy(downloadedNodesCount = syncState.downloadedNodesCount + nodeData.values.size)

      processSyncing()
    }

    private def handleMptNode(nodesKvStorage: NodesKeyValueStorage, mptNode: MptNode): Seq[HashType] = mptNode match {
      case n: MptLeaf =>
        val evm = n.getAccount.codeHash
        val storage = n.getAccount.storageRoot
        nodesKvStorage.put(n.hash, n.toBytes)

        val evmRequests =
          if (evm != Account.EmptyCodeHash) Seq(EvmCodeHash(evm))
          else Nil

        val storageRequests =
          if (storage != Account.EmptyStorageRootHash) Seq(StorageRootHash(storage))
          else Nil

        evmRequests ++ storageRequests

      case n: MptBranch =>
        val hashes = n.children.collect { case Left(MptHash(childHash)) => childHash }.filter(_.nonEmpty)
        nodesKvStorage.put(n.hash, n.toBytes)
        hashes.map(StateMptNodeHash)

      case n: MptExtension =>
        nodesKvStorage.put(n.hash, n.toBytes)
        n.child.fold(
          mptHash => Seq(StateMptNodeHash(mptHash.hash)),
          _ => Nil)
    }

    private def handleContractMptNode(nodesKvStorage: NodesKeyValueStorage, mptNode: MptNode): Seq[HashType] = {
      mptNode match {
        case n: MptLeaf =>
          nodesKvStorage.put(n.hash, n.toBytes)
          Nil

        case n: MptBranch =>
          val hashes = n.children.collect { case Left(MptHash(childHash)) => childHash }.filter(_.nonEmpty)
          nodesKvStorage.put(n.hash, n.toBytes)
          hashes.map(ContractStorageMptNodeHash)

        case n: MptExtension =>
          nodesKvStorage.put(n.hash, n.toBytes)
          n.child.fold(
            mptHash => Seq(ContractStorageMptNodeHash(mptHash.hash)),
            _ => Nil)
      }
    }

    private def handleRequestFailure(peer: Peer, handler: ActorRef, reason: String) = {
      removeRequestHandler(handler)

      syncState = syncState
        .enqueueNodes(requestedMptNodes.getOrElse(handler, Nil))
        .enqueueNodes(requestedNonMptNodes.getOrElse(handler, Nil))
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
        mptNodesQueue = requestedMptNodes.values.flatten.toSeq.distinct ++ syncState.mptNodesQueue,
        nonMptNodesQueue = requestedNonMptNodes.values.flatten.toSeq.distinct ++ syncState.nonMptNodesQueue,
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
        syncState = syncState.enqueueBlockBodies(remainingBlockBodies)
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
        if (lastHeader.number > syncState.bestBlockHeaderNumber) {
          syncState = syncState.copy(bestBlockHeaderNumber = lastHeader.number)
        }
      }

      val blockHashes = blockHeadersObtained.map(_.hash)
      syncState = syncState
        .enqueueBlockBodies(blockHashes)
        .enqueueReceipts(blockHashes)
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
      if (syncState.nonMptNodesQueue.nonEmpty || syncState.mptNodesQueue.nonEmpty) {
        requestNodes(peer)
      } else {
        assignBlockChainWork(peer)
      }
    }

    def assignBlockChainWork(peer: Peer): Unit = {
      if (syncState.receiptsQueue.nonEmpty) {
        requestReceipts(peer)
      } else if (syncState.blockBodiesQueue.nonEmpty) {
        requestBlockBodies(peer)
      } else if (context.child(blockHeadersHandlerName).isEmpty &&
        initialSyncState.targetBlock.number > syncState.bestBlockHeaderNumber) {
        requestBlockHeaders(peer)
      }
    }

    def requestReceipts(peer: Peer): Unit = {
      val (receiptsToGet, remainingReceipts) = syncState.receiptsQueue.splitAt(receiptsPerRequest)

      val handler = context.actorOf(
        PeerRequestHandler.props[GetReceipts, Receipts](
          peer, etcPeerManager, peerEventBus,
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
          peer, etcPeerManager, peerEventBus,
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
          peer, etcPeerManager, peerEventBus,
          requestMsg = GetBlockHeaders(Left(syncState.bestBlockHeaderNumber + 1), limit, skip = 0, reverse = false),
          responseMsgCode = BlockHeaders.code), blockHeadersHandlerName)

      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
    }

    def requestNodes(peer: Peer): Unit = {
      val (nonMptNodesToGet, remainingNonMptNodes) = syncState.nonMptNodesQueue.splitAt(nodesPerRequest)
      val (mptNodesToGet, remainingMptNodes) = syncState.mptNodesQueue.splitAt(nodesPerRequest - nonMptNodesToGet.size)
      val nodesToGet = nonMptNodesToGet ++ mptNodesToGet

      val handler = context.actorOf(
        PeerRequestHandler.props[GetNodeData, NodeData](
          peer, etcPeerManager, peerEventBus,
          requestMsg = GetNodeData(nodesToGet.map(_.v)),
          responseMsgCode = NodeData.code))

      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
      syncState = syncState.copy(
        nonMptNodesQueue = remainingNonMptNodes,
        mptNodesQueue = remainingMptNodes)
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
    bestBlockHeaderNumber: BigInt = 0) {

    def enqueueBlockBodies(blockBodies: Seq[ByteString]): SyncState =
      copy(blockBodiesQueue = blockBodiesQueue ++ blockBodies)

    def enqueueReceipts(receipts: Seq[ByteString]): SyncState =
      copy(receiptsQueue = receiptsQueue ++ receipts)

    def enqueueNodes(hashes: Seq[HashType]): SyncState = {
      val (mpt, nonMpt) = hashes.partition {
        case _: StateMptNodeHash | _: ContractStorageMptNodeHash => true
        case _: EvmCodeHash | _: StorageRootHash => false
      }
      copy(
        mptNodesQueue = mptNodesQueue ++ mpt,
        nonMptNodesQueue = nonMptNodesQueue ++ nonMpt)
    }

    def anythingQueued: Boolean =
      nonMptNodesQueue.nonEmpty ||
        mptNodesQueue.nonEmpty ||
        blockBodiesQueue.nonEmpty ||
        receiptsQueue.nonEmpty

    val totalNodesCount: Int = downloadedNodesCount + mptNodesQueue.size + nonMptNodesQueue.size
  }

  sealed trait HashType {
    def v: ByteString
  }
  case class StateMptNodeHash(v: ByteString) extends HashType
  case class ContractStorageMptNodeHash(v: ByteString) extends HashType
  case class EvmCodeHash(v: ByteString) extends HashType
  case class StorageRootHash(v: ByteString) extends HashType

  sealed trait ReceiptsValidationResult
  object ReceiptsValidationResult {
    case class ReceiptsValid(blockHashesAndReceipts: Seq[(ByteString, Seq[Receipt])]) extends ReceiptsValidationResult
    case class ReceiptsInvalid(error: BlockError) extends ReceiptsValidationResult
    case object ReceiptsDbError extends ReceiptsValidationResult
  }
}
