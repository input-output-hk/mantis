package io.iohk.ethereum.blockchain.sync

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{ PeerSelector, Subscribe }
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{ GetNodeData, NodeData }
import io.iohk.ethereum.ommers.OmmersPool.{ AddOmmers, RemoveOmmers }
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.{ AddTransactions, RemoveTransactions }
import io.iohk.ethereum.utils.Config.SyncConfig
import org.bouncycastle.util.encoders.Hex

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success, Try }

// scalastyle:off number.of.methods
class RegularSync(
  val appStateStorage: AppStateStorage,
  val etcPeerManager: ActorRef,
  val peerEventBus: ActorRef,
  val ommersPool: ActorRef,
  val pendingTransactionsManager: ActorRef,
  val broadcaster: BlockBroadcast,
  val ledger: Ledger,
  val blockchain: Blockchain,
  val syncConfig: SyncConfig,
  implicit val scheduler: Scheduler
) extends Actor with ActorLogging with PeerListSupport with BlacklistSupport {

  import RegularSync._
  import syncConfig._

  scheduler.schedule(printStatusInterval, printStatusInterval, self, PrintStatus)

  peerEventBus ! Subscribe(MessageClassifier(Set(NewBlock.code, NewBlockHashes.code), PeerSelector.AllPeers))

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  override def receive: Receive = idle

  def idle: Receive = handleCommonMessages orElse {
    case Start =>
      log.info("Starting block synchronization")
      appStateStorage.fastSyncDone()
      askForHeaders(RegularSyncState())

    case StartIdle =>
      appStateStorage.fastSyncDone()
      context become running(RegularSyncState())
  }

  def running(state: RegularSyncState): Receive =
    handleBasicMessages(state) orElse
    handleAdditionalMessages(state) orElse
    handleResumingAndPrinting(state)

  def handleBasicMessages(state: RegularSyncState): Receive =
    handleCommonMessages orElse
    handleResponseToRequest(state)

  def handleAdditionalMessages(state: RegularSyncState): Receive =
    handleNewBlockMessages(state) orElse
    handleMinedBlock(notDownloading(state)) orElse
    handleNewBlockHashesMessages(state)

  def handleResumingAndPrinting(state: RegularSyncState): Receive = {
    case ResumeRegularSync =>
      resumeRegularSync(state)

    case PrintStatus =>
      log.info(s"Block: ${blockchain.getBestBlockNumber()}. Peers: ${handshakedPeers.size} (${blacklistedPeers.size} blacklisted)")
  }

  private def resumeRegularSync(state: RegularSyncState): Unit = {
    cancelScheduledResume(state)

    // The case that waitingForActor is defined (we are waiting for some response),
    // can happen when we are on top of the chain and currently handling newBlockHashes message
    if (state.notWaitingForAnActor) {
      if (state.notMissingNode) {
        val syncState = state.withHeadersQueue(Seq.empty).withResolvingBranches(false)
        context become running(syncState)
        askForHeaders(syncState)
      } else {
        val nodeId = state.missingStateNodeRetry.get.nodeId
        requestMissingNode(nodeId, state)
      }
    } else {
      scheduleResume(state)
    }
  }

  private def scheduleResume(state: RegularSyncState): Unit = {
    val newTimeout = Some(scheduler.scheduleOnce(checkForNewBlockInterval, self, ResumeRegularSync))
    context become running(state.withResumeRegularSyncTimeout(newTimeout))
  }

  private def cancelScheduledResume(state: RegularSyncState): Unit = {
    state.resumeRegularSyncTimeout.foreach(_.cancel)
    context become running(state.withResumeRegularSyncTimeout(None))
  }

  // scalastyle:off method.length
  def handleNewBlockMessages(state: RegularSyncState): Receive = {
    case MessageFromPeer(NewBlock(newBlock, _), peerId) =>
      //we allow inclusion of new block only if we are not syncing
      if (notDownloading(state) && state.topOfTheChain) {
        log.debug(s"Handling NewBlock message for block (${newBlock.idTag})")
        val importResult = Try(ledger.importBlock(newBlock))

        importResult match {
          case Success(result) =>
            lazy val headerHash = hash2string(newBlock.header.hash)
            lazy val newNumber = newBlock.header.number

            result match {
              case BlockImportedToTop(newBlocks, newTds) =>
                broadcastBlocks(newBlocks, newTds)
                updateTxAndOmmerPools(newBlocks, Seq.empty)
                log.info(s"Added new block $newNumber to the top of the chain received from $peerId")

              case BlockEnqueued =>
                ommersPool ! AddOmmers(newBlock.header)
                log.debug(s"Block $newNumber ($headerHash) from $peerId added to queue")

              case DuplicateBlock =>
                log.debug(s"Ignoring duplicate block $newNumber ($headerHash) from $peerId")

              case UnknownParent =>
                // This is normal when receiving broadcast blocks
                log.debug(s"Ignoring orphaned block $newNumber ($headerHash) from $peerId")

              case ChainReorganised(oldBranch, newBranch, totalDifficulties) =>
                updateTxAndOmmerPools(newBranch, oldBranch)
                broadcastBlocks(newBranch, totalDifficulties)
                val header = newBranch.last.header
                log.debug(s"Imported block $newNumber ($headerHash) from $peerId, " +
                  s"resulting in chain reorganisation: new branch of length ${newBranch.size} with head at block " +
                  s"${header.number} (${hash2string(header.hash)})")

              case BlockImportFailed(error) =>
                blacklist(peerId, blacklistDuration, error)
            }

          case Failure(missingNodeEx: MissingNodeException) if syncConfig.redownloadMissingStateNodes =>
            // state node re-download will be handled when downloading headers
            log.error("Ignoring broadcast block, reason: {}", missingNodeEx)

          case Failure(ex) =>
            throw ex
        }
      }
  }

  private def hash2string(hash: ByteString): String = Hex.toHexString(hash.toArray[Byte])

  /** Handles NewHashesMessage, should only cover this message when we are top of the chain */
  def handleNewBlockHashesMessages(state: RegularSyncState): Receive = {
    case MessageFromPeer(NewBlockHashes(hashes), peerId) =>
      val possiblePeer = peersToDownloadFrom.find { case (peer, _) => peer.id == peerId }
      // we allow asking for new hashes when we are not syncing and we can download from specified peer,
      // we are on top of the chain and not resolving branches currently
      if (notDownloading(state) && state.topOfTheChain && possiblePeer.isDefined) {
        log.debug("Handling NewBlockHashes message: \n" + hashes.mkString("\n"))
        val (peer, _) = possiblePeer.get
        val hashesToCheck = hashes.take(syncConfig.maxNewHashes)

        if (!containsAncientBlockHash(hashesToCheck)) {
          val filteredHashes = getValidHashes(hashesToCheck)

          if (filteredHashes.nonEmpty) {
            val headers = GetBlockHeaders(Right(filteredHashes.head.hash), filteredHashes.length, BigInt(0), reverse = false)
            val request = requestBlockHeaders(peer, headers)
            cancelScheduledResume(state.withWaitingForActor(request))
          } else {
            log.debug("All received hashes all already in Chain or Queue")
          }
        } else {
          blacklist(peerId, blacklistDuration, "received ancient blockHash")
        }
      }
  }

  /** Filters hashes that already in the chain or queue. Only adds not known blocks */
  private def getValidHashes(unfilteredHashes: Seq[BlockHash]): Seq[BlockHash] = {
    unfilteredHashes.foldLeft(Seq.empty[BlockHash]){ case (hashesList, bh @ BlockHash(blockHash, blockNumber)) =>
      val hash = hash2string(blockHash)

      ledger.checkBlockStatus(blockHash) match {
        case InChain =>
          log.debug(s"BlockHash with Number: $blockNumber and Hash: $hash already in chain")
          hashesList

        case Queued =>
          log.debug(s"BlockHash with Number: $blockNumber and Hash: $hash already in queue")
          hashesList

        case UnknownBlock =>
          log.debug(s"Preparing to download unknown block with Number: $blockNumber and Hash: $hash")
          hashesList :+ bh
      }
    }
  }

  private def containsAncientBlockHash(hashes: Seq[BlockHash]): Boolean = {
    val currentBestBlock = blockchain.getBestBlockNumber()
    hashes.exists(blockHash => ancientBlockHash(blockHash.number, currentBestBlock))
  }

  private def ancientBlockHash(blockNumber: BigInt, currentBestBlockNumber: BigInt): Boolean =
    (currentBestBlockNumber > blockNumber) && (currentBestBlockNumber - blockNumber > syncConfig.maxNewBlockHashAge)

  def handleResponseToRequest(state: RegularSyncState): Receive = {
    case ResponseReceived(peer, BlockHeaders(headers), timeTaken) =>
      val resolvingBranches = state.resolvingBranches
      val syncState = state.withWaitingForActor(None)
      context become running(syncState)
      log.debug("Received {} block headers in {} ms from {} (branch resolution: {})", headers.size, timeTaken, peer, resolvingBranches)
      if (resolvingBranches) {
        handleBlockBranchResolution(peer, headers.reverse, syncState)
      } else {
        handleBlockHeaders(peer, headers, syncState)
      }

    case ResponseReceived(peer, BlockBodies(blockBodies), timeTaken) /*if !state.hasEmptyHeadersQueue*/ =>
      log.debug("Received {} block bodies in {} ms", blockBodies.size, timeTaken)
      context become running(state.withWaitingForActor(None))
      handleBlockBodies(peer, blockBodies, state.withWaitingForActor(None))

    case ResponseReceived(peer, NodeData(nodes), timeTaken) if !state.notMissingNode =>
      log.debug("Received {} missing state nodes in {} ms", nodes.size, timeTaken)
      context become running(state.withWaitingForActor(None))
      handleReDownloadedStateNodes(peer, nodes, state.withWaitingForActor(None))

    case PeerRequestHandler.RequestFailed(peer, reason) if state.waitingForAnActor.contains(sender()) =>
      log.debug(s"Request to peer ({}) failed: {}", peer, reason)
      if (handshakedPeers.contains(peer)) blacklist(peer.id, blacklistDuration, reason)

      scheduleResume(state.withWaitingForActor(None))
  }

  def handleMinedBlock(notBusyDownloading: Boolean): Receive = {
    // todo improve mined block handling - add info that block was not included because of syncing [EC-250]
    // we allow inclusion of mined block only if we are not syncing / reorganising chain
    case MinedBlock(block) =>
      val header = block.header
      if (notBusyDownloading) {
        val importResult = Try(ledger.importBlock(block))

        importResult match {
          case Success(result) =>
            lazy val blockNumber = header.number

            result match {
              case BlockImportedToTop(blocks, totalDifficulties) =>
                log.debug(s"Added new mined block $blockNumber to top of the chain")
                broadcastBlocks(blocks, totalDifficulties)
                updateTxAndOmmerPools(blocks, Seq.empty)

              case ChainReorganised(oldBranch, newBranch, totalDifficulties) =>
                log.debug(s"Added new mined block $blockNumber resulting in chain reorganization")
                broadcastBlocks(newBranch, totalDifficulties)
                updateTxAndOmmerPools(newBranch, oldBranch)

              case DuplicateBlock =>
                log.warning("Mined block is a duplicate, this should never happen")

              case BlockEnqueued =>
                log.debug(s"Mined block $blockNumber was added to the queue")
                ommersPool ! AddOmmers(header)

              case UnknownParent =>
                log.warning("Mined block has no parent on the main chain")

              case BlockImportFailed(err) =>
                log.warning(s"Failed to execute mined block because of $err")
            }

          case Failure(missingNodeEx: MissingNodeException) if syncConfig.redownloadMissingStateNodes =>
            log.error(missingNodeEx, "Ignoring mined block")

          case Failure(ex) =>
            throw ex
        }
      } else {
        ommersPool ! AddOmmers(header)
      }
  }

  private def askForHeaders(state: RegularSyncState): Unit = {
    bestPeer match {
      case Some(peer) =>
        val blockNumber = blockchain.getBestBlockNumber() + 1
        log.debug(s"Requesting $blockHeadersPerRequest headers, starting from $blockNumber")
        val headers = GetBlockHeaders(Left(blockNumber), blockHeadersPerRequest, skip = 0, reverse = false)
        val request = requestBlockHeaders(peer, headers)
        context become running(state.withWaitingForActor(request))

      case None =>
        log.debug("No peers to download from")
        scheduleResume(state)
    }
  }

  private def requestMissingNode(nodeId: ByteString, state: RegularSyncState): Unit = {
    bestPeer match {
      case Some(peer) =>
        log.debug(s"Requesting node data for node with id: $nodeId, from peer: $peer")
        val request = requestNodeData(peer, nodeId)
        context become running(state.withWaitingForActor(request))

      case None =>
        log.debug("Requesting missing state nodes: no peers to download from")
        scheduleResume(state)
    }
  }

  private def handleReDownloadedStateNodes(peer: Peer, nodes: Seq[ByteString], state: RegularSyncState): Unit = {
    if (nodes.isEmpty) {
      log.debug(s"Did not receive missing state node from peer ($peer)")
      val reason = "did not receive missing state node"
      resumeWithDifferentPeer(peer, reason, state)
    } else {
      val MissingStateNodeRetry(requestedHash, blockPeer, blocksToRetry) = state.missingStateNodeRetry.get
      val receivedHash = kec256(nodes.head)

      if (receivedHash != requestedHash) {
        log.debug("Received missing state node has different hash than requested")
        val reason = "wrong state node hash"
        resumeWithDifferentPeer(peer, reason, state)
      } else {
        val syncState = state.withMissingStateNodeRetry(None)
        context become running(syncState)
        val nextBlockNumber = blocksToRetry.head.header.number
        // note that we do not analyse whether the node is a leaf, extension or a branch, thus we only
        // handle one state node at a time and retry executing block - this may require multiple attempts
        blockchain.saveNode(requestedHash, nodes.head.toArray, nextBlockNumber)
        log.info(s"Inserted missing state node: ${hash2string(requestedHash)}. Retrying execution starting with block $nextBlockNumber")
        handleBlocks(blockPeer, blocksToRetry, syncState)
      }
    }
  }

  private def handleBlockBranchResolution(peer: Peer, message: Seq[BlockHeader], state: RegularSyncState): Unit = {
    val syncState = state.withResolvingBranches(false)
    val headersQueue = state.headersQueue
    if (message.nonEmpty && message.last.hash == headersQueue.head.parentHash) {
      val blockHeaders = message ++ headersQueue
      context become running(syncState.withHeadersQueue(blockHeaders))
      processBlockHeaders(peer, blockHeaders, syncState.withHeadersQueue(blockHeaders))
    } else {
      // we did not get previous blocks, there is no way to resolve, blacklist peer and continue download
      val reason = "failed to resolve branch"
      resumeWithDifferentPeer(peer, reason, syncState)
    }

    context become running(syncState)
  }

  private def handleBlockHeaders(peer: Peer, message: Seq[BlockHeader], state: RegularSyncState): Unit = {
    if (message.nonEmpty) {
      context become running(state.withHeadersQueue(message))
      processBlockHeaders(peer, message, state.withHeadersQueue(message))
    } else {
      // no new headers to process, schedule to ask again in future, we are at the top of chain
      scheduleResume(state.withTopOfTheChain(true))
    }
  }

  private def processBlockHeaders(peer: Peer, headers: Seq[BlockHeader], state: RegularSyncState): Unit = {
    lazy val headersQueue = state.headersQueue

    ledger.resolveBranch(headers) match {
      case NewBetterBranch(oldBranch) =>
        val transactionsToAdd = oldBranch.flatMap(_.body.transactionList).toSet
        pendingTransactionsManager ! PendingTransactionsManager.AddTransactions(transactionsToAdd)
        val request = requestBlockBodies(peer, headers)
        context become running(state.withWaitingForActor(request))

        // add first block from branch as ommer
        oldBranch.headOption.foreach{ h => ommersPool ! AddOmmers(h.header) }

      case NoChainSwitch =>
        // add first block from branch as ommer
        headers.headOption.foreach{ h => ommersPool ! AddOmmers(h) }

        scheduleResume(state)

      case UnknownBranch =>
        if (state.resolvingBranches) {
          log.debug("Fail to resolve branch, branch too long, it may indicate malicious peer")
          val reason = "failed to resolve branch"
          resumeWithDifferentPeer(peer, reason, state)
        } else {
          val parentHash = hash2string(headersQueue.head.parentHash)
          log.debug(s"Requesting $branchResolutionRequestSize additional headers for branch resolution, starting from: " + parentHash)
          val headers = GetBlockHeaders(Right(headersQueue.head.parentHash), branchResolutionRequestSize, skip = 0, reverse = true)
          val request = requestBlockHeaders(peer, headers)
          val syncState = state.withWaitingForActor(request).withResolvingBranches(true)
          context become running(syncState)
        }

      case InvalidBranch =>
        log.debug("Got block header that does not have parent")
        val reason = "error in response"
        resumeWithDifferentPeer(peer, reason, state)
    }
  }

  private def requestBlockHeaders(peer: Peer, msg: GetBlockHeaders): Option[ActorRef] = {
    Some(context.actorOf(PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
      peer,
      peerResponseTimeout,
      etcPeerManager,
      peerEventBus,
      requestMsg = msg,
      responseMsgCode = BlockHeaders.code
    )))
  }

  private def requestBlockBodies(peer: Peer, headers: Seq[BlockHeader]): Option[ActorRef] = {
    val hashes = headers.take(blockBodiesPerRequest).map(_.hash)
    Some(context.actorOf(PeerRequestHandler.props[GetBlockBodies, BlockBodies](
      peer,
      peerResponseTimeout,
      etcPeerManager,
      peerEventBus,
      requestMsg = GetBlockBodies(hashes),
      responseMsgCode = BlockBodies.code
    )))
  }

  private def requestNodeData(peer: Peer, nodeId: ByteString): Option[ActorRef] = {
    Some(context.actorOf(PeerRequestHandler.props[GetNodeData, NodeData](
      peer,
      peerResponseTimeout,
      etcPeerManager,
      peerEventBus,
      requestMsg = GetNodeData(List(nodeId)),
      responseMsgCode = NodeData.code
    )))
  }

  private def handleBlockBodies(peer: Peer, blockBodies: Seq[BlockBody], state: RegularSyncState): Unit = {
    if (blockBodies.nonEmpty) {
      if (state.hasEmptyHeadersQueue){
        log.debug("Handling block bodies while not having any queued headers")
      } else {
        val blocks = state.headersQueue.zip(blockBodies).map{ case (header, body) => Block(header, body) }
        handleBlocks(peer, blocks, state)
      }
    } else {
      val reason = "received empty block bodies"
      resumeWithDifferentPeer(peer, reason, state)
    }
  }

  private def handleBlocks(peer: Peer, blocks: Seq[Block], state: RegularSyncState): Unit = {
    val (importedBlocks, errorOpt) = importBlocks(blocks.toList)

    if (importedBlocks.nonEmpty) {
      val lastHeader = importedBlocks.last.header
      log.debug(s"Got new blocks up till block: ${lastHeader.number} with hash ${hash2string(lastHeader.hash)}")
    }

    lazy val headers = state.headersQueue.drop(blocks.length)

    errorOpt match {
      case Some(missingNodeEx: MissingNodeException) =>
        // Missing state node recovery mechanism.
        // All block headers that were about to be imported are removed from the queue and we save the full blocks
        // that weren't imported yet - this will avoid re-downloading block headers and bodies in between
        // (possibly multiple) state node requests.
        log.error(missingNodeEx, "Requesting missing state nodes")
        val blocksToRetry = blocks.drop(importedBlocks.length)
        val newMissingStateNodeRetry = Some(MissingStateNodeRetry(missingNodeEx.hash, peer, blocksToRetry))
        val syncState = state.withHeadersQueue(headers).withMissingStateNodeRetry(newMissingStateNodeRetry)
        context become running(syncState)
        requestMissingNode(missingNodeEx.hash, syncState)

      case Some(error) =>
        val reason = s"a block execution error: ${error.toString}"
        resumeWithDifferentPeer(peer, reason, state)

      case None =>
        val syncState = state.withHeadersQueue(headers)
        if (headers.nonEmpty) {
          val request = requestBlockBodies(peer, headers)
          context become running(syncState.withWaitingForActor(request))
        } else {
          context become running(syncState)
          self ! ResumeRegularSync
        }
    }
  }

  @tailrec
  private def importBlocks(blocks: List[Block], importedBlocks: List[Block] = Nil): (List[Block], Option[Any]) =
    blocks match {
      case Nil =>
        (importedBlocks, None)

      case block :: tail =>
        Try(ledger.importBlock(block)) match {
          case Success(result) =>
            result match {
              case BlockImportedToTop(_, _) =>
                importBlocks(tail, block :: importedBlocks)

              case ChainReorganised(_, newBranch, _) =>
                importBlocks(tail, newBranch.reverse ::: importedBlocks)

              case (DuplicateBlock | BlockEnqueued) =>
                importBlocks(tail, importedBlocks)

              case err@(UnknownParent | BlockImportFailed(_)) =>
                (importedBlocks, Some(err))
            }

          // return this exception as a result only when the recovery mechanism is turned on in config
          case Failure(missingNodeEx: MissingNodeException) if syncConfig.redownloadMissingStateNodes =>
            (importedBlocks, Some(missingNodeEx))

          case Failure(ex) =>
            throw ex
        }
    }

  private def resumeWithDifferentPeer(currentPeer: Peer, reason: String, state: RegularSyncState): Unit = {
    blacklist(currentPeer.id, blacklistDuration, reason)
    context become running(state.withHeadersQueue(Seq.empty))
    self ! ResumeRegularSync
  }

  private def bestPeer: Option[Peer] = {
    val peersToUse = peersToDownloadFrom
      .collect{ case (ref, PeerInfo(_, totalDifficulty, true, _)) => (ref, totalDifficulty) }

    if (peersToUse.nonEmpty) {
      val (peer, _) = peersToUse.maxBy{ case (_, td) => td }
      Some(peer)
    } else {
      None
    }
  }

  private def updateTxAndOmmerPools(blocksAdded: Seq[Block], blocksRemoved: Seq[Block]): Unit = {
    blocksRemoved.headOption.foreach(block => ommersPool ! AddOmmers(block.header))
    blocksRemoved.foreach(block => pendingTransactionsManager ! AddTransactions(block.body.transactionList.toSet))

    blocksAdded.foreach { block =>
      ommersPool ! RemoveOmmers(block.header :: block.body.uncleNodesList.toList)
      pendingTransactionsManager ! RemoveTransactions(block.body.transactionList)
    }
  }

  private def broadcastBlocks(blocks: Seq[Block], totalDifficulties: Seq[BigInt]): Unit = {
    blocks.zip(totalDifficulties).foreach { case (block, td) =>
      broadcaster.broadcastBlock(NewBlock(block, td), handshakedPeers)
    }
  }

  private def notDownloading(state: RegularSyncState): Boolean = {
    state.hasEmptyHeadersQueue && state.notWaitingForAnActor && state.notResolvingBranches
  }

}

object RegularSync {
  // scalastyle:off parameter.number
  def props(
    appStateStorage: AppStateStorage,
    etcPeerManager: ActorRef,
    peerEventBus: ActorRef,
    ommersPool: ActorRef,
    pendingTransactionsManager: ActorRef,
    broadcaster: BlockBroadcast,
    ledger: Ledger,
    blockchain: Blockchain,
    syncConfig: SyncConfig,
    scheduler: Scheduler
  ): Props = Props(new RegularSync(
    appStateStorage,
    etcPeerManager,
    peerEventBus,
    ommersPool,
    pendingTransactionsManager,
    broadcaster,
    ledger,
    blockchain,
    syncConfig,
    scheduler
  ))

  private[sync] case object ResumeRegularSync

  private case class ResolveBranch(peer: ActorRef)

  private[sync] case object PrintStatus

  case object Start

  /** This start the actor without asking for headers, currently only used in tests */
  case object StartIdle

  case class MinedBlock(block: Block)

  case class MissingStateNodeRetry(nodeId: ByteString, p: Peer, blocksToRetry: Seq[Block])

/** Stores all state changes during regular synchronisation.
  *
  * @param waitingForAnActor        when on top of the chain and handling newBlockHashes message
  * @param headersQueue             headers queue
  * @param topOfTheChain            is used as an optimisation to avoid handling broadcast messages when we haven't reached the top of the chain.
  *                                 Currently it's set to true after we receive 0 headers from a peer, which usually means it doesn't have any new headers.
  *                                 But there could be other reasons for receiving 0 blocks. It may be better to make handling broadcast messages
  *                                 dependent on our current best block info (stored in this actor to avoid DB look-ups).
  * @param resolvingBranches        defines if branch is being resolved or not
  * @param resumeRegularSyncTimeout schedule if regular synchronization should be resumed by calling self with
  *                                 `ResumeRegularSync` message or cancelled by calling cancel
  * @param missingStateNodeRetry    state node that is missing
  */
  case class RegularSyncState(
    waitingForAnActor: Option[ActorRef] = None,
    headersQueue: Seq[BlockHeader] = Seq.empty,
    topOfTheChain: Boolean = false,
    resolvingBranches: Boolean = false,
    resumeRegularSyncTimeout: Option[Cancellable] = None,
    missingStateNodeRetry: Option[MissingStateNodeRetry] = None
  ) {

    def withWaitingForActor(actorRef: Option[ActorRef]): RegularSyncState = copy(waitingForAnActor = actorRef)

    def withHeadersQueue(headers: Seq[BlockHeader]): RegularSyncState = copy(headersQueue = headers)

    def withTopOfTheChain(top: Boolean): RegularSyncState = copy(topOfTheChain = top)

    def withResolvingBranches(resolving: Boolean): RegularSyncState = copy(resolvingBranches = resolving)

    def withResumeRegularSyncTimeout(resume: Option[Cancellable]): RegularSyncState = copy(resumeRegularSyncTimeout = resume)

    def withMissingStateNodeRetry(node: Option[MissingStateNodeRetry]): RegularSyncState = copy(missingStateNodeRetry = node)

    def notWaitingForAnActor: Boolean = waitingForAnActor.isEmpty

    def hasEmptyHeadersQueue: Boolean = headersQueue.isEmpty

    def notResolvingBranches: Boolean = !resolvingBranches

    def notMissingNode: Boolean = missingStateNodeRetry.isEmpty

  }
}
