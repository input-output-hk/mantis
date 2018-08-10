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
  implicit val scheduler: Scheduler)
  extends Actor with ActorLogging with PeerListSupport with BlacklistSupport {

  import RegularSync._
  import syncConfig._

  scheduler.schedule(printStatusInterval, printStatusInterval, self, PrintStatus)

  peerEventBus ! Subscribe(MessageClassifier(Set(NewBlock.code, NewBlockHashes.code), PeerSelector.AllPeers))

  override def receive: Receive = idle

  def idle: Receive = handleCommonMessages orElse {
    case Start =>
      log.info("Starting block synchronization")
      appStateStorage.fastSyncDone()
      context become running(None, Seq.empty, topOfTheChain = false, resolvingBranches = false, None, None)
      askForHeaders(None, Seq.empty, topOfTheChain = false, resolvingBranches = false, None, None)

    case StartIdle =>
      appStateStorage.fastSyncDone()
      context become running(None, Seq.empty, topOfTheChain = false, resolvingBranches = false, None, None)
  }

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  /**
    * @param waitingForActor when on top of the chain and handling newBlockHashes message
    * @param topOfTheChain   is used as an optimisation to avoid handling broadcast messages when we haven't reached the top of the chain.
    *                        Currently it's set to true after we receive 0 headers from a peer, which usually means it doesn't have any new headers.
    *                        But there could be other reasons for receiving 0 blocks. It may be better to make handling broadcast messages
    *                        dependent on our current best block info (stored in this actor to avoid DB lookups).
    */
  def running(
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Receive = handleBasicMessages(waitingForActor, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry) orElse
    handleAdditionalMessages(waitingForActor, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry) orElse
    handleResumingAndPrinting(waitingForActor, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)

  def handleBasicMessages(
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Receive = handleCommonMessages orElse
    handleResponseToRequest(waitingForActor, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)

  def handleResumingAndPrinting(
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Receive = {
    case ResumeRegularSync =>
      resumeRegularSync(waitingForActor, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)

    case PrintStatus =>
      log.info(s"Block: ${blockchain.getBestBlockNumber()}. Peers: ${handshakedPeers.size} (${blacklistedPeers.size} blacklisted)")
  }

  def handleAdditionalMessages(
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Receive = handleNewBlockMessages(waitingForActor, headersQueue, topOfTheChain, resolvingBranches) orElse
    handleMinedBlock(waitingForActor, headersQueue, resolvingBranches) orElse
    handleNewBlockHashesMessages(waitingForActor, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)

  private def resumeRegularSync(
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Unit = {
    cancelScheduledResume(waitingForActor, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)

    // The case that waitingForActor is defined (we are waiting for some response),
    // can happen when we are on top of the chain and currently handling newBlockHashes message

    if (waitingForActor.isEmpty) {
      if (missingStateNodeRetry.isEmpty) {
        // resumeRegularSyncTimeout is None because of cancelScheduleResume
        askForHeaders(waitingForActor, Seq.empty, topOfTheChain, resolvingBranches = false, None, missingStateNodeRetry)
      } else {
        val nodeId = missingStateNodeRetry.get.nodeId
        // resumeRegularSyncTimeout is None because of cancelScheduleResume
        requestMissingNode(nodeId, waitingForActor, headersQueue, topOfTheChain, resolvingBranches, None, missingStateNodeRetry)
      }
    } else {
      val newTimeout = Some(scheduler.scheduleOnce(checkForNewBlockInterval, self, ResumeRegularSync))
      context become running(waitingForActor, headersQueue, topOfTheChain, resolvingBranches, newTimeout, missingStateNodeRetry)
    }
  }

  private def scheduleResume(
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Unit = {
    val newTimeout = Some(scheduler.scheduleOnce(checkForNewBlockInterval, self, ResumeRegularSync))
    context become running(waitingForActor, headersQueue, topOfTheChain, resolvingBranches, newTimeout, missingStateNodeRetry)
  }

  private def cancelScheduledResume(
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Unit = {
    resumeRegularSyncTimeout.foreach(_.cancel)
    context become running(waitingForActor, headersQueue, topOfTheChain, resolvingBranches, None, missingStateNodeRetry)
  }

  // scalastyle:off method.length
  def handleNewBlockMessages(
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean
  ): Receive = {
    case MessageFromPeer(NewBlock(newBlock, _), peerId) =>
      //we allow inclusion of new block only if we are not syncing
      if (notDownloading(waitingForActor, headersQueue, resolvingBranches) && topOfTheChain) {
        log.debug(s"Handling NewBlock message for block (${ newBlock.idTag })")
        val importResult = Try(ledger.importBlock(newBlock))

        importResult match {
          case Success(result) =>
            val headerHash = hash2string(newBlock.header.hash)
            val newNumber = newBlock.header.number
            result match {
              case BlockImportedToTop(newBlocks, newTds) =>
                broadcastBlocks(newBlocks, newTds)
                updateTxAndOmmerPools(newBlocks, Nil)
                log.info(s"Added new block $newNumber to the top of the chain received from $peerId")

              case BlockEnqueued =>
                ommersPool ! AddOmmers(newBlock.header)
                log.debug(s"Block $newNumber ($headerHash) from $peerId added to queue")

              case DuplicateBlock =>
                log.debug(s"Ignoring duplicate block $newNumber ($headerHash) from $peerId")

              case UnknownParent =>
                // This is normal when receiving broadcasted blocks
                log.debug(s"Ignoring orphaned block $newNumber ($headerHash) from $peerId")

              case ChainReorganised(oldBranch, newBranch, totalDifficulties) =>
                updateTxAndOmmerPools(newBranch, oldBranch)
                broadcastBlocks(newBranch, totalDifficulties)
                log.debug(s"Imported block $newNumber ($headerHash) from $peerId, " +
                  s"resulting in chain reorganisation: new branch of length ${newBranch.size} with head at block " +
                  s"${newBranch.last.header.number} (${hash2string(newBranch.last.header.hash)})")

              case BlockImportFailed(error) =>
                blacklist(peerId, blacklistDuration, error)
            }

          case Failure(missingNodeEx: MissingNodeException) if syncConfig.redownloadMissingStateNodes =>
            // state node re-download will be handled when downloading headers
            log.error("Ignoring broadcast block {}", missingNodeEx)

          case Failure(ex) =>
            throw ex
        }
      }
  }

  private def hash2string(hash: ByteString): String = Hex.toHexString(hash.toArray[Byte])

  /** Handles NewHashesMessage, should only cover this message when we are top of the chain */
  def handleNewBlockHashesMessages(
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Receive = {
    case MessageFromPeer(NewBlockHashes(hashes), peerId) =>
      val maybePeer = peersToDownloadFrom.find(peer => peer._1.id == peerId)
      // we allow asking for new hashes when we are not syncing and we can download from specified peer,
      // we are on top of the chain and not resolving branches currently
      if (notDownloading(waitingForActor, headersQueue, resolvingBranches) && topOfTheChain && maybePeer.isDefined) {
        log.debug("Handling NewBlockHashes message: \n" + hashes.mkString("\n"))
        val (peer, _) = maybePeer.get
        val hashesToCheck = hashes.take(syncConfig.maxNewHashes)

        if (!containsAncientBlockHash(hashesToCheck)) {
          val filteredHashes = getValidHashes(hashesToCheck)

          if (filteredHashes.nonEmpty) {
            val headers = GetBlockHeaders(Right(filteredHashes.head.hash), filteredHashes.length, BigInt(0), reverse = false)
            val request = requestBlockHeaders(peer, headers)
            cancelScheduledResume(request, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
          } else {
            log.debug("All received hashes all already in Chain or Queue")
          }
        } else {
          blacklist(peerId, blacklistDuration, "received ancient blockHash")
        }
      }
  }

  private def getValidHashes(unfilteredHashes: Seq[BlockHash]): Seq[BlockHash] = {
    unfilteredHashes.foldLeft(Seq.empty[BlockHash]){ (hashesList, blockHash) =>
      val blockNumber = blockHash.number
      val hash = hash2string(blockHash.hash)
      ledger.checkBlockStatus(blockHash.hash) match {
        case InChain =>
          log.debug(s"BlockHash with Number: $blockNumber and Hash: $hash already in chain")
          hashesList
        case Queued =>
          log.debug(s"BlockHash with Number: $blockNumber and Hash: $hash already in queue")
          hashesList
        case UnknownBlock =>
          log.debug(s"Preparing to download unknown block with Number: $blockNumber and Hash: $hash")
          hashesList :+ blockHash
      }
    }
  }

  private def containsAncientBlockHash(hashes: Seq[BlockHash]): Boolean = {
    val currentBestBlock = blockchain.getBestBlockNumber()
    hashes.exists(bh => ancientBlockHash(bh, currentBestBlock))
  }

  private def ancientBlockHash(blockHash: BlockHash, currentBestBlockNumber: BigInt): Boolean =
    (currentBestBlockNumber > blockHash.number) && (currentBestBlockNumber - blockHash.number > syncConfig.maxNewBlockHashAge)

  def handleResponseToRequest(
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Receive = {
    case ResponseReceived(peer: Peer, BlockHeaders(headers), timeTaken) =>
      log.debug("Received {} block headers in {} ms from {} (branch resolution: {})", headers.size, timeTaken, peer, resolvingBranches)
      if (resolvingBranches) {
        handleBlockBranchResolution(peer, headers.reverse, None, headersQueue, topOfTheChain, resumeRegularSyncTimeout, missingStateNodeRetry)
      } else {
        handleBlockHeaders(peer, headers, None, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
      }

    case ResponseReceived(peer, BlockBodies(blockBodies), timeTaken) =>
      log.debug("Received {} block bodies in {} ms", blockBodies.size, timeTaken)
      handleBlockBodies(peer, blockBodies, None, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)

    case ResponseReceived(peer, NodeData(nodes), timeTaken) if missingStateNodeRetry.isDefined =>
      log.debug("Received {} missing state nodes in {} ms", nodes.size, timeTaken)
      handleReDownloadedStateNodes(peer, nodes, None, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)

    case PeerRequestHandler.RequestFailed(peer, reason) if waitingForActor.contains(sender()) =>
      log.debug(s"Request to peer ($peer) failed: $reason")

      if (handshakedPeers.contains(peer)) blacklist(peer.id, blacklistDuration, reason)

      scheduleResume(None, headersQueue, topOfTheChain, resolvingBranches, missingStateNodeRetry)
  }

  def handleMinedBlock(waitingForActor: Option[ActorRef], headersQueue: Seq[BlockHeader], resolvingBranches: Boolean): Receive = {
    // todo improve mined block handling - add info that block was not included because of syncing [EC-250]
    // we allow inclusion of mined block only if we are not syncing / reorganising chain
    case MinedBlock(block) =>
      if (notDownloading(waitingForActor, headersQueue, resolvingBranches)) {
        val importResult = Try(ledger.importBlock(block))

        importResult match {
          case Success(result) =>
            val blockNumber = block.header.number
            result match {
              case BlockImportedToTop(blocks, totalDifficulties) =>
                log.debug(s"Added new mined block $blockNumber to top of the chain")
                broadcastBlocks(blocks, totalDifficulties)
                updateTxAndOmmerPools(blocks, Nil)

              case ChainReorganised(oldBranch, newBranch, totalDifficulties) =>
                log.debug(s"Added new mined block $blockNumber resulting in chain reorganization")
                broadcastBlocks(newBranch, totalDifficulties)
                updateTxAndOmmerPools(newBranch, oldBranch)

              case DuplicateBlock =>
                log.warning("Mined block is a duplicate, this should never happen")

              case BlockEnqueued =>
                log.debug(s"Mined block $blockNumber was added to the queue")
                ommersPool ! AddOmmers(block.header)

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
        ommersPool ! AddOmmers(block.header)
      }
  }

  private def askForHeaders(
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Unit = {
    bestPeer match {
      case Some(peer) =>
        val blockNumber = blockchain.getBestBlockNumber() + 1
        log.debug(s"Requesting $blockHeadersPerRequest headers, starting from $blockNumber")
        val headers = GetBlockHeaders(Left(blockNumber), blockHeadersPerRequest, skip = 0, reverse = false)
        val request = requestBlockHeaders(peer, headers)
        context become running(request, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)

      case None =>
        log.debug("No peers to download from")
        scheduleResume(waitingForActor, headersQueue, topOfTheChain, resolvingBranches, missingStateNodeRetry)
    }
  }

  private def requestMissingNode(
    nodeId: ByteString,
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Unit = {
    bestPeer match {
      case Some(peer) =>
        val request = requestNodeData(peer, nodeId)
        context become running(request, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)

      case None =>
        log.debug("Requesting missing state nodes: no peers to download from")
        scheduleResume(waitingForActor, headersQueue, topOfTheChain, resolvingBranches, missingStateNodeRetry)
    }
  }

  private def handleReDownloadedStateNodes(
    peer: Peer,
    nodes: Seq[ByteString],
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Unit = {
    if (nodes.isEmpty) {
      log.debug(s"Did not receive missing state node from peer ($peer)")
      val reason = "did not receive missing state node"
      resumeWithDifferentPeer(peer, reason, waitingForActor, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
    } else {
      val MissingStateNodeRetry(requestedHash, blockPeer, blocksToRetry) = missingStateNodeRetry.get
      val receivedHash = kec256(nodes.head)

      if (receivedHash != requestedHash) {
        log.debug("Received missing state node has different hash than requested")
        val reason = "wrong state node hash"
        resumeWithDifferentPeer(peer, reason, waitingForActor, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
      } else {
        val nextBlockNumber = blocksToRetry.head.header.number
        // note that we do not analyse whether the node is a leaf, extension or a branch, thus we only
        // handle one state node at a time and retry executing block - this may require multiple attempts
        blockchain.saveNode(requestedHash, nodes.head.toArray, nextBlockNumber)
        log.info(s"Inserted missing state node: ${ hash2string(requestedHash) }. Retrying execution starting with block $nextBlockNumber")
        handleBlocks(blockPeer, blocksToRetry, waitingForActor, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, None)
      }
    }
  }

  private def handleBlockBranchResolution(
    peer: Peer,
    message: Seq[BlockHeader],
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Unit = {
    val resolvingBranches = false
    if (message.nonEmpty && message.last.hash == headersQueue.head.parentHash) {
      val blockHeaders = message ++ headersQueue
      context become running(waitingForActor, blockHeaders, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
      processBlockHeaders(peer, waitingForActor, blockHeaders, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
    } else {
      // we did not get previous blocks, there is no way to resolve, blacklist peer and continue download
      val reason = "failed to resolve branch"
      resumeWithDifferentPeer(peer, reason, waitingForActor, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
    }
  }

  private def handleBlockHeaders(
    peer: Peer,
    message: Seq[BlockHeader],
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Unit = {
    if (message.nonEmpty) {
      processBlockHeaders(peer, waitingForActor, message, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
    } else {
      // no new headers to process, schedule to ask again in future, we are at the top of chain
      scheduleResume(waitingForActor, headersQueue, topOfTheChain = true, resolvingBranches, missingStateNodeRetry)
    }
  }

  private def processBlockHeaders(
    peer: Peer,
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Unit = ledger.resolveBranch(headersQueue) match {
    case NewBetterBranch(oldBranch) =>
      val transactionsToAdd = oldBranch.flatMap(_.body.transactionList).toSet
      pendingTransactionsManager ! PendingTransactionsManager.AddTransactions(transactionsToAdd)
      val request = requestBlockBodies(peer, headersQueue)
      context become running(request, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)

      // add first block from branch as ommer
      oldBranch.headOption.foreach{ h => ommersPool ! AddOmmers(h.header) }

    case NoChainSwitch =>
      // add first block from branch as ommer
      headersQueue.headOption.foreach{ h => ommersPool ! AddOmmers(h) }

      scheduleResume(waitingForActor, headersQueue, topOfTheChain, resolvingBranches, missingStateNodeRetry)

    case UnknownBranch =>
      if (resolvingBranches) {
        log.debug("Fail to resolve branch, branch too long, it may indicate malicious peer")
        val reason = "failed to resolve branch"
        resumeWithDifferentPeer(peer, reason, waitingForActor, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
      } else {
        val parentHash = hash2string(headersQueue.head.parentHash)
        log.debug(s"Requesting $branchResolutionRequestSize additional headers for branch resolution, starting from: " + parentHash)
        val headers = GetBlockHeaders(Right(headersQueue.head.parentHash), branchResolutionRequestSize, skip = 0, reverse = true)
        val request = requestBlockHeaders(peer, headers)
        context become running(request, headersQueue, topOfTheChain, resolvingBranches = true, resumeRegularSyncTimeout, missingStateNodeRetry)
      }

    case InvalidBranch =>
      log.debug("Got block header that does not have parent")
      val reason = "error in response"
      resumeWithDifferentPeer(peer, reason, waitingForActor, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
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

  private def handleBlockBodies(
    peer: Peer,
    blockBodies: Seq[BlockBody],
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Unit = {
    if (blockBodies.nonEmpty) {
      assert(headersQueue.nonEmpty, "handling block bodies while not having any queued headers")
      val blocks = headersQueue.zip(blockBodies).map{ case (header, body) => Block(header, body) }
      handleBlocks(peer, blocks, waitingForActor, headersQueue, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
    } else {
      val reason = "received empty block bodies"
      resumeWithDifferentPeer(peer, reason, waitingForActor, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
    }
  }

  private def handleBlocks(
    peer: Peer,
    blocks: Seq[Block],
    waitingForActor: Option[ActorRef],
    headersQueue: Seq[BlockHeader],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Unit = {
    val (importedBlocks, errorOpt) = importBlocks(blocks.toList)

    if (importedBlocks.nonEmpty) {
      val lastHeader = importedBlocks.last.header
      log.debug(s"Got new blocks up till block: ${lastHeader.number} with hash ${hash2string(lastHeader.hash)}")
    }

    val headers = headersQueue.drop(blocks.length)
    errorOpt match {
      case Some(missingNodeEx: MissingNodeException) =>
        // Missing state node recovery mechanism.
        // All block headers that were about to be imported are removed from the queue and we save the full blocks
        // that weren't imported yet - this will avoid re-downloading block headers and bodies in between (possibly
        // multiple) state node requests.
        log.error(missingNodeEx, "Requesting missing state nodes")
        val blocksToRetry = blocks.drop(importedBlocks.length)
        val newMissingStateNodeRetry = Some(MissingStateNodeRetry(missingNodeEx.hash, peer, blocksToRetry))
        requestMissingNode(missingNodeEx.hash, waitingForActor, headers, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, newMissingStateNodeRetry)

      case Some(error) =>
        val reason = s"a block execution error: ${error.toString}"
        resumeWithDifferentPeer(peer, reason, waitingForActor, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)

      case None =>
        if (headersQueue.nonEmpty) {
          val request = requestBlockBodies(peer, headersQueue)
          context become running(request, headers, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
        } else {
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

  private def resumeWithDifferentPeer(
    currentPeer: Peer,
    reason: String,
    waitingForActor: Option[ActorRef],
    topOfTheChain: Boolean,
    resolvingBranches: Boolean,
    resumeRegularSyncTimeout: Option[Cancellable],
    missingStateNodeRetry: Option[MissingStateNodeRetry]
  ): Unit = {
    blacklist(currentPeer.id, blacklistDuration, reason)
    context become running(waitingForActor, Seq.empty, topOfTheChain, resolvingBranches, resumeRegularSyncTimeout, missingStateNodeRetry)
    context.self ! ResumeRegularSync
  }

  private def bestPeer: Option[Peer] = {
    val peersToUse = peersToDownloadFrom
      .collect{ case (ref, PeerInfo(_, totalDifficulty, true, _)) => (ref, totalDifficulty) }

    if (peersToUse.nonEmpty) Some(peersToUse.maxBy{ case (_, td) => td }._1) else None
  }

  private def updateTxAndOmmerPools(blocksAdded: Seq[Block], blocksRemoved: Seq[Block]): Unit = {
    blocksRemoved.headOption.foreach(block => ommersPool ! AddOmmers(block.header))
    blocksRemoved.foreach(block => pendingTransactionsManager ! AddTransactions(block.body.transactionList.toSet))

    blocksAdded.foreach{ block =>
      ommersPool ! RemoveOmmers(block.header :: block.body.uncleNodesList.toList)
      pendingTransactionsManager ! RemoveTransactions(block.body.transactionList)
    }
  }

  private def broadcastBlocks(blocks: Seq[Block], totalDifficulties: Seq[BigInt]): Unit = {
    blocks.zip(totalDifficulties).foreach{ case (block, td) =>
      broadcaster.broadcastBlock(NewBlock(block, td), handshakedPeers)
    }
  }

  private def notDownloading(waitingForActor: Option[ActorRef], headersQueue: Seq[BlockHeader], resolvingBranches: Boolean): Boolean =
    headersQueue.isEmpty && waitingForActor.isEmpty && !resolvingBranches

}

object RegularSync {
  // scalastyle:off parameter.number
  def props(appStateStorage: AppStateStorage, etcPeerManager: ActorRef, peerEventBus: ActorRef, ommersPool: ActorRef,
    pendingTransactionsManager: ActorRef, broadcaster: BlockBroadcast, ledger: Ledger, blockchain: Blockchain,
    syncConfig: SyncConfig, scheduler: Scheduler): Props =
    Props(new RegularSync(appStateStorage, etcPeerManager, peerEventBus, ommersPool, pendingTransactionsManager,
      broadcaster, ledger, blockchain, syncConfig, scheduler))

  private[sync] case object ResumeRegularSync

  private case class ResolveBranch(peer: ActorRef)

  private[sync] case object PrintStatus

  case object Start

  /** This start the actor without asking for headers, currently only used in tests */
  case object StartIdle

  case class MinedBlock(block: Block)

  case class MissingStateNodeRetry(nodeId: ByteString, p: Peer, blocksToRetry: Seq[Block])

}
