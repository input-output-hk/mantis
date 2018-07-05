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
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.ommers.OmmersPool.{AddOmmers, RemoveOmmers}
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.{AddTransactions, RemoveTransactions}
import io.iohk.ethereum.utils.Config.SyncConfig
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.utils.Riemann

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

//TODO Refactor to get rid of most of mutable state [EC-320]
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


  private var headersQueue: Seq[BlockHeader] = Nil
  private var waitingForActor: Option[ActorRef] = None
  var resolvingBranches: Boolean = false
  /** This is used as an optimisation to avoid handling broadcast messages when we haven't reached the top of the chain.
    * Currently it's set to true after we receive 0 headers from a peer, which usually means it doesn't have any new headers.
    * But there could be other reasons for receiving 0 blocks. It may be better to make handling broadcast messages
    * dependent on our current best block info (stored in this actor to avoid DB lookups).
    */
  var topOfTheChain: Boolean = false
  private var resumeRegularSyncTimeout: Option[Cancellable] = None

  var missingStateNodeRetry: Option[MissingStateNodeRetry] = None

  scheduler.schedule(reportStatusInterval, reportStatusInterval, self, PrintStatus)

  peerEventBus ! Subscribe(MessageClassifier(Set(NewBlock.code, NewBlockHashes.code), PeerSelector.AllPeers))

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  override def receive: Receive = idle

  def idle: Receive = handleCommonMessages orElse {
    case Start =>
      log.info("Starting block synchronization")
      appStateStorage.fastSyncDone()
      context become running
      askForHeaders()

    case StartIdle =>
      appStateStorage.fastSyncDone()
      context become running
  }

  def running: Receive = handleBasicMessages orElse handleAdditionalMessages orElse handleResumingAndPrinting

  def handleBasicMessages: Receive = handleCommonMessages orElse handleResponseToRequest

  def handleResumingAndPrinting: Receive = {
    case ResumeRegularSync =>
      resumeRegularSync()

    case PrintStatus =>
      log.info(s"Block: ${appStateStorage.getBestBlockNumber()}. Peers: ${handshakedPeers.size} (${blacklistedPeers.size} blacklisted)")
  }

  def handleAdditionalMessages: Receive = handleNewBlockMessages orElse handleMinedBlock orElse handleNewBlockHashesMessages

  private def resumeRegularSync(): Unit = {
    cancelScheduledResume()

    // The case that waitingForActor is defined (we are waiting for some response),
    // can happen when we are on top of the chain and currently handling newBlockHashes message

    if (waitingForActor.isEmpty) {
      if (missingStateNodeRetry.isEmpty) {
        headersQueue = Nil
        resolvingBranches = false
        askForHeaders()
      } else {
        requestMissingNode(missingStateNodeRetry.get.nodeId)
      }
    } else {
      resumeRegularSyncTimeout = Some(scheduler.scheduleOnce(checkForNewBlockInterval, self, ResumeRegularSync))
    }
  }

  private def cancelScheduledResume(): Unit = {
    resumeRegularSyncTimeout.foreach(_.cancel)
    resumeRegularSyncTimeout = None
  }

  def handleNewBlockMessages: Receive = {
    case MessageFromPeer(NewBlock(newBlock, _), peerId) =>
      //we allow inclusion of new block only if we are not syncing
      if (notDownloading() && topOfTheChain) {
        Riemann.ok("new block message").attribute("id", newBlock.idTag)
        val importResult = Try(ledger.importBlock(newBlock))

        importResult match {
          case Success(result) => result match {
            case BlockImportedToTop(newBlocks, newTds) =>
              broadcastBlocks(newBlocks, newTds)
              updateTxAndOmmerPools(newBlocks, Nil)
              Riemann.ok("new block imported to top")
                .metric(newBlock.header.number.longValue)
                .attribute("number", newBlock.header.number.toString)
                .attribute("peerId", peerId.toString)
                .send

            case BlockEnqueued =>
              ommersPool ! AddOmmers(newBlock.header)
              Riemann.ok("new block enqueued")
                .metric(newBlock.header.number.longValue)
                .attribute("block", Hex.toHexString(newBlock.header.hash.toArray).toString)
                .attribute("peerId", peerId.toString)
                .send

            case DuplicateBlock =>
              Riemann.ok("new block duplicate")
                .metric(newBlock.header.number.longValue)
                .attribute("block", Hex.toHexString(newBlock.header.hash.toArray).toString)
                .attribute("peerId", peerId.toString)
                .send

            case UnknownParent =>
              // This is normal when receiving broadcasted blocks
              Riemann.ok("new block unknown parent")
                .metric(newBlock.header.number.longValue)
                .attribute("block", Hex.toHexString(newBlock.header.hash.toArray).toString)
                .attribute("peerId", peerId.toString)
                .send

            case ChainReorganised(oldBranch, newBranch, totalDifficulties) =>
              updateTxAndOmmerPools(newBranch, oldBranch)
              broadcastBlocks(newBranch, totalDifficulties)
              Riemann.ok("new block chain reorganised")
                .metric(newBlock.header.number.longValue)
                .attribute("block", Hex.toHexString(newBlock.header.hash.toArray).toString)
                .attribute("peerId", peerId.toString)
                .attribute("branchLength", newBranch.size.toString)
                .attribute("previousNumber", newBranch.last.header.number.toString)
                .attribute("previousBlock", Hex.toHexString(newBranch.last.header.hash.toArray).toString)
                .send

            case BlockImportFailed(error) =>
              blacklist(peerId, blacklistDuration, error)
              Riemann.warning("new block import failed")
                .metric(newBlock.header.number.longValue)
                .attribute("peerId", peerId.toString)
                .attribute("blacklistDuration", blacklistDuration.toString)
                .attribute("peerId", error)
                .send
          }

          case Failure(missingNodeEx: MissingNodeException) if syncConfig.redownloadMissingStateNodes =>
            // state node redownload will be handled when downloading headers
            Riemann.exception("new block missing node", missingNodeEx).send

          case Failure(ex) =>
            Riemann.exception("new block", ex).send
            throw ex
        }
      }
  }

  /**
    * Handles NewHashesMessege, should only cover this message when we are top of the chain
    */
  def handleNewBlockHashesMessages: Receive = {
    case MessageFromPeer(NewBlockHashes(hashes), peerId) =>
      val maybePeer = peersToDownloadFrom.find(peer => peer._1.id == peerId)
      //we allow asking for new hashes when we are not syncing and we can download from specified peer ,we are
      //top of the chain and not resolving branches currently
      if (notDownloading() && topOfTheChain && maybePeer.isDefined) {
        log.debug("Handling NewBlockHashes message: \n" + hashes.mkString("\n"))
        val (peer, _) = maybePeer.get
        val hashesToCheck = hashes.take(syncConfig.maxNewHashes)

        if (!containsAncientBlockHash(hashesToCheck)) {
          val filteredHashes = getValidHashes(hashesToCheck)

          if (filteredHashes.nonEmpty) {
            val request = GetBlockHeaders(Right(filteredHashes.head.hash), filteredHashes.length, BigInt(0), reverse = false)
            requestBlockHeaders(peer, request)
            cancelScheduledResume()
          } else {
            log.debug("All received hashes all already in Chain, or Queue ")
          }
        } else {
          blacklist(peerId, blacklistDuration, "received ancient blockHash")
        }
      }
  }

  private def getValidHashes(unfilteredHashes: Seq[BlockHash]): Seq[BlockHash] = unfilteredHashes.foldLeft(Seq.empty[BlockHash])((hashesList, blockHash) =>
    ledger.checkBlockStatus(blockHash.hash) match {
      case InChain =>
        log.debug(s"BlockHash with Number: ${blockHash.number} and Hash: ${Hex.toHexString(blockHash.hash.toArray)} already in chain")
        hashesList
      case Queued =>
        log.debug(s"BlockHash with Number: ${blockHash.number} and Hash: ${Hex.toHexString(blockHash.hash.toArray)} already in queue")
        hashesList
      case UnknownBlock =>
        log.debug(s"Preparing to download unknown block with number ${blockHash.number} and hash ${Hex.toHexString(blockHash.hash.toArray)}")
        hashesList :+ blockHash
    }
  )

  private def containsAncientBlockHash(hashes: Seq[BlockHash]): Boolean = {
    val currentBestBlock = appStateStorage.getBestBlockNumber()
    hashes.exists(bh => ancientBlockHash(bh, currentBestBlock))
  }

  private def ancientBlockHash(blockHash: BlockHash, currentBestBlockNumber: BigInt) =
    currentBestBlockNumber > blockHash.number && currentBestBlockNumber - blockHash.number > syncConfig.maxNewBlockHashAge

  def handleResponseToRequest: Receive = {
    case ResponseReceived(peer: Peer, BlockHeaders(headers), timeTaken) =>
      Riemann.ok("received block headers")
        .metric(headers.size)
        .attribute("timeTaken", timeTaken.toString)
        .attribute("peer", peer.toString)
        .attribute("resolvingBranches", resolvingBranches.toString)
        .send
      waitingForActor = None
      if (resolvingBranches) handleBlockBranchResolution(peer, headers.reverse)
      else handleBlockHeaders(peer, headers)

    case ResponseReceived(peer, BlockBodies(blockBodies), timeTaken) =>
      Riemann.ok("received block bodies")
        .metric(blockBodies.size)
        .attribute("timeTaken", timeTaken.toString)
        .send
      waitingForActor = None
      handleBlockBodies(peer, blockBodies)

    case ResponseReceived(peer, NodeData(nodes), timeTaken) if missingStateNodeRetry.isDefined =>
      Riemann.ok("received missing state nodes")
        .metric(nodes.size)
        .attribute("timeTaken", timeTaken.toString)
        .send
      waitingForActor = None
      handleRedownloadedStateNodes(peer, nodes)

    case PeerRequestHandler.RequestFailed(peer, reason) if waitingForActor.contains(sender()) =>
      Riemann.warning("peer request failed")
        .attribute("peer", peer.toString)
        .attribute("reason", reason.toString)
        .send
      waitingForActor = None
      if (handshakedPeers.contains(peer)) {
        blacklist(peer.id, blacklistDuration, reason)
      }
      scheduleResume()
  }

  def handleMinedBlock: Receive = {

    //todo improve mined block handling - add info that block was not included because of syncing [EC-250]
    //we allow inclusion of mined block only if we are not syncing / reorganising chain
    case MinedBlock(block) =>
      if (notDownloading()) {
        val importResult = Try(ledger.importBlock(block))

        importResult match {
          case Success(result) => result match {
            case BlockImportedToTop(blocks, totalDifficulties) =>
              Riemann.ok("mined block imported to top").metric(block.header.number.longValue).send
              broadcastBlocks(blocks, totalDifficulties)
              updateTxAndOmmerPools(blocks, Nil)

            case ChainReorganised(oldBranch, newBranch, totalDifficulties) =>
              Riemann.ok("mined block chain reorganised").metric(block.header.number.longValue).send
              broadcastBlocks(newBranch, totalDifficulties)
              updateTxAndOmmerPools(newBranch, oldBranch)

            case DuplicateBlock =>
              Riemann.warning("mined block chain duplicate").metric(block.header.number.longValue).send

            case BlockEnqueued =>
              Riemann.ok("mined block enqueued").metric(block.header.number.longValue).send
              ommersPool ! AddOmmers(block.header)

            case UnknownParent =>
              Riemann.warning("mined block unknown parent").metric(block.header.number.longValue).send

            case BlockImportFailed(err) =>
              Riemann.warning("mined block import failed").metric(block.header.number.longValue).attribute("error", err).send
          }

          case Failure(missingNodeEx: MissingNodeException) if syncConfig.redownloadMissingStateNodes =>
            Riemann.exception("mined block missing node", missingNodeEx).metric(block.header.number.longValue).send

          case Failure(ex) =>
            Riemann.exception("mined block", ex).metric(block.header.number.longValue).send
            throw ex
        }

      } else {
        ommersPool ! AddOmmers(block.header)
      }
  }

  private def askForHeaders() = {
    bestPeer match {
      case Some(peer) =>
        val blockNumber = appStateStorage.getBestBlockNumber() + 1
        log.debug(s"Requesting $blockHeadersPerRequest headers, starting from $blockNumber")
        requestBlockHeaders(peer, GetBlockHeaders(Left(blockNumber), blockHeadersPerRequest, skip = 0, reverse = false))

      case None =>
        log.debug("No peers to download from")
        scheduleResume()
    }
  }

  private def requestMissingNode(nodeId: ByteString): Unit = {
    bestPeer match {
      case Some(peer) =>
        waitingForActor = Some(context.actorOf(
          PeerRequestHandler.props[GetNodeData, NodeData](
            peer, peerResponseTimeout, etcPeerManager, peerEventBus,
            requestMsg = GetNodeData(List(nodeId)),
            responseMsgCode = NodeData.code)))

      case None =>
        log.debug("Requesting missing state nodes: no peers to download from")
        scheduleResume()
    }
  }

  private def handleRedownloadedStateNodes(peer: Peer, nodes: Seq[ByteString]): Unit = {
    if (nodes.isEmpty) {
      log.debug(s"Did not receive missing state node from peer ($peer)")
      resumeWithDifferentPeer(peer, "did not receive missing state node")
    } else {
      val MissingStateNodeRetry(requestedHash, blockPeer, blocksToRetry) = missingStateNodeRetry.get
      val receivedHash = kec256(nodes.head)

      if (receivedHash != requestedHash) {
        log.debug(s"Received missing state node has different hash than requested")
        resumeWithDifferentPeer(peer, "wrong state node hash")
      } else {
        val nextBlockNumber = blocksToRetry.head.header.number
        // note that we do not analyse whether the node is a leaf, extension or a branch, thus we only
        // handle one state node at a time and retry executing block - this may require multiple attempts
        blockchain.saveNode(requestedHash, nodes.head.toArray, nextBlockNumber)
        missingStateNodeRetry = None
        log.info(s"Inserted missing state node: ${Hex.toHexString(requestedHash.toArray)}. " +
          s"Retrying execution starting with block $nextBlockNumber")
        handleBlocks(blockPeer, blocksToRetry)
      }
    }
  }

  private def handleBlockBranchResolution(peer: Peer, message: Seq[BlockHeader]) = {
    if (message.nonEmpty && message.last.hash == headersQueue.head.parentHash) {
      headersQueue = message ++ headersQueue
      processBlockHeaders(peer, headersQueue)
    } else {
      //we did not get previous blocks, there is no way to resolve, blacklist peer and continue download
      resumeWithDifferentPeer(peer, "failed to resolve branch")
    }
    resolvingBranches = false
  }

  private def handleBlockHeaders(peer: Peer, message: Seq[BlockHeader]) = if (message.nonEmpty) {
    headersQueue = message
    processBlockHeaders(peer, message)
  } else {
    //no new headers to process, schedule to ask again in future, we are at the top of chain
    topOfTheChain = true
    scheduleResume()
  }

  private def processBlockHeaders(peer: Peer, headers: Seq[BlockHeader]) = ledger.resolveBranch(headers) match {
    case NewBetterBranch(oldBranch) =>
      val transactionsToAdd = oldBranch.flatMap(_.body.transactionList)
      pendingTransactionsManager ! PendingTransactionsManager.AddTransactions(transactionsToAdd.toList)
      val hashes = headers.take(blockBodiesPerRequest).map(_.hash)
      requestBlockBodies(peer, GetBlockBodies(hashes))

      //add first block from branch as ommer
      oldBranch.headOption.foreach { h => ommersPool ! AddOmmers(h.header) }

    case NoChainSwitch =>
      //add first block from branch as ommer
      headers.headOption.foreach { h => ommersPool ! AddOmmers(h) }
      scheduleResume()

    case UnknownBranch =>
      if (resolvingBranches) {
        log.debug("fail to resolve branch, branch too long, it may indicate malicious peer")
        resumeWithDifferentPeer(peer, "failed to resolve branch")
      } else {
        log.debug(s"requesting $branchResolutionRequestSize additional headers for branch resolution, " +
          "starting from: " + Hex.toHexString(headersQueue.head.parentHash.toArray))
        val request = GetBlockHeaders(Right(headersQueue.head.parentHash), branchResolutionRequestSize, skip = 0, reverse = true)
        requestBlockHeaders(peer, request)
        resolvingBranches = true
      }

    case InvalidBranch =>
      log.debug("Got block header that does not have parent")
      resumeWithDifferentPeer(peer)
  }

  private def requestBlockHeaders(peer: Peer, msg: GetBlockHeaders): Unit = {
    waitingForActor = Some(context.actorOf(
      PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
        peer, peerResponseTimeout, etcPeerManager, peerEventBus,
        requestMsg = msg,
        responseMsgCode = BlockHeaders.code)))
  }

  private def requestBlockBodies(peer: Peer, msg: GetBlockBodies): Unit = {
    waitingForActor = Some(context.actorOf(
      PeerRequestHandler.props[GetBlockBodies, BlockBodies](
        peer, peerResponseTimeout, etcPeerManager, peerEventBus,
        requestMsg = msg,
        responseMsgCode = BlockBodies.code)))
  }

  private def handleBlockBodies(peer: Peer, m: Seq[BlockBody]): Unit = {
    if (m.nonEmpty) {
      assert(headersQueue.nonEmpty, "handling block bodies while not having any queued headers")
      val blocks = headersQueue.zip(m).map { case (header, body) => Block(header, body) }
      handleBlocks(peer, blocks)
    } else {
      resumeWithDifferentPeer(peer, "received empty block bodies")
      None
    }
  }

  private def handleBlocks(peer: Peer, blocks: Seq[Block]) = {
    val (importedBlocks, errorOpt) = importBlocks(blocks.toList)

    if (importedBlocks.nonEmpty) {
      log.debug(s"got new blocks up till block: ${importedBlocks.last.header.number} " +
        s"with hash ${Hex.toHexString(importedBlocks.last.header.hash.toArray[Byte])}")
    }

    errorOpt match {
      case Some(missingNodeEx: MissingNodeException) =>
        // Missing state node recovery mechanism.
        // All block headers that were about to be imported are removed from the queue and we save the full blocks
        // that weren't imported yet - this will avoid redownloading block headers and bodies in between (possibly
        // multiple) state node requests.
        log.error(missingNodeEx, "Requesting missing state nodes")
        headersQueue = headersQueue.drop(blocks.length)
        val blocksToRetry = blocks.drop(importedBlocks.length)
        missingStateNodeRetry = Some(MissingStateNodeRetry(missingNodeEx.hash, peer, blocksToRetry))
        requestMissingNode(missingNodeEx.hash)

      case Some(error) =>
        resumeWithDifferentPeer(peer, reason = s"a block execution error: ${error.toString}")

      case None =>
        headersQueue = headersQueue.drop(blocks.length)
        if (headersQueue.nonEmpty) {
          val hashes = headersQueue.take(blockBodiesPerRequest).map(_.hash)
          requestBlockBodies(peer, GetBlockBodies(hashes))
        } else {
          context.self ! ResumeRegularSync
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

              case r @ (DuplicateBlock | BlockEnqueued) =>
                importBlocks(tail, importedBlocks)

              case err @ (UnknownParent | BlockImportFailed(_)) =>
                (importedBlocks, Some(err))
            }

          //return this exception as a result only when the recovery mechanism is turned on in config
          case Failure(missingNodeEx: MissingNodeException) if syncConfig.redownloadMissingStateNodes =>
            (importedBlocks, Some(missingNodeEx))

          case Failure(ex) =>
            throw ex
        }
    }

  private def scheduleResume() = {
    resumeRegularSyncTimeout = Some(scheduler.scheduleOnce(checkForNewBlockInterval, self, ResumeRegularSync))
  }

  private def resumeWithDifferentPeer(currentPeer: Peer, reason: String = "error in response") = {
    blacklist(currentPeer.id, blacklistDuration, reason)
    headersQueue = Nil
    context.self ! ResumeRegularSync
  }

  private def bestPeer: Option[Peer] = {
    val peersToUse = peersToDownloadFrom
      .collect {
        case (ref, PeerInfo(_, totalDifficulty, true, _)) => (ref, totalDifficulty)
      }

    if (peersToUse.nonEmpty) Some(peersToUse.maxBy { case (_, td) => td }._1)
    else None
  }

  private def updateTxAndOmmerPools(blocksAdded: Seq[Block], blocksRemoved: Seq[Block]): Unit = {
    blocksRemoved.headOption.foreach(block => ommersPool ! AddOmmers(block.header))
    blocksRemoved.foreach(block => pendingTransactionsManager ! AddTransactions(block.body.transactionList.toList))

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

  private def notDownloading(): Boolean =
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
  private case object PrintStatus

  case object Start

  /**
    * This start the actor without asking for headers, currently only used in tests
    */
  case object StartIdle

  case class MinedBlock(block: Block)

  case class MissingStateNodeRetry(nodeId: ByteString, p: Peer, blocksToRetry: Seq[Block])
}
