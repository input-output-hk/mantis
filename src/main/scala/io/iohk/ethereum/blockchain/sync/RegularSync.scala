package io.iohk.ethereum.blockchain.sync

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.eventbus.event.NewHead
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
import io.iohk.ethereum.utils.events._

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
  extends Actor with ActorLogging with PeerListSupport with BlacklistSupport with EventSupport {

  import RegularSync._
  import syncConfig._

  protected def mainService: String = "regular sync"

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
      val number = appStateStorage.getBestBlockNumber()
      val numberHex = number.toString(16)
      val handshakedPeerCount = handshakedPeers.size
      val blacklistedPeerCount = blacklistedPeers.size
      log.info(s"Best Block: ${number} (= 0x${numberHex}). Handshaked Peers: ${handshakedPeerCount}. Blacklisted: ${blacklistedPeerCount}")
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

  // scalastyle:off
  def handleNewBlockMessages: Receive = {
    case MessageFromPeer(NewBlock(newBlock, _), peerId) =>
      //we allow inclusion of new block only if we are not syncing
      if (notDownloading() && topOfTheChain) {
        Event.ok("block new message")
          .block(newBlock)
          .send()

        val importResult = RegularSync.tryImportBlock(
          context = "handleNewBlockMessages",
          regularSync = this,
          block = newBlock
        )

        importResult match {
          case Success(result) => result match {
            case BlockImportedToTop(newBlocks, newTds) =>
              broadcastBlocks(newBlocks, newTds)
              updateTxAndOmmerPools(newBlocks, Nil)
              publishEvents(newBlocks, Nil)

              Event.ok("block new imported to top")
                .metric(newBlock.header.number.longValue)
                .block(newBlock)
                .peerId(peerId)
                .send()

            case BlockEnqueued =>
              ommersPool ! AddOmmers(newBlock.header)

              Event.ok("block new enqueued")
                .metric(newBlock.header.number.longValue)
                .block(newBlock)
                .peerId(peerId)
                .send()

            case DuplicateBlock =>
              Event.ok("block new duplicate")
                .metric(newBlock.header.number.longValue)
                .block(newBlock)
                .peerId(peerId)
                .send()

            case UnknownParent =>
              // This is normal when receiving broadcasted blocks
              Event.ok("block new unknown parent")
                .metric(newBlock.header.number.longValue)
                .block(newBlock)
                .peerId(peerId)
                .send()

            case ChainReorganised(oldBranch, newBranch, totalDifficulties) =>
              updateTxAndOmmerPools(newBranch, oldBranch)
              broadcastBlocks(newBranch, totalDifficulties)
              publishEvents(newBranch, oldBranch)

              Event.ok("block new chain reorganised")
                .metric(newBlock.header.number.longValue)
                .block(newBlock)
                .peerId(peerId)
                .attribute("branchLength", newBranch.size.toString)
                .send()

            case BlockImportFailed(error) =>
              blacklist(peerId, blacklistDuration, error)

              Event.warning("block new import failed")
                .metric(newBlock.header.number.longValue)
                .peerId(peerId)
                .attribute("blacklistDuration", blacklistDuration.toString)
                .attribute(EventAttr.Error, error)
                .send()
          }

          case Failure(missingNodeEx: MissingNodeException) if syncConfig.redownloadMissingStateNodes =>
            // state node redownload will be handled when downloading headers
            Event.exception("block new missing node", missingNodeEx).send()

          case Failure(ex) =>
            Event.exception("block new", ex).send()
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
      Event.ok("block headers received")
        .metric(headers.size)
        .timeTakenMs(timeTaken)
        .attribute("peer", peer.toString)
        .attribute("resolvingBranches", resolvingBranches.toString)
        .send()

      waitingForActor = None
      if (resolvingBranches) handleBlockBranchResolution(peer, headers.reverse)
      else handleBlockHeaders(peer, headers)

    case ResponseReceived(peer, BlockBodies(blockBodies), timeTaken) =>
      Event.ok("block bodies received")
        .metric(blockBodies.size)
        .timeTakenMs(timeTaken)
        .send()

      waitingForActor = None
      handleBlockBodies(peer, blockBodies)

    case ResponseReceived(peer, NodeData(nodes), timeTaken) if missingStateNodeRetry.isDefined =>
      Event.ok("received missing state nodes")
        .metric(nodes.size)
        .timeTakenMs(timeTaken)
        .send()

      waitingForActor = None
      handleRedownloadedStateNodes(peer, nodes)

    case PeerRequestHandler.RequestFailed(peer, reason) if waitingForActor.contains(sender()) =>
      Event.warning("peer request failed")
        .attribute("peer", peer.toString)
        .attribute("reason", reason.toString)
        .send()

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
        val importResult = RegularSync.tryImportBlock(
          context = "handleMinedBlock",
          regularSync = this,
          block = block
        )

        importResult match {
          case Success(result) => result match {
            case BlockImportedToTop(blocks, totalDifficulties) =>
              Event.ok("block mined imported to top")
                .metric(block.header.number.longValue)
                .send()

              publishEvents(blocks, Nil)
              broadcastBlocks(blocks, totalDifficulties)
              updateTxAndOmmerPools(blocks, Nil)

            case ChainReorganised(oldBranch, newBranch, totalDifficulties) =>
              Event.ok("mined block chain reorganised")
                .metric(block.header.number.longValue)
                .send()

              broadcastBlocks(newBranch, totalDifficulties)
              updateTxAndOmmerPools(newBranch, oldBranch)
              publishEvents(newBranch, oldBranch)

            case DuplicateBlock =>
              Event.warning("mined block chain duplicate")
                .metric(block.header.number.longValue)
                .send()

            case BlockEnqueued =>
              Event.ok("mined block enqueued")
                .metric(block.header.number.longValue)
                .send()

              ommersPool ! AddOmmers(block.header)

            case UnknownParent =>
              Event.warning("mined block unknown parent")
                .metric(block.header.number.longValue)
                .send()

            case BlockImportFailed(err) =>
              Event.warning("mined block import failed")
                .metric(block.header.number.longValue)
                .attribute("error", err)
                .send()
          }

          case Failure(missingNodeEx: MissingNodeException) if syncConfig.redownloadMissingStateNodes =>
            Event.exception("mined block missing node", missingNodeEx)
              .metric(block.header.number.longValue)
              .send()

          case Failure(ex) =>
            Event.exception("mined block", ex)
              .metric(block.header.number.longValue)
              .send()
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
        val blockNumberHex = blockNumber.toString(16)
        log.info(s"Requesting $blockHeadersPerRequest headers, starting block $blockNumber [= 0x${blockNumberHex}] from ${peer.id}")
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
      val receivedHashHex = Hex.toHexString(receivedHash.toArray)
      val requestedHashHex = Hex.toHexString(requestedHash.toArray)

      if (receivedHash != requestedHash) {
        log.info(s"Received missing state node has different hash 0x${receivedHashHex} than requested 0x${requestedHash}")
        resumeWithDifferentPeer(peer, s"wrong state node hash, requested 0x${requestedHashHex}, received 0x${receivedHashHex}")
      } else {
        val nextBlock = blocksToRetry.head
        val nextBlockNumber = nextBlock.header.number
        val nextBlockNumberHex = nextBlockNumber.toString(16)

        // note that we do not analyse whether the node is a leaf, extension or a branch, thus we only
        // handle one state node at a time and retry executing block - this may require multiple attempts
        blockchain.saveNode(requestedHash, nodes.head.toArray, nextBlockNumber)
        missingStateNodeRetry = None
        log.info(s"Inserted missing state node: 0x${requestedHashHex}. " +
          s"Retrying execution starting with block ${nextBlock.idTag}")
        handleBlocks(blockPeer, blocksToRetry)
      }
    }
  }

  private def handleBlockBranchResolution(peer: Peer, message: Seq[BlockHeader]) = {
    val messageNonEmpty: Boolean = message.nonEmpty
    val sameHashes: Boolean = message.last.hash == headersQueue.head.parentHash
    if (messageNonEmpty && sameHashes) {
      headersQueue = message ++ headersQueue
      processBlockHeaders(peer, headersQueue)
    } else {
      //we did not get previous blocks, there is no way to resolve, blacklist peer and continue download
      resumeWithDifferentPeer(peer, s"failed to resolve branch, messageNonEmpty=${messageNonEmpty}, sameHashes=${sameHashes}, both should be true")
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

  private def headersLogInfo(headers: Seq[BlockHeader]): String = {
    if(headers.nonEmpty) {
      val howmany = headers.size
      val firstNumber = headers.head.number
      val firstNumberHex = firstNumber.toString(16)
      val lastNumber = headers.last.number
      val lastNumberHex = lastNumber.toString(16)

      s"${howmany} headers: [$firstNumber (= 0x$firstNumberHex), $lastNumber (= 0x$lastNumberHex)]"
    } else "[]"
  }

  private def blocksLogInfo(blocks: Seq[Block]): String =
    headersLogInfo(blocks.map(_.header))

  private def processBlockHeaders(peer: Peer, headers: Seq[BlockHeader]) = ledger.resolveBranch(headers) match {
    case NewBetterBranch(oldBranch) =>
      log.info(s"[NewBetterBranch] ${headersLogInfo(headers)}, oldBranch: ${blocksLogInfo(oldBranch)}")

      val transactionsToAdd = oldBranch.flatMap(_.body.transactionList)
      pendingTransactionsManager ! PendingTransactionsManager.AddTransactions(transactionsToAdd.toList)
      val hashes = headers.take(blockBodiesPerRequest).map(_.hash)
      requestBlockBodies(peer, GetBlockBodies(hashes))

      //add first block from branch as ommer
      oldBranch.headOption.foreach { h => ommersPool ! AddOmmers(h.header) }

    case NoChainSwitch =>
      log.info(s"[NoChainSwitch] ${headersLogInfo(headers)}")

      //add first block from branch as ommer
      headers.headOption.foreach { h => ommersPool ! AddOmmers(h) }
      scheduleResume()

    case UnknownBranch =>
      log.info(s"[UnknownBranch] ${headersLogInfo(headers)}")

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

    case e: InvalidBranch =>
      e match {
        case InvalidBranchNoChain =>
          log.info(s"[InvalidBranchNoChain] ${headersLogInfo(headers)}")

        case InvalidBranchLastBlockNumberIsSmall =>
          log.info(s"[InvalidBranchLastBlockNumberIsSmall] ${headersLogInfo(headers)}")
      }

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

    if(importedBlocks.nonEmpty) {
      val howmany = importedBlocks.size

      if(howmany == 1) {
        val block = importedBlocks.head
        log.debug(s"Handled 1 new block ${block.idTag}")
      } else {
        val firstBlock = importedBlocks.last
        val lastBlock = importedBlocks.head
        log.info(s"Handled ${howmany} new blocks, from block ${firstBlock.idTag} to ${lastBlock.idTag}")
      }
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
        publishEvents(importedBlocks, Nil)
    }
  }

  private def importBlocks(blocks: List[Block], importedBlocks: List[Block] = Nil): (List[Block], Option[Any]) =
    importBlocks(1, blocks.size, blocks, importedBlocks)

  @tailrec
  private[this] def importBlocks(count: Int, initialBlocksSize: Int, blocks: List[Block], importedBlocks: List[Block]): (List[Block], Option[Any]) =
    blocks match {
      case Nil =>
        (importedBlocks, None)

      case block :: tail =>
        val importedSize = importedBlocks.size
        val importResult = RegularSync.tryImportBlock(
          context = s"importBlocks/$count/$initialBlocksSize/$importedSize",
          regularSync = this,
          block = block
        )

        importResult match {
          case Success(result) =>
            result match {
              case BlockImportedToTop(_, _) =>
                importBlocks(count + 1, initialBlocksSize, tail, block :: importedBlocks)

              case ChainReorganised(_, newBranch, _) =>
                importBlocks(count + 1, initialBlocksSize, tail, newBranch.reverse ::: importedBlocks)

              case r @ (DuplicateBlock | BlockEnqueued) =>
                importBlocks(count + 1, initialBlocksSize, tail, importedBlocks)

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

  private def publishEvents(newBlocks: Seq[Block], removedBlocks: Seq[Block]): Unit = {
    context.system.eventStream.publish(NewHead(removedBlocks, newBlocks))
  }
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

  // Utility method that tries to import the block and gives an opportunity for unified logging treatment.
  def tryImportBlock(context: String, regularSync: RegularSync, block: Block): Try[BlockImportResult] = {
    val log = regularSync.log
    val ledger = regularSync.ledger

    val triedResult = Try(ledger.importBlock(block))
    triedResult match {
      case Success(result) ⇒
        result match {
          case BlockImportedToTop(imported: List[Block], totalDifficulties: List[BigInt]) ⇒
          case BlockEnqueued ⇒
          case DuplicateBlock ⇒
          case ChainReorganised(oldBranch: List[Block], newBranch: List[Block], totalDifficulties: List[BigInt]) ⇒
          case bif @ BlockImportFailed(error: String) ⇒
            log.error(s"[$context] Block: ${block.idTag}. $bif")
          case UnknownParent ⇒
        }

      case Failure(t) ⇒
        log.error(s"Could not import block ${block.idTag}", t)
    }

    triedResult
  }
}
