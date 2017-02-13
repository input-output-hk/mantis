package io.iohk.ethereum.network

import java.util.UUID

import scala.collection.immutable.Queue
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.network.FastSyncController.UnblacklistPeer
import io.iohk.ethereum.network.p2p.validators.ForkValidator
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Config.FastSync._
import io.iohk.ethereum.network.FastSyncActor._
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, GetBlockBodies, GetBlockHeaders, BlockHeaders}
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.crypto._
import org.spongycastle.util.encoders.Hex

class FastSyncController(
    peerManager: ActorRef,
    mptNodeStorage: MptNodeStorage,
    blockHeadersStorage: BlockHeadersStorage,
    blockBodiesStorage: BlockBodiesStorage,
    receiptStorage: ReceiptStorage,
    evmStorage: EvmCodeStorage)
  extends Actor with ActorLogging with BlacklistSupport {

  import FastSyncController._
  import Config.FastSync._
  import context.system

  var handshakedPeers: Seq[ActorRef] = Nil

  val EmptyAccountStorageHash = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"))
  val EmptyAccountEvmCodeHash = ByteString(Hex.decode("c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

  system.scheduler.schedule(0.seconds, 30.seconds, peerManager, PeerManagerActor.GetPeers)

  def peersToDownloadFrom: Seq[ActorRef] = handshakedPeers.filterNot(isBlacklisted)

  override def receive: Receive = idle

  def idle: Receive = handlePeerUpdates orElse {
    case StartFastSync(targetBlockHash) =>
      peersToDownloadFrom match {
        case firstPeer :: Nil =>
          log.info("Starting fast sync, asking peer {} for target block header", firstPeer.path.name)
          firstPeer ! PeerActor.Subscribe(Set(BlockHeaders.code))
          firstPeer ! PeerActor.SendMessage(GetBlockHeaders(Right(targetBlockHash), 1, 0, reverse = false))
          val timeout = system.scheduler.scheduleOnce(30.seconds, self, Timeout)
          context become waitingForTargetBlock(targetBlockHash, timeout)

        case Nil =>
          log.info("Cannot start fast sync, no peers to download from. Scheduling retry in {}", startRetryInterval)
          scheduleStartFastSync(startRetryInterval, targetBlockHash)
      }
  }

  def handlePeerUpdates: Receive = {
    case PeerManagerActor.PeersResponse(peers) =>
      peers.foreach { peer =>
        peer.ref ! PeerActor.GetStatus
        context watch peer.ref
      }

    case PeerActor.StatusResponse(status) =>
      if (status == PeerActor.Status.Handshaked) {
        if (!handshakedPeers.contains(sender()) && !isBlacklisted(sender())) {
          handshakedPeers :+= sender()
        }
      } else removePeer(sender())

    case Terminated(ref) =>
      removePeer(ref)

    case UnblacklistPeer(ref) =>
      undoBlacklist(ref)
  }

  def removePeer(peer: ActorRef): Unit = {
    context.unwatch(peer)
    undoBlacklist(peer)
    handshakedPeers = handshakedPeers.filterNot(_ == peer)
  }

  def waitingForTargetBlock(targetBlockHash: ByteString, timeout: Cancellable): Receive = handlePeerUpdates orElse {
    case PeerActor.MessageReceived(blockHeaders: BlockHeaders) =>
      timeout.cancel()
      val targetBlockHeaderOpt = blockHeaders.headers.find(header => ForkValidator.hash(header) == targetBlockHash)
      targetBlockHeaderOpt match {
        case Some(targetBlockHeader) =>
          log.info("Received target block from peer, starting fast sync")
          val initialSyncState = SyncState(
            targetBlockHash = targetBlockHash,
            targetBlockNumber = targetBlockHeader.number,
            nodesToDownload = Queue(StateMptNodeHash(targetBlockHeader.stateRoot)))

          context.system.scheduler.schedule(2.seconds, 2.seconds, self, "print")
          new SyncingHandler(initialSyncState).processAndBecome()

        case None =>
          log.info("Peer ({}) did not respond with target block header, blacklisting and scheduling retry in {}",
            sender().path.name,
            startRetryInterval)

          blacklist(sender())
          sender() ! PeerActor.Unsubscribe
          scheduleStartFastSync(2.minutes, targetBlockHash)
      }

    case Timeout =>
      log.info("Peer ({}) did not respond with target block header (timeout), blacklisting and scheduling retry in {}",
        sender().path.name,
        startRetryInterval)

      blacklist(sender())
      sender() ! PeerActor.Unsubscribe
      scheduleStartFastSync(startRetryInterval, targetBlockHash)
  }

  def scheduleStartFastSync(interval: FiniteDuration, targetBlockHash: ByteString): Unit = {
    system.scheduler.scheduleOnce(interval, self, StartFastSync(targetBlockHash))
  }

  class SyncingHandler(syncState: SyncState) {

    def processAndBecome(): Unit = {
      becomeWithState(processSyncState(syncState))
    }

    private def becomeWithState(newState: SyncState) =
      context.become(new SyncingHandler(newState).receive)

    // scalastyle:off
    def receive: Receive = handleNodeDataMsg(syncState) orElse handlePeerUpdates orElse {
      case PeerActor.MessageReceived(blockHeaders: BlockHeaders) =>
        log.debug("Received {} block headers", blockHeaders.headers.size)

        val blockHashes = blockHeaders.headers.map(ForkValidator.hash) // todo move this methode from ForkValidator
        (blockHashes zip blockHeaders.headers).foreach { case (hash, header) => blockHeadersStorage.put(hash, header) }
        val requests = syncState.sentBlockHeadersRequests.filter(_.peer == sender())
        requests.foreach(_.timeout.cancel())

        val updatedSyncState = syncState
          .enqueueBlockBodies(blockHashes)
          .enqueueReceipts(blockHashes)
          .copy(sentRequests = syncState.sentRequests.filterNot(requests.contains))
          .copy(currentBlockNumber = blockHeaders.headers.lastOption.map(_.number).getOrElse(syncState.currentBlockNumber))

        becomeWithState(processSyncState(updatedSyncState))

      case PeerActor.MessageReceived(blockBodies: BlockBodies) =>
        log.debug("Received {} block bodies", blockBodies.bodies.size)

        val senderRequests = syncState.sentRequests.collect { case br: BlocksRequest if br.peer == sender() => br }
        val requestedBodies = senderRequests.flatMap(_.sentMessage.hashes)
        senderRequests.foreach(_.timeout.cancel())

        (requestedBodies zip blockBodies.bodies).foreach { case (hash, body) => blockBodiesStorage.put(hash, body) }

        // TODO: what if we receive less block bodies than requested? how do we know which ones to request again? seq order?

        val updatedSyncState = syncState
          .copy(sentRequests = syncState.sentRequests.filterNot(senderRequests.contains))

        becomeWithState(processSyncState(updatedSyncState))

      case PeerActor.MessageReceived(receipts: Receipts) =>
        log.debug("Received receipts for {} blocks", receipts.receiptsForBlocks.size)
        val senderRequests = syncState.sentRequests.collect { case rr: ReceiptsRequest if rr.peer == sender() => rr }
        val requestedReceipts = senderRequests.flatMap(_.sentMessage.blockHashes)
        (requestedReceipts zip receipts.receiptsForBlocks) foreach { case (hash, rec) => receiptStorage.put(hash, rec) } // does it always work!?
        senderRequests.foreach(_.timeout.cancel())

        val updatedSyncState = syncState
          .copy(sentRequests = syncState.sentRequests.filterNot(senderRequests.contains))

        becomeWithState(processSyncState(updatedSyncState))


      case RequestTimeout(requestId) =>
        val requestOpt = syncState.sentRequests.find(_.id == requestId)
        requestOpt foreach { request =>
          log.debug("The request {} has timed out. Blacklisting peer {} for {}", request.sentMessage, request.peer.path.name, blacklistDuration)
          request.timeout.cancel()
          blacklist(request.peer)
          val updatedSyncState = (request match {
            case nodesReq: NodesRequest => syncState.enqueueNodes(nodesReq.hashes)
            case headersReq: BlockHeadersRequest => syncState
            case bodiesReq: BlocksRequest => syncState.enqueueBlockBodies(bodiesReq.sentMessage.hashes)
            case receiptsReq: ReceiptsRequest => syncState.enqueueReceipts(receiptsReq.sentMessage.blockHashes)
          }).copy(sentRequests = syncState.sentRequests.filterNot(_.id == requestId))
          becomeWithState(processSyncState(updatedSyncState))

        }

      case RequestNodes(hashes) =>
        val nodesToEnqueue = hashes.filterNot(hash =>
          syncState.nodesToDownload.contains(hash) ||
            syncState.sentRequests.exists {
              case nodesReq: NodesRequest => nodesReq.hashes.contains(hash)
              case _ => false
            }
        )
        val updatedSyncState = syncState.enqueueNodes(nodesToEnqueue)
        becomeWithState(processSyncState(updatedSyncState))

      case "print" =>
        val knownNodesCount =
          syncState.downloadedNodesCount +
          syncState.nodesToDownload.size +
          syncState.sentRequests.collect { case nr: NodesRequest => nr.hashes.size }.sum

        log.info(
          s"""
             |Block: ${syncState.currentBlockNumber}/${syncState.targetBlockNumber}.
             |Peers: ${busyPeers(syncState).size}/${peersToDownloadFrom.size} (${blacklistedPeers.size} blacklisted).
             |State: ${syncState.downloadedNodesCount}/$knownNodesCount known nodes.
           """.stripMargin.replace("\n", " "))
    }


    private def handleNodeDataMsg(syncState: SyncState): Receive = {
      case PeerActor.MessageReceived(nodeData: NodeData) =>
        log.info("Received {} mpt nodes", nodeData.values.size)

        val peerRequests: Seq[NodesRequest] = syncState.sentRequests.collect {
          case req: NodesRequest if req.peer == sender() => req
        }

        val requestedHashes = peerRequests.flatMap(_.hashes)
        val receivedHashes = nodeData.values.map(v => ByteString(sha3(v.toArray[Byte])))

        val remainingHashes = requestedHashes.filterNot(h => receivedHashes.contains(h.v))
        if (remainingHashes.nonEmpty) {
          self ! RequestNodes(remainingHashes)
          blacklist(sender())
        }

        nodeData.values.indices foreach { idx =>
          requestedHashes.find(_.v == ByteString(sha3(nodeData.values(idx).toArray[Byte]))) foreach {
            case StateMptNodeHash(hash) =>
              handleMptNode(hash, nodeData.getMptNode(idx))

            case ContractStorageMptNodeHash(hash) =>
              handleContractMptNode(hash, nodeData.getMptNode(idx))

            case EvmCodeHash(hash) =>
              val evmCode = nodeData.values(idx)
              evmStorage.put(hash, evmCode)

            case StorageRootHash(hash) =>
              val rootNode = nodeData.getMptNode(idx)
              handleContractMptNode(hash, rootNode)
          }
        }

        val nodesRequests = peerRequests.filter(_.hashes.exists(h => receivedHashes.contains(h.v)))
        nodesRequests.foreach(_.timeout.cancel())
        val updatedSyncState = syncState.copy(
          sentRequests = syncState.sentRequests.filterNot(nodesRequests.contains),
          downloadedNodesCount = syncState.downloadedNodesCount + nodeData.values.size)
        becomeWithState(processSyncState(updatedSyncState))

    }

    private def handleMptNode(hash: ByteString, mptNode: MptNode) = {
      mptNode match {
        case n: MptLeaf =>
          val evm = n.getAccount.codeHash
          val storage = n.getAccount.storageRoot

          if (evm != EmptyAccountEvmCodeHash) {
            self ! RequestNodes(Seq(EvmCodeHash(evm)))
          }

          if (storage != EmptyAccountStorageHash) {
            self ! RequestNodes(Seq(StorageRootHash(storage)))
          }

          mptNodeStorage.put(n)

        case n: MptBranch =>
          log.info("Got branch node: {}", n)
          val hashes = n.children.collect { case Left(MptHash(childHash)) => childHash }.filter(_.nonEmpty)
          self ! RequestNodes(hashes.map(StateMptNodeHash))
          mptNodeStorage.put(n)

        case n: MptExtension =>
          log.info("Got extension node: {}", n)
          n.child.fold(
            { case MptHash(nodeHash) =>
              self ! RequestNodes(Seq(StateMptNodeHash(nodeHash)))
            }, { case MptValue(value) =>
              log.info("Got value in extension node: ", Hex.toHexString(value.toArray[Byte]))
            })
          mptNodeStorage.put(n)
      }
    }

    private def handleContractMptNode(hash: ByteString, mptNode: MptNode) = {
      mptNode match {
        case n: MptLeaf =>
          log.info("Got contract leaf node: {}", n)
          mptNodeStorage.put(n)

        case n: MptBranch =>
          log.info("Got contract branch node: {}", n)
          val hashes = n.children.collect { case Left(MptHash(childHash)) => childHash }.filter(_.nonEmpty)
          println(hashes)
          self ! RequestNodes(hashes.map(ContractStorageMptNodeHash))
          mptNodeStorage.put(n)

        case n: MptExtension =>
          log.info("Got contract extension node: {}", n)
          n.child.fold(
            { case MptHash(nodeHash) =>
              self ! RequestNodes(Seq(ContractStorageMptNodeHash(nodeHash)))
            }, { case MptValue(value) =>
              log.info("Got contract value in extension node: ", Hex.toHexString(value.toArray[Byte]))
            })
          mptNodeStorage.put(n)
      }
    }

    // scalastyle:off
    private def processSyncState(syncState: SyncState): SyncState = {
      if (syncState.finished) {
        log.info("Fast sync finished")
        context stop self
        syncState
      } else {
        // don't forget to subscribe
        if (syncState.hasAnythingToDownload) {
          log.debug("Using {} peers, {} still available ({} blacklisted).", busyPeers(syncState).size, availablePeers(syncState).size, blacklistedPeers.size)

          if (availablePeers(syncState).isEmpty) {
            if (busyPeers(syncState).nonEmpty) {
              log.debug("There are no available peers, waiting for responses")
              syncState
            } else {
              log.debug("There are no peers to download from, scheduling a retry in {}", downloadRetryInterval)
              // schedule a retry
              syncState
            }
          } else {
            val remainingRequests = maxConcurrentRequests - syncState.sentRequests.size
            val requestsNumToSend = Math.min(remainingRequests, availablePeers(syncState).size)

            (0 until requestsNumToSend).foldLeft(syncState) { case (state, idx) =>
              val peer = availablePeers(syncState)(idx)

              if (state.receiptsToDownload.nonEmpty) {
                val msg = GetReceipts(state.receiptsToDownload.take(10)) // 10 do zmiennej
                val requestId = UUID.randomUUID().toString
                val timeout = context.system.scheduler.scheduleOnce(30.seconds, self, RequestTimeout(requestId))
                val receiptsRequest = ReceiptsRequest(requestId, peer, timeout, msg)
                peer ! PeerActor.SendMessage(msg)
                peer ! PeerActor.Subscribe(Set(Receipts.code))
                state.copy(sentRequests = state.sentRequests :+ receiptsRequest, receiptsToDownload = state.receiptsToDownload.drop(10)) // drop 10 do zmiennej
              } else if (state.blocksBodiesToDownload.nonEmpty) {
                val msg = GetBlockBodies(state.blocksBodiesToDownload.take(10)) // 10 do zmiennej
                val requestId = UUID.randomUUID().toString
                val timeout = context.system.scheduler.scheduleOnce(30.seconds, self, RequestTimeout(requestId))
                val blocksRequest = BlocksRequest(requestId, peer, timeout, msg)
                peer ! PeerActor.SendMessage(msg)
                peer ! PeerActor.Subscribe(Set(BlockBodies.code))
                state.copy(sentRequests = state.sentRequests :+ blocksRequest, blocksBodiesToDownload = state.blocksBodiesToDownload.drop(10)) // drop 10 do zmiennej
              } else if (state.sentBlockHeadersRequests.isEmpty) {
                val msg = GetBlockHeaders(Left(state.currentBlockNumber + 1), 10, 0, reverse = false) // max 10 do zmiennej
                val requestId = UUID.randomUUID().toString
                val timeout = context.system.scheduler.scheduleOnce(30.seconds, self, RequestTimeout(requestId))
                val blockHeadersRequest = BlockHeadersRequest(requestId, peer, timeout, msg)
                peer ! PeerActor.SendMessage(msg)
                peer ! PeerActor.Subscribe(Set(BlockHeaders.code))
                state.copy(sentRequests = state.sentRequests :+ blockHeadersRequest)
              } else if (state.nodesToDownload.nonEmpty) {
                val nodesToGet = state.nodesToDownload.take(10)
                val msg = GetNodeData(nodesToGet.map(_.v)) // max 10 do zmiennej
                val requestId = UUID.randomUUID().toString
                val timeout = context.system.scheduler.scheduleOnce(30.seconds, self, RequestTimeout(requestId))
                val nodesRequest = NodesRequest(requestId, peer, timeout, msg, nodesToGet)
                peer ! PeerActor.SendMessage(msg)
                peer ! PeerActor.Subscribe(Set(NodeData.code))
                state.copy(sentRequests = state.sentRequests :+ nodesRequest, nodesToDownload = state.nodesToDownload.drop(10))
              } else {
                state
              }
            }
          }

        } else {
          log.debug("No more items to request, waiting for {} responses", syncState.sentRequests.size)
          syncState
        }
      }
    }

    // to powinna ubywac stanu ktory sie mu przekaze.. lol
    def busyPeers(syncState: SyncState) = syncState.sentRequests.map(_.peer)

    def availablePeers(syncState: SyncState) = peersToDownloadFrom.filterNot(busyPeers(syncState).contains)
  }

}

object FastSyncController {
  case class StartFastSync(targetBlockHash: ByteString)

  case class UnblacklistPeer(peer: ActorRef)

  private case object Timeout
  case class RequestTimeout(requestId: String)

  private case class RequestNodes(hashes: Seq[HashType])

  sealed trait SentRequest[Msg] {
    def id: String
    def peer: ActorRef
    def timeout: Cancellable
    def sentMessage: Msg
  }

  case class NodesRequest(
      override val id: String,
      override val peer: ActorRef,
      override val timeout: Cancellable,
      override val sentMessage: GetNodeData,
      hashes: Seq[HashType])
    extends SentRequest[GetNodeData]

  case class BlockHeadersRequest(
                                  override val id: String,

                                  override val peer: ActorRef,
      override val timeout: Cancellable,
      override val sentMessage: GetBlockHeaders)
    extends SentRequest[GetBlockHeaders]

  case class BlocksRequest(
                            override val id: String,
      override val peer: ActorRef,
      override val timeout: Cancellable,
      override val sentMessage: GetBlockBodies)
    extends SentRequest[GetBlockBodies]

  case class ReceiptsRequest(
                              override val id: String,
      override val peer: ActorRef,
      override val timeout: Cancellable,
      override val sentMessage: GetReceipts)
    extends SentRequest[GetReceipts]

  case class SyncState(
      targetBlockHash: ByteString,
      targetBlockNumber: BigInt,
      currentBlockNumber: BigInt = 0,
      sentRequests: Seq[SentRequest[_]] = Nil,
      nodesToDownload: Queue[HashType] = Queue(),
      blocksBodiesToDownload: Queue[ByteString] = Queue(),
      receiptsToDownload: Queue[ByteString] = Queue(),
      downloadedNodesCount: Int = 0) {

    def sentBlockHeadersRequests: Seq[BlockHeadersRequest] =
      sentRequests.collect { case req: BlockHeadersRequest => req }

    def hasAnythingToDownload: Boolean =
      nodesToDownload.nonEmpty ||
      blocksBodiesToDownload.nonEmpty ||
      receiptsToDownload.nonEmpty

    def finished: Boolean =
      currentBlockNumber == targetBlockNumber &&
        !hasAnythingToDownload &&
        sentRequests.isEmpty

    def enqueueReceipts(blockHashes: Seq[ByteString]): SyncState =
      copy(receiptsToDownload = receiptsToDownload ++ blockHashes.filterNot(receiptsToDownload.contains))

    def enqueueBlockBodies(blockHashes: Seq[ByteString]): SyncState =
      copy(blocksBodiesToDownload = blocksBodiesToDownload ++ blockHashes.filterNot(blocksBodiesToDownload.contains))

    def enqueueNodes(hashes: Seq[HashType]): SyncState =
      copy(nodesToDownload = nodesToDownload ++ hashes.filterNot(nodesToDownload.contains))

    override def toString = {
      s"""
         |SyncState(
         |  current block: $currentBlockNumber,
         |  target block: $targetBlockNumber,
         |  ${sentRequests.size} sent requests,
         |  ${nodesToDownload.size} state nodes queued,
         |  ${blocksBodiesToDownload.size} block bodies queued,
         |  ${receiptsToDownload.size} receipts queued)
       """.stripMargin
    }

  }
}

trait BlacklistSupport {
  selfActor: Actor =>

  import context.system

  var blacklistedPeers: Seq[(ActorRef, Cancellable)] = Nil

  def blacklist(peer: ActorRef): Unit = {
    undoBlacklist(peer)
    val unblacklistCancellable = system.scheduler.scheduleOnce(blacklistDuration, self, UnblacklistPeer(peer))
    blacklistedPeers :+= (peer, unblacklistCancellable)
  }

  def undoBlacklist(peer: ActorRef): Unit = {
    blacklistedPeers.find(_._1 == peer).foreach(_._2.cancel())
    blacklistedPeers = blacklistedPeers.filterNot(_._1 == peer)
  }

  def isBlacklisted(peer: ActorRef): Boolean =
    blacklistedPeers.exists(_._1 == peer)
}
