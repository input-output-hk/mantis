package io.iohk.ethereum.network

import java.util.UUID

import akka.agent.Agent
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp.RLPEncoder

import scala.collection.immutable.Queue
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.network.p2p.validators.ForkValidator
import io.iohk.ethereum.utils.{BlockchainStatus, NodeStatus, Config}
import io.iohk.ethereum.utils.Config.FastSync._
import io.iohk.ethereum.network.FastSyncActor._
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, GetBlockBodies, GetBlockHeaders, BlockHeaders}
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.crypto._
import org.spongycastle.util.encoders.Hex

class FastSyncController(
    peerManager: ActorRef,
    nodeStatusHolder: Agent[NodeStatus],
    mptNodeStorage: MptNodeStorage,
    blockHeadersStorage: BlockHeadersStorage,
    blockBodiesStorage: BlockBodiesStorage,
    receiptStorage: ReceiptStorage,
    evmStorage: EvmCodeStorage)
  extends Actor with ActorLogging with BlacklistSupport {

  import FastSyncController._
  import Config.FastSync._
  import BlacklistSupport._
  import context.system

  var handshakedPeers: Seq[ActorRef] = Nil

  system.scheduler.schedule(0.seconds, peersScanInterval, peerManager, PeerManagerActor.GetPeers)

  override def receive: Receive = idle

  def idle: Receive = handlePeerUpdates orElse {
    case StartFastSync(targetBlockHash) =>
      peersToDownloadFrom match {
        case firstPeer :: _ =>
          log.info("Starting fast sync, asking peer {} for target block header", firstPeer.path.name)
          firstPeer ! PeerActor.Subscribe(Set(BlockHeaders.code))
          firstPeer ! PeerActor.SendMessage(GetBlockHeaders(Right(targetBlockHash), 1, 0, reverse = false))
          val timeout = system.scheduler.scheduleOnce(peerResponseTimeout, self, TargetBlockTimeout)
          context become waitingForTargetBlock(targetBlockHash, timeout)

        case Nil =>
          log.info("Cannot start fast sync, no peers to download from. Scheduling retry in {}", startRetryInterval)
          scheduleStartFastSync(startRetryInterval, targetBlockHash)
      }
  }

  def waitingForTargetBlock(targetBlockHash: ByteString, timeout: Cancellable): Receive = handlePeerUpdates orElse {
    case PeerActor.MessageReceived(blockHeaders: BlockHeaders) =>
      timeout.cancel()
      val targetBlockHeaderOpt = blockHeaders.headers.find(header => header.hash == targetBlockHash)
      targetBlockHeaderOpt match {
        case Some(targetBlockHeader) =>
          log.info("Received target block from peer, starting fast sync")
          val initialSyncState = SyncState(
            targetBlockHash = targetBlockHash,
            targetBlockNumber = targetBlockHeader.number,
            nodesToDownload = Queue(StateMptNodeHash(targetBlockHeader.stateRoot)))

          context.system.scheduler.schedule(0.seconds, printStatusInterval, self, PrintStatus)
          new SyncingHandler(initialSyncState).proceedSyncing()

        case None =>
          log.info("Peer ({}) did not respond with target block header, blacklisting and scheduling retry in {}",
            sender().path.name,
            startRetryInterval)

          blacklist(sender())
          sender() ! PeerActor.Unsubscribe
          scheduleStartFastSync(startRetryInterval, targetBlockHash)
      }

    case TargetBlockTimeout =>
      log.info("Peer ({}) did not respond with target block header (timeout), blacklisting and scheduling retry in {}",
        sender().path.name,
        startRetryInterval)

      blacklist(sender())
      sender() ! PeerActor.Unsubscribe
      scheduleStartFastSync(startRetryInterval, targetBlockHash)
  }

  def peersToDownloadFrom: Seq[ActorRef] = handshakedPeers.filterNot(isBlacklisted)

  def scheduleStartFastSync(interval: FiniteDuration, targetBlockHash: ByteString): Unit = {
    system.scheduler.scheduleOnce(interval, self, StartFastSync(targetBlockHash))
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

  class SyncingHandler(syncState: SyncState) {

    def becomeWithState(newState: SyncState): SyncingHandler = {
      val handler = new SyncingHandler(newState)
      context.become(handler.receive)
      handler
    }

    def receive: Receive =
      handlePeerUpdates orElse
      handleNodeDataMsg orElse
      handleBlockHeadersMsg orElse handleBlockBodiesMsg orElse handleReceiptsMsg orElse
      handleRequestTimeout orElse {

      case RequestNodes(hashes) =>
        val updatedSyncState = syncState.enqueueNodes(hashes)
        becomeWithState(updatedSyncState).proceedSyncing()

      case PrintStatus =>
        import syncState._
        log.info(
          s"""|Block: $currentBlockNumber/$targetBlockNumber.
              |Peers: ${busyPeers.size}/${handshakedPeers.size} (${blacklistedPeers.size} blacklisted).
              |State: $downloadedNodesCount/$knownNodesCount known nodes.""".stripMargin.replace("\n", " "))
  }

    def handleBlockHeadersMsg: Receive = {
      case PeerActor.MessageReceived(BlockHeaders(headers)) =>
        log.debug("Received {} block headers", headers.size)

        val requests = syncState.sentRequests.collect { case r: BlockHeadersRequest if r.peer == sender() => r }
        requests.foreach(_.timeout.cancel())

        val blockHashes = headers.map(_.hash)
        (blockHashes zip headers).foreach { case (hash, header) =>
          blockHeadersStorage.put(hash, header)
        }
        headers.lastOption.foreach { lastHeader =>
          val newBlockchainStatus = BlockchainStatus(lastHeader.difficulty, lastHeader.hash)
          nodeStatusHolder.send(_.copy(blockchainStatus = newBlockchainStatus))
        }

        val newBlockNumber = headers.map(_.number).fold(syncState.currentBlockNumber)(_ max _)

        val updatedSyncState = syncState
          .enqueueBlockBodies(blockHashes)
          .enqueueReceipts(blockHashes)
          .removeSentRequests(requests)
          .copy(currentBlockNumber = newBlockNumber)

        becomeWithState(updatedSyncState).proceedSyncing()
    }

    def handleBlockBodiesMsg: Receive = {
      case PeerActor.MessageReceived(BlockBodies(bodies)) =>
        log.debug("Received {} block bodies", bodies.size)

        val requests = syncState.sentRequests.collect { case r: BlockBodiesRequest if r.peer == sender() => r }
        requests.foreach(_.timeout.cancel())

        // TODO: is order always the same? can I get a hash from block body only?
        val requestedBlockBodies = requests.flatMap(_.sentMessage.hashes)
        (requestedBlockBodies zip bodies).foreach { case (hash, body) =>
          blockBodiesStorage.put(hash, body)
        }

        val remainingBlockBodies = requestedBlockBodies.drop(bodies.size)

        val updatedSyncState = syncState
            .enqueueBlockBodies(remainingBlockBodies)
            .removeSentRequests(requests)

        becomeWithState(updatedSyncState).proceedSyncing()
    }

    def handleReceiptsMsg: Receive = {
      case PeerActor.MessageReceived(Receipts(receiptsForBlocks)) =>
        log.debug("Received receipts for {} blocks", receiptsForBlocks.size)

        val requests = syncState.sentRequests.collect { case r: ReceiptsRequest if r.peer == sender() => r }
        requests.foreach(_.timeout.cancel())

        // TODO: is order always the same?
        val requestedReceipts = requests.flatMap(_.sentMessage.blockHashes)
        (requestedReceipts zip receiptsForBlocks) foreach { case (hash, receipts) =>
          receiptStorage.put(hash, receipts)
        }

        val remainingReceipts = requestedReceipts.drop(receiptsForBlocks.size)

        val updatedSyncState = syncState
            .enqueueReceipts(remainingReceipts)
            .removeSentRequests(requests)

        becomeWithState(updatedSyncState).proceedSyncing()
    }

    private def handleRequestTimeout: Receive = {
      case RequestTimeout(requestId) =>
        val requestOpt = syncState.sentRequests.find(_.id == requestId)
        requestOpt foreach { request =>
          log.info("The request {} has timed out. Blacklisting peer {} for {}",
            request.sentMessage,
            request.peer.path.name,
            blacklistDuration)

          request.timeout.cancel()
          blacklist(request.peer)

          val updatedSyncState = (request match {
            case nodesReq: NodesRequest => syncState.enqueueNodes(nodesReq.hashes)
            case headersReq: BlockHeadersRequest => syncState
            case bodiesReq: BlockBodiesRequest => syncState.enqueueBlockBodies(bodiesReq.sentMessage.hashes)
            case receiptsReq: ReceiptsRequest => syncState.enqueueReceipts(receiptsReq.sentMessage.blockHashes)
          }).copy(sentRequests = syncState.sentRequests.filterNot(_.id == requestId))

          becomeWithState(updatedSyncState).proceedSyncing()
        }
    }

    private def handleNodeDataMsg: Receive = {
      case PeerActor.MessageReceived(nodeData: NodeData) =>
        log.debug("Received {} state nodes", nodeData.values.size)

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
        becomeWithState(updatedSyncState).proceedSyncing()
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
          log.debug("Got branch node: {}", n)
          val hashes = n.children.collect { case Left(MptHash(childHash)) => childHash }.filter(_.nonEmpty)
          self ! RequestNodes(hashes.map(StateMptNodeHash))
          mptNodeStorage.put(n)

        case n: MptExtension =>
          log.debug("Got extension node: {}", n)
          n.child.fold(
            { case MptHash(nodeHash) =>
              self ! RequestNodes(Seq(StateMptNodeHash(nodeHash)))
            }, { case MptValue(value) =>
              log.debug("Got value in extension node: ", Hex.toHexString(value.toArray[Byte]))
            })
          mptNodeStorage.put(n)
      }
    }

    private def handleContractMptNode(hash: ByteString, mptNode: MptNode) = {
      mptNode match {
        case n: MptLeaf =>
          log.debug("Got contract leaf node: {}", n)
          mptNodeStorage.put(n)

        case n: MptBranch =>
          log.debug("Got contract branch node: {}", n)
          val hashes = n.children.collect { case Left(MptHash(childHash)) => childHash }.filter(_.nonEmpty)
          self ! RequestNodes(hashes.map(ContractStorageMptNodeHash))
          mptNodeStorage.put(n)

        case n: MptExtension =>
          log.debug("Got contract extension node: {}", n)
          n.child.fold(
            { case MptHash(nodeHash) =>
              self ! RequestNodes(Seq(ContractStorageMptNodeHash(nodeHash)))
            }, { case MptValue(value) =>
              log.debug("Got contract value in extension node: ", Hex.toHexString(value.toArray[Byte]))
            })
          mptNodeStorage.put(n)
      }
    }

    // scalastyle:off
    def proceedSyncing(): Unit = {
      if (syncState.finishedSyncing) {
        log.info("Fast sync finished")
        context stop self
      } else {
        if (syncState.hasAnythingToDownload) {
          if (availablePeers.isEmpty) {
            if (busyPeers.nonEmpty) {
              log.debug("There are no available peers, waiting for responses")
            } else {
              log.debug("There are no peers to download from, scheduling a retry in {}", downloadRetryInterval)
              // schedule a retry
            }
          } else {
            val remainingRequests = maxConcurrentRequests - syncState.sentRequests.size
            val requestsNumToSend = Math.min(remainingRequests, availablePeers.size)

            val newState = (0 until requestsNumToSend).foldLeft(syncState) { case (state, idx) =>
              val peer = availablePeers(idx)

              def createAndSendRequest[T <: Message : RLPEncoder](requestFn: (String, Cancellable) => SentRequest[T]) = {
                val requestId = UUID.randomUUID().toString
                val timeout = context.system.scheduler.scheduleOnce(peerResponseTimeout, self, RequestTimeout(requestId))
                val request = requestFn(requestId, timeout)
                request.peer ! PeerActor.SendMessage(request.sentMessage)
                request.peer ! PeerActor.Subscribe(Set(request.sentMessage.code))
                state.copy(sentRequests = state.sentRequests :+ request)
              }

              if (state.receiptsToDownload.nonEmpty) {
                val msg = GetReceipts(state.receiptsToDownload.take(receiptsPerRequest))
                createAndSendRequest { (id, timeout) => ReceiptsRequest(id, peer, timeout, msg)
                }.copy(receiptsToDownload = state.receiptsToDownload.drop(receiptsPerRequest))
              } else if (state.blocksBodiesToDownload.nonEmpty) {
                val msg = GetBlockBodies(state.blocksBodiesToDownload.take(blockBodiesPerRequest))
                createAndSendRequest { (id, timeout) => BlockBodiesRequest(id, peer, timeout, msg)
                }.copy(blocksBodiesToDownload = state.blocksBodiesToDownload.drop(blockBodiesPerRequest))
              } else if (state.sentRequests.collect { case r: BlockHeadersRequest => r }.isEmpty) {
                val msg = GetBlockHeaders(Left(state.currentBlockNumber + 1), blockHeadersPerRequest, 0, reverse = false)
                createAndSendRequest { (id, timeout) => BlockHeadersRequest(id, peer, timeout, msg) }
              } else if (state.nodesToDownload.nonEmpty) {
                val nodesToGet = state.nodesToDownload.take(nodesPerRequest)
                val msg = GetNodeData(nodesToGet.map(_.v))
                createAndSendRequest { (id, timeout) => NodesRequest(id, peer, timeout, msg, nodesToGet)
                }.copy(nodesToDownload = state.nodesToDownload.drop(nodesPerRequest))
              } else {
                state
              }
            }

            becomeWithState(newState)
          }

        } else {
          log.debug("No more items to request, waiting for {} responses", syncState.sentRequests.size)
        }
      }
    }

    def busyPeers: Seq[ActorRef] = syncState.sentRequests.map(_.peer)

    def availablePeers: Seq[ActorRef] = peersToDownloadFrom.filterNot(busyPeers.contains)
  }

}

object FastSyncController {

  val EmptyAccountStorageHash = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"))
  val EmptyAccountEvmCodeHash = ByteString(Hex.decode("c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

  case class StartFastSync(targetBlockHash: ByteString)

  private case object PrintStatus
  private case object TargetBlockTimeout
  private case class RequestTimeout(requestId: String)
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

  case class BlockBodiesRequest(
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

    def hasAnythingToDownload: Boolean =
      nodesToDownload.nonEmpty ||
      blocksBodiesToDownload.nonEmpty ||
      receiptsToDownload.nonEmpty

    def finishedSyncing: Boolean =
      currentBlockNumber == targetBlockNumber &&
      !hasAnythingToDownload &&
      sentRequests.isEmpty

    def isReceiptAdded(hash: ByteString): Boolean = {
      receiptsToDownload.contains(hash) ||
      sentRequests.exists {
        case r: ReceiptsRequest => r.sentMessage.blockHashes.contains(hash)
        case _ => false
      }
    }

    def enqueueReceipts(blockHashes: Seq[ByteString]): SyncState =
      copy(receiptsToDownload = receiptsToDownload ++ blockHashes.filterNot(isReceiptAdded))

    def isBlockBodyAdded(hash: ByteString): Boolean = {
      blocksBodiesToDownload.contains(hash) ||
      sentRequests.exists {
        case r: BlockBodiesRequest => r.sentMessage.hashes.contains(hash)
        case _ => false
      }
    }

    def enqueueBlockBodies(blockHashes: Seq[ByteString]): SyncState =
      copy(blocksBodiesToDownload = blocksBodiesToDownload ++ blockHashes.filterNot(isBlockBodyAdded))

    def isNodeAdded(hash: HashType): Boolean = {
      nodesToDownload.contains(hash) ||
      sentRequests.exists {
        case r: NodesRequest => r.hashes.contains(hash)
        case _ => false
      }
    }

    def enqueueNodes(hashes: Seq[HashType]): SyncState = {
      copy(nodesToDownload = nodesToDownload ++ hashes.filterNot(isNodeAdded))
    }

    def removeSentRequests(requestsToRemove: Seq[SentRequest[_]]) =
      copy(sentRequests = sentRequests.filterNot(requestsToRemove.contains))

    def knownNodesCount =
      downloadedNodesCount + nodesToDownload.size + sentRequests.collect { case r: NodesRequest => r.hashes.size }.sum

    override def toString = {
      s"""
         |SyncState(
         |  current block: $currentBlockNumber,
         |  target block: $targetBlockNumber,
         |  ${sentRequests.size} sent requests,
         |  ${nodesToDownload.size} nodes queued,
         |  ${blocksBodiesToDownload.size} block bodies queued,
         |  ${receiptsToDownload.size} receipts queued)""".stripMargin
    }
  }

}

trait BlacklistSupport {
  selfActor: Actor =>

  import BlacklistSupport._
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

object BlacklistSupport {
  case class UnblacklistPeer(peer: ActorRef)
}
