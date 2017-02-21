package io.iohk.ethereum.blockchain.sync

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.{PeerActor, PeerManagerActor}
import io.iohk.ethereum.utils.{Config, NodeStatus}

class FastSyncController(
    peerManager: ActorRef,
    nodeStatusHolder: Agent[NodeStatus],
    blockchain: Blockchain,
    mptNodeStorage: MptNodeStorage,
    externalSchedulerOpt: Option[Scheduler] = None)
  extends Actor with ActorLogging with BlacklistSupport {

  import BlacklistSupport._
  import Config.FastSync._
  import FastSyncController._

  override val supervisorStrategy =
    OneForOneStrategy() {
      case _ => Stop
    }

  var handshakedPeers: Set[ActorRef] = Set.empty

  scheduler.schedule(0.seconds, peersScanInterval, peerManager, PeerManagerActor.GetPeers)

  override implicit def scheduler: Scheduler = externalSchedulerOpt getOrElse context.system.scheduler

  override def receive: Receive = idle

  def idle: Receive = handlePeerUpdates orElse {
    case StartFastSync(targetBlockHash) =>
      peersToDownloadFrom.headOption match {
        case Some(firstPeer) =>
          log.info("Starting fast sync, asking peer {} for target block header", firstPeer.path.name)
          firstPeer ! PeerActor.Subscribe(Set(BlockHeaders.code))
          firstPeer ! PeerActor.SendMessage(GetBlockHeaders(Right(targetBlockHash), 1, 0, reverse = false))
          val timeout = scheduler.scheduleOnce(peerResponseTimeout, self, TargetBlockTimeout)
          context become waitingForTargetBlock(firstPeer, targetBlockHash, timeout)

        case None =>
          log.info("Cannot start fast sync, no peers to download from. Scheduling retry in {}", startRetryInterval)
          scheduleStartFastSync(startRetryInterval, targetBlockHash)
      }
  }

  def waitingForTargetBlock(peer: ActorRef,
                            targetBlockHash: ByteString,
                            timeout: Cancellable): Receive =handlePeerUpdates orElse {
    case PeerActor.MessageReceived(blockHeaders: BlockHeaders) =>
      timeout.cancel()
      peer ! PeerActor.Unsubscribe

      val targetBlockHeaderOpt = blockHeaders.headers.find(header => header.hash == targetBlockHash)
      targetBlockHeaderOpt match {
        case Some(targetBlockHeader) =>
          log.info("Received target block from peer, starting fast sync")

          scheduler.schedule(0.seconds, printStatusInterval, self, PrintStatus)
          context become new SyncingHandler(targetBlockHeader).receive
          self ! EnqueueNodes(Seq(StateMptNodeHash(targetBlockHeader.stateRoot)))
          self ! ProcessSyncing

        case None =>
          log.info("Peer ({}) did not respond with target block header, blacklisting and scheduling retry in {}",
            sender().path.name,
            startRetryInterval)

          blacklist(sender(), blacklistDuration)
          scheduleStartFastSync(startRetryInterval, targetBlockHash)
          context become idle
      }

    case TargetBlockTimeout =>
      log.info("Peer ({}) did not respond with target block header (timeout), blacklisting and scheduling retry in {}",
        sender().path.name,
        startRetryInterval)

      blacklist(sender(), blacklistDuration)
      peer ! PeerActor.Unsubscribe
      scheduleStartFastSync(startRetryInterval, targetBlockHash)
      context become idle
  }

  def peersToDownloadFrom: Set[ActorRef] = handshakedPeers.filterNot(isBlacklisted)

  def scheduleStartFastSync(interval: FiniteDuration, targetBlockHash: ByteString): Unit = {
    scheduler.scheduleOnce(interval, self, StartFastSync(targetBlockHash))
  }

  def handlePeerUpdates: Receive = {
    case PeerManagerActor.PeersResponse(peers) =>
      peers.foreach(_.ref ! PeerActor.GetStatus)

    case PeerActor.StatusResponse(status) =>
      if (status == PeerActor.Status.Handshaked) {
        if (!handshakedPeers.contains(sender()) && !isBlacklisted(sender())) {
          handshakedPeers += sender()
          context watch sender()
        }
      } else removePeer(sender())

    case Terminated(ref) if handshakedPeers.contains(ref) =>
      removePeer(ref)

    case BlacklistPeer(ref) =>
      blacklist(ref, blacklistDuration)

    case UnblacklistPeer(ref) =>
      undoBlacklist(ref)
  }

  def removePeer(peer: ActorRef): Unit = {
    context.unwatch(peer)
    undoBlacklist(peer)
    handshakedPeers -= peer
  }

  class SyncingHandler(targetBlock: BlockHeader) {

    private val blockHeadersHandlerName = "block-headers-request-handler"

    private var nonMptNodesQueue: Set[HashType] = Set.empty
    private var mptNodesQueue: Set[HashType] = Set.empty

    private var blockBodiesQueue: Set[ByteString] = Set.empty
    private var receiptsQueue: Set[ByteString] = Set.empty

    private var downloadedNodesCount: Int = 0

    private var assignedHandlers: Map[ActorRef, ActorRef] = Map.empty

    def receive: Receive = handlePeerUpdates orElse {
      case EnqueueNodes(hashes) =>
        hashes.foreach {
          case h: EvmCodeHash => nonMptNodesQueue += h
          case h: StorageRootHash => nonMptNodesQueue += h
          case h: StateMptNodeHash => mptNodesQueue += h
          case h: ContractStorageMptNodeHash => mptNodesQueue += h
        }

      case EnqueueBlockBodies(hashes) =>
        blockBodiesQueue ++= hashes

      case EnqueueReceipts(hashes) =>
        receiptsQueue ++= hashes

      case UpdateDownloadedNodesCount(num) =>
        downloadedNodesCount += num

      case ProcessSyncing =>
        processSyncing()

      case FastSyncRequestHandler.Done =>
        context unwatch sender()
        assignedHandlers -= sender()
        processSyncing()

      case Terminated(ref) if assignedHandlers.contains(ref) =>
        context unwatch ref
        assignedHandlers -= ref

      case PrintStatus =>
        val totalNodesCount = downloadedNodesCount + mptNodesQueue.size + nonMptNodesQueue.size
        log.info(
          s"""|Block: ${nodeStatusHolder().blockchainStatus.bestNumber}/${targetBlock.number}.
              |Peers: ${assignedHandlers.size}/${handshakedPeers.size} (${blacklistedPeers.size} blacklisted).
              |State: $downloadedNodesCount/$totalNodesCount known nodes.""".stripMargin.replace("\n", " "))
    }

    def processSyncing(): Unit = {
      if (fullySynced) finish()
      else {
        if (anythingQueued) processQueues()
        else log.debug("No more items to request, waiting for {} responses", assignedHandlers.size)
      }
    }

    def finish(): Unit = {
      log.info("Fast sync finished")
      context stop self
    }

    def processQueues(): Unit = {
      if (unassignedPeers.isEmpty) {
        if (assignedHandlers.nonEmpty) {
          log.info("There are no available peers, waiting for responses")
        } else {
          log.info("There are no peers to download from, scheduling a retry in {}", syncRetryInterval)
          scheduler.scheduleOnce(syncRetryInterval, self, ProcessSyncing)
        }
      } else {
        unassignedPeers
          .take(maxConcurrentRequests - assignedHandlers.size)
          .foreach(assignWork)
      }
    }

    def assignWork(peer: ActorRef): Unit = {
      if (receiptsQueue.nonEmpty) {
        requestReceipts(peer)
      } else if (blockBodiesQueue.nonEmpty) {
        requestBlockBodies(peer)
      } else if (context.child(blockHeadersHandlerName).isEmpty &&
        targetBlock.number > nodeStatusHolder().blockchainStatus.bestNumber) {
        requestBlockHeaders(peer)
      } else if (nonMptNodesQueue.nonEmpty || mptNodesQueue.nonEmpty) {
        requestNodes(peer)
      }
    }

    def requestReceipts(peer: ActorRef): Unit = {
      val (receiptsToGet, remainingReceipts) = receiptsQueue.splitAt(receiptsPerRequest)
      val handler = context.actorOf(FastSyncReceiptsRequestHandler.props(peer, receiptsToGet.toSeq, blockchain))
      context watch handler
      assignedHandlers += (handler -> peer)
      receiptsQueue = remainingReceipts
    }

    def requestBlockBodies(peer: ActorRef): Unit = {
      val (blockBodiesToGet, remainingBlockBodies) = blockBodiesQueue.splitAt(blockBodiesPerRequest)
      val handler = context.actorOf(FastSyncBlockBodiesRequestHandler.props(peer, blockBodiesToGet.toSeq, blockchain))
      context watch handler
      assignedHandlers += (handler -> peer)
      blockBodiesQueue = remainingBlockBodies
    }

    def requestBlockHeaders(peer: ActorRef): Unit = {
      val handler = context.actorOf(FastSyncBlockHeadersRequestHandler.props(
        peer,
        nodeStatusHolder().blockchainStatus.bestNumber + 1,
        blockHeadersPerRequest,
        nodeStatusHolder, blockchain), blockHeadersHandlerName)
      context watch handler
      assignedHandlers += (handler -> peer)
    }

    def requestNodes(peer: ActorRef): Unit = {
      val (nonMptNodesToGet, remainingNonMptNodes) = nonMptNodesQueue.splitAt(nodesPerRequest)
      val (mptNodesToGet, remainingMptNodes) = mptNodesQueue.splitAt(nodesPerRequest - nonMptNodesToGet.size)
      val nodesToGet = nonMptNodesToGet.toSeq ++ mptNodesToGet.toSeq
      val handler = context.actorOf(FastSyncNodesRequestHandler.props(peer, nodesToGet, blockchain, mptNodeStorage))
      context watch handler
      assignedHandlers += (handler -> peer)
      nonMptNodesQueue = remainingNonMptNodes
      mptNodesQueue = remainingMptNodes
    }

    def unassignedPeers: Set[ActorRef] = peersToDownloadFrom diff assignedHandlers.values.toSet

    def anythingQueued: Boolean =
      nonMptNodesQueue.nonEmpty ||
      mptNodesQueue.nonEmpty ||
      blockBodiesQueue.nonEmpty ||
      receiptsQueue.nonEmpty

    def fullySynced: Boolean =
      nodeStatusHolder().blockchainStatus.bestNumber >= targetBlock.number &&
      !anythingQueued &&
      assignedHandlers.isEmpty
  }
}

object FastSyncController {
  def props(
             peerManager: ActorRef,
             nodeStatusHolder: Agent[NodeStatus],
             blockchain: Blockchain,
             mptNodeStorage: MptNodeStorage):
  Props = Props(new FastSyncController(peerManager, nodeStatusHolder, blockchain, mptNodeStorage))

  case class StartFastSync(targetBlockHash: ByteString)

  case class EnqueueNodes(hashes: Seq[HashType])
  case class EnqueueBlockBodies(hashes: Seq[ByteString])
  case class EnqueueReceipts(hashes: Seq[ByteString])

  case class UpdateDownloadedNodesCount(update: Int)

  sealed trait HashType {
    def v: ByteString
  }
  case class StateMptNodeHash(v: ByteString) extends HashType
  case class ContractStorageMptNodeHash(v: ByteString) extends HashType
  case class EvmCodeHash(v: ByteString) extends HashType
  case class StorageRootHash(v: ByteString) extends HashType

  private case object PrintStatus
  private case object TargetBlockTimeout
  private case object ProcessSyncing
}
