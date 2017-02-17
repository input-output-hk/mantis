package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.Blockchain
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.network.FastSyncActor._
import io.iohk.ethereum.network.PeerActor.MessageReceived
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.utils.Config.FastSync
import org.spongycastle.util.encoders.Hex

class FastSyncActor(
    peerActor: ActorRef,
    blockchain: Blockchain,
    mptNodeStorage: MptNodeStorage)
  extends Actor
  with ActorLogging{

  import context.{dispatcher, system}

  val GenesisBlockNumber = 0
  val EmptyAccountStorageHash = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"))
  val EmptyAccountEvmCodeHash = ByteString(Hex.decode("c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

  override def receive: Receive = {
    case StartSync(targetBlockHash) =>
      //TODO check DB if it is not empty delete everything
      peerActor ! PeerActor.Subscribe(Set(NodeData.code, Receipts.code, BlockBodies.code, BlockHeaders.code))
      peerActor ! PeerActor.SendMessage(GetBlockHeaders(Right(targetBlockHash), 1, 0, reverse = false))
      context become waitForTargetBlockHeader(targetBlockHash)
  }

  def waitForTargetBlockHeader(targetBlockHash: ByteString): Receive = {
    case MessageReceived(BlockHeaders(blockHeaders)) if blockHeaders.nonEmpty =>
      peerActor ! PeerActor.SendMessage(GetNodeData(Seq(blockHeaders.head.stateRoot)))
      peerActor ! PeerActor.SendMessage(GetBlockHeaders(Left(GenesisBlockNumber + 1), FastSync.BlocksPerMessage, 0, reverse = false))
      context become processMessages(ProcessingState(targetBlockHash,
        targetBlockNumber = Some(blockHeaders.head.number),
        currentBlockNumber = Some(GenesisBlockNumber),
        requestedNodes = Seq(StateMptNodeHash(blockHeaders.head.stateRoot))))
  }

  def processMessages(state: ProcessingState): Receive = handleMptDownload(state) orElse {
    case MessageReceived(BlockHeaders(headers)) =>
      val blockHashes = headers.map(_.hash)
      peerActor ! PeerActor.SendMessage(GetBlockBodies(blockHashes))
      peerActor ! PeerActor.SendMessage(GetReceipts(blockHashes))
      context become processMessages(state.copy(blockHeaders = headers))

    case MessageReceived(BlockBodies(bodies)) if bodies.length == state.blockHeaders.length =>
      self ! PartialDownloadDone
      context become processMessages(state.copy(blockBodies = bodies))

    case MessageReceived(Receipts(receipts: Seq[Seq[Receipt]])) if receipts.length == state.blockHeaders.length =>
      self ! PartialDownloadDone
      context become processMessages(state.copy(blockReceipts = receipts))

    case PartialDownloadDone =>
      handlePartialDownload(state)

    case SyncDone => if (isFastSyncDone(state)) {
      peerActor ! FastSyncDone(fastSyncActor = self)
    }

    case MessageReceived(m) =>
      log.info("Got unexpected message {}", m)
      log.info("Requested nodes {}, blockHeaders {}", state.requestedNodes, state.blockHeaders)
      peerActor ! SyncFailure
      self ! PoisonPill
  }

  private def isFastSyncDone(state: ProcessingState): Boolean =
    state.requestedNodes.isEmpty && state.nodesQueue.isEmpty &&
      state.targetBlockNumber.nonEmpty && state.targetBlockNumber == state.currentBlockNumber

  private def handlePartialDownload(state: ProcessingState) = {
    state match {
      case ProcessingState(_, Some(targetBlockNumber), Some(_), _, _, headers, receipts, bodies) if receipts.nonEmpty && bodies.nonEmpty =>
        log.info("Got complete blocks")
        log.info("headers: {}", headers)
        log.info("receipts: {}", receipts)
        log.info("bodies: {}", bodies)

        val blockHashes = headers.map(_.hash)

        (blockHashes, headers, bodies).zipped.foreach { case (hash, header, body) => blockchain.save(Block(header, body)) }

        blockHashes.zip(receipts).foreach { case (hash, value) =>
          blockchain.save(hash, value)
        }

        val nextCurrentBlockNumber = headers.last.number
        val nextBlockNumber = nextCurrentBlockNumber + 1

        if (nextCurrentBlockNumber < targetBlockNumber) {
          if (nextBlockNumber + FastSync.BlocksPerMessage <= targetBlockNumber) {
            peerActor ! PeerActor.SendMessage(GetBlockHeaders(Left(nextBlockNumber), FastSync.BlocksPerMessage, skip = 0, reverse = false))
          } else {
            peerActor ! PeerActor.SendMessage(GetBlockHeaders(Left(nextBlockNumber), targetBlockNumber - nextCurrentBlockNumber, skip = 0, reverse = false))
          }
        } else if (nextCurrentBlockNumber == targetBlockNumber) {
          self ! SyncDone
        }

        context become processMessages(state.copy(
          currentBlockNumber = Some(nextCurrentBlockNumber),
          blockHeaders = Seq.empty, blockReceipts = Seq.empty, blockBodies = Seq.empty))

      case _ =>
    }
  }

  private def handleMptDownload(state: ProcessingState): Receive = {
    case MessageReceived(m: NodeData) if m.values.length == state.requestedNodes.length =>
      state.requestedNodes.zipWithIndex.foreach {
        case (StateMptNodeHash(hash), idx) =>
          handleMptNode(hash, m.getMptNode(idx))

        case (ContractStorageMptNodeHash(hash), idx) =>
          handleContractMptNode(hash, m.getMptNode(idx))

        case (EvmCodeHash(hash), idx) =>
          val evmCode = m.values(idx)
          val msg =
            s"got EVM code: ${Hex.toHexString(evmCode.toArray[Byte])}"
          log.info(msg)
          blockchain.save(hash, evmCode)
        case (StorageRootHash(hash), idx) =>
          val rootNode = m.getMptNode(idx)
          log.info(s"got root node for contract storage: $rootNode")
          handleContractMptNode(hash, rootNode)
      }
      //remove hashes from state as nodes received
      context become processMessages(state.copy(requestedNodes = Seq.empty))
      self ! FetchNodes

    case RequestNodes(hashes@_*) =>
      //prep end nodesQueue to get deep first
      context become processMessages(state.copy(nodesQueue = hashes ++ state.nodesQueue))
      self ! FetchNodes

    case FetchNodes =>
      fetchNodes(state)
  }

  private def fetchNodes(state: ProcessingState) = if (state.requestedNodes.isEmpty) {
    (state.requestedNodes, state.nodesQueue) match {
      case (requested, queue) if requested.isEmpty && queue.isEmpty =>
        self ! SyncDone
      case (requested, queue) if requested.isEmpty =>
        val (nonMptHashes, mptHashes) = queue.partition {
          case EvmCodeHash(_) => true
          case StorageRootHash(_) => true
          case StateMptNodeHash(_) => false
          case ContractStorageMptNodeHash(_) => false
        }

        val (forRequest, forQueue) = (nonMptHashes ++ mptHashes).splitAt(FastSync.NodesPerRequest)
        context become processMessages(state.copy(requestedNodes = forRequest, nodesQueue = forQueue))
        peerActor ! PeerActor.SendMessage(GetNodeData(forRequest.map(_.v)))
        system.scheduler.scheduleOnce(FastSync.NodeRequestsInterval) {
          self ! FetchNodes
        }
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
        log.info("{}", hashes)
        self ! RequestNodes(hashes.map(ContractStorageMptNodeHash): _*)
        mptNodeStorage.put(n)

      case n: MptExtension =>
        log.info("Got contract extension node: {}", n)
        n.child.fold(
          { case MptHash(nodeHash) =>
            self ! RequestNodes(ContractStorageMptNodeHash(nodeHash))
          }, { case MptValue(value) =>
            log.info("Got contract value in extension node: ", Hex.toHexString(value.toArray[Byte]))
          })
        mptNodeStorage.put(n)
    }
  }

  private def handleMptNode(hash: ByteString, mptNode: MptNode) = {
    mptNode match {
      case n: MptLeaf =>
        log.info("Got leaf node: {}", n)
        val evm = n.getAccount.codeHash
        val storage = n.getAccount.storageRoot

        if (evm != EmptyAccountEvmCodeHash) {
          self ! RequestNodes(EvmCodeHash(evm))
        }

        if (storage != EmptyAccountStorageHash) {
          self ! RequestNodes(StorageRootHash(storage))
        }

        mptNodeStorage.put(n)

      case n: MptBranch =>
        log.info("Got branch node: {}", n)
        val hashes = n.children.collect { case Left(MptHash(childHash)) => childHash }.filter(_.nonEmpty)
        self ! RequestNodes(hashes.map(StateMptNodeHash): _*)
        mptNodeStorage.put(n)

      case n: MptExtension =>
        log.info("Got extension node: {}", n)
        n.child.fold(
          { case MptHash(nodeHash) =>
            self ! RequestNodes(StateMptNodeHash(nodeHash))
          }, { case MptValue(value) =>
            log.info("Got value in extension node: ", Hex.toHexString(value.toArray[Byte]))
          })
        mptNodeStorage.put(n)
    }
  }
}

object FastSyncActor {
  def props(peerActor: ActorRef, blockchain: Blockchain, mptNodeStorage: MptNodeStorage): Props = {
    Props(new FastSyncActor(peerActor, blockchain, mptNodeStorage))
  }

  case class Storage(
    blockchain: Blockchain,
    receiptStorage: ReceiptStorage,
    mptNodeStorage: MptNodeStorage,
    evmStorage: EvmCodeStorage)

  case class StartSync(targetBlockHash: ByteString)

  case object SyncFailure

  case class FastSyncDone(fastSyncActor: ActorRef)

  private case object PartialDownloadDone

  private sealed trait HashType {
    val v: ByteString
  }

  private case class StateMptNodeHash(v: ByteString) extends HashType {
    override def toString: String = s"StateMptNodeHash(${Hex.toHexString(v.toArray[Byte])})"
  }

  private case class ContractStorageMptNodeHash(v: ByteString) extends HashType {
    override def toString: String = s"ContractStorageMptNodeHash(${Hex.toHexString(v.toArray[Byte])})"
  }

  private case class EvmCodeHash(v: ByteString) extends HashType {
    override def toString: String = s"EvmCodeHash(${Hex.toHexString(v.toArray[Byte])})"
  }

  private case class StorageRootHash(v: ByteString) extends HashType {
    override def toString: String = s"StorageRootHash(${Hex.toHexString(v.toArray[Byte])})"
  }

  private case class RequestNodes(hashes: HashType*)

  private case object FetchNodes

  private case object SyncDone

  private case class ProcessingState(
    targetBlockHash: ByteString,
    targetBlockNumber: Option[BigInt] = None,
    currentBlockNumber: Option[BigInt] = None,
    requestedNodes: Seq[HashType] = Seq.empty,
    nodesQueue: Seq[HashType] = Seq.empty,
    blockHeaders: Seq[BlockHeader] = Seq.empty,
    blockReceipts: Seq[Seq[Receipt]] = Seq.empty,
    blockBodies: Seq[BlockBody] = Seq.empty)

}
