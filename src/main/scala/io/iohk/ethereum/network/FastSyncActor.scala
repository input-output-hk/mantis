package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.util.ByteString
import io.iohk.ethereum.network.FastSyncActor._
import io.iohk.ethereum.network.PeerActor.MessageReceived
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63._
import org.spongycastle.util.encoders.Hex

class FastSyncActor(peerActor: ActorRef) extends Actor with ActorLogging {

  val BlocksPerMessage = 10
  //TODO move to conf
  val EmptyAccountStorageHash = ByteString(Hex.decode("c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))
  val EmptyAccountEvmCodeHash = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"))

  def handleTerminated: Receive = {
    case _: Terminated =>
      log.info("FastSync actor terminated")
      context stop self
  }

  override def receive: Receive = handleTerminated orElse {
    case StartSync(startBlockHash, targetBlockHash) =>
      peerActor ! PeerActor.Subscribe(Set(NodeData.code, Receipts.code, BlockBodies.code, BlockHeaders.code))
      peerActor ! PeerActor.SendMessage(GetBlockHeaders(Right(targetBlockHash), 1, 0, reverse = false))
      context become waitForTargetBlockHeader(startBlockHash, targetBlockHash)
  }

  def waitForTargetBlockHeader(startBlockHash: ByteString, targetBlockHash: ByteString): Receive = handleTerminated orElse {
    case MessageReceived(BlockHeaders(blockHeaders)) =>
      peerActor ! PeerActor.SendMessage(GetNodeData(Seq(blockHeaders.head.stateRoot)))
      peerActor ! PeerActor.SendMessage(GetBlockHeaders(Right(startBlockHash), BlocksPerMessage, 0, reverse = false))
      context become processMessages(ProcessingState(startBlockHash, targetBlockHash,
        requestedNodes = Seq(MptNodeHash(blockHeaders.head.stateRoot))))
  }

  def processMessages(state: ProcessingState): Receive = handleTerminated orElse {
    case MessageReceived(BlockHeaders(headers)) =>
      val blockHashes = headers.map(_.blockHash)
      peerActor ! PeerActor.SendMessage(GetBlockBodies(blockHashes))
      peerActor ! PeerActor.SendMessage(GetReceipts(blockHashes))
      context become processMessages(state.copy(blockHeaders = headers))

    case MessageReceived(BlockBodies(bodies)) if bodies.length == state.blockHeaders.length =>
      self ! PartialDownloadDone
      context become processMessages(state.copy(blockBodies = bodies))

    case MessageReceived(Receipts(receipts: Seq[Seq[Receipt]])) if receipts.length == state.blockHeaders.length =>
      self ! PartialDownloadDone
      context become processMessages(state.copy(blockReceipts = receipts))

    case MessageReceived(m: NodeData) if m.values.length == state.requestedNodes.length =>
      state.requestedNodes.zipWithIndex.foreach {
        case (MptNodeHash(hash), idx) =>
          handleMptNode(hash, m.getMptNode(idx), state)

        case (EvmCodeHash(hash), idx) =>
          val evmCode = m.values(idx)
          val msg =
            s"""got EVM code: ${Hex.toHexString(evmCode.toArray[Byte])}
               |for hash: ${Hex.toHexString(hash.toArray[Byte])}
             """.stripMargin
          log.info(msg)
        case (StorageRootHash(hash), idx) =>
          val rootNode = m.getMptNode(idx)
          val msg =
            s"""got root node for contract storage: $rootNode
               |for hash: ${Hex.toHexString(hash.toArray[Byte])}
             """.stripMargin
          log.info(msg)
      }

    case PartialDownloadDone =>
      handlePartialDownload(state)

    case MessageReceived(m) =>
      log.info("Got unexpected message {}", m)
      log.info("Requested nodes {}, blockHeaders {}",
        state.requestedNodes, state.blockHeaders)
  }

  private def handlePartialDownload(state: ProcessingState) = {
    state match {
      case ProcessingState(_, _, _, headers, receipts, bodies) if receipts.nonEmpty && bodies.nonEmpty =>
        log.info("Got complete blocks")
        log.info("headers: {}", headers)
        log.info("receipts: {}", receipts)
        log.info("bodies: {}", bodies)
        peerActor ! PeerActor.SendMessage(GetBlockHeaders(Right(headers.last.blockHash), BlocksPerMessage, skip = 1, reverse = false))
        context become processMessages(state.copy(blockHeaders = Seq.empty, blockReceipts = Seq.empty, blockBodies = Seq.empty))
      case _ =>
    }
  }

  private def handleMptNode(hash: ByteString, mptNode: MptNode, state: ProcessingState) = {
    mptNode match {
      case n: MptLeaf =>
        log.info("Got leaf node: {}", n)
        val evm = n.getAccount.codeHash
        val storage = n.getAccount.storageRoot

        peerActor ! PeerActor.SendMessage(GetNodeData(Seq(evm, storage)))
        context become processMessages(state.copy(requestedNodes = Seq(EvmCodeHash(evm), StorageRootHash(storage))))

      case n: MptBranch =>
        val hashes = n.children.collect { case Left(MptHash(childHash)) => childHash }
        peerActor ! PeerActor.SendMessage(GetNodeData(hashes))
        context become processMessages(state.copy(requestedNodes = hashes.map(MptNodeHash)))

      case n: MptExtension =>
        n.child.fold(
          { hash =>
            peerActor ! PeerActor.SendMessage(GetNodeData(Seq(hash.hash)))
            context become processMessages(state.copy(requestedNodes = Seq(MptNodeHash(hash.hash))))
          }, { value =>
            log.info("Got value in extension node: ", Hex.toHexString(value.value.toArray[Byte]))
          })
    }
  }
}

object FastSyncActor {
  def props(peerActor: ActorRef): Props = Props(new FastSyncActor(peerActor))

  case class StartSync(startBlockHash: ByteString, targetBlockHash: ByteString)

  case object PartialDownloadDone

  private trait HashType

  private case class MptNodeHash(v: ByteString) extends HashType

  private case class EvmCodeHash(v: ByteString) extends HashType

  private case class StorageRootHash(v: ByteString) extends HashType

  private case class ProcessingState(
    startBlockHash: ByteString,
    targetBlockHash: ByteString,
    requestedNodes: Seq[HashType] = Seq.empty,
    blockHeaders: Seq[BlockHeader] = Seq.empty,
    blockReceipts: Seq[Seq[Receipt]] = Seq.empty,
    blockBodies: Seq[BlockBody] = Seq.empty)

}
