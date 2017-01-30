package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.util.ByteString
import io.iohk.ethereum.network.FastSyncActor._
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockHeaders, GetBlockHeaders}
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
      peerActor ! PeerActor.SendMessage(GetBlockHeaders(Right(targetBlockHash), 1, 0, reverse = false))
      context become waitForTargetBlockHeader(startBlockHash, targetBlockHash)
  }

  def waitForTargetBlockHeader(startBlockHash: ByteString, targetBlockHash: ByteString): Receive = handleTerminated orElse {
    case BlockHeaders(blockHeader :: Nil) =>
      peerActor ! PeerActor.SendMessage(GetNodeData(Seq(blockHeader.stateRoot)))
      peerActor ! PeerActor.SendMessage(GetBlockHeaders(Right(startBlockHash), BlocksPerMessage, 0, reverse = false))
      context become processMessages(ProcessingState(startBlockHash, targetBlockHash,
        requestedNodes = Seq(MptNodeHash(blockHeader.stateRoot))))
  }

  def processMessages(state: ProcessingState): Receive = handleTerminated orElse {
    case m: BlockHeaders =>
    case m: BlockBodies if m.bodies.length == state.requestedBlockBodies.length =>
    case m: Receipts if m.receiptsForBlocks.length == state.requestedReceipts.length =>

    case m: NodeData if m.values.length == state.requestedNodes.length =>
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
    case m =>
      log.info("Got unexpected message {}", m)
      log.info("Requested nodes {}, requested blockBodies {}, requested receipts {}",
        state.requestedNodes, state.requestedBlockBodies, state.requestedReceipts)
  }

  private def handleMptNode(hash: ByteString, mptNode: MptNode, state: ProcessingState) = {
    mptNode match {
      case n: MptLeaf =>
        log.info("Got leaf node: {}", n)
        n.getAccount.codeHash
        n.getAccount.storageRoot


      case n: MptBranch =>
        val hashes = n.children.collect { case Left(MptHash(childHash)) => childHash }
        peerActor ! PeerActor.SendMessage(GetNodeData(hashes))
        context become processMessages(state.copy(requestedNodes = hashes.map(MptNodeHash)))
      case n: MptExtension =>
        n.child.fold(
          { hash =>

            peerActor ! PeerActor.SendMessage(GetNodeData(Seq(hash.hash)))
          }, { value =>

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
    requestedBlockBodies: Seq[ByteString] = Seq.empty,
    requestedReceipts: Seq[ByteString] = Seq.empty)

}
