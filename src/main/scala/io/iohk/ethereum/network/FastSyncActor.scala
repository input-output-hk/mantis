package io.iohk.ethereum.network

import akka.actor.Status.Success
import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.util.ByteString
import io.iohk.ethereum.network.FastSyncActor.{ByteValueHash, MptNodeHash, PartialDownloadDone, StartSync}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockHeaders, GetBlockBodies, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.PV63._

import scala.util.{Try, Failure => TryFailure, Success => TrySuccess}

class FastSyncActor(peerActor: ActorRef) extends Actor with ActorLogging {

  val blocksPerMessage = 10 //TODO move to conf

//  var currentBlockHeaders: Option[BlockHeaders] = None
//  var currentBlockBodies: Option[BlockBodies] = None
//  var currentReceipts: Option[Receipts] = None
//
//  var currentNodeHash: Option[Seq[ByteString]] = None
//
//  var requestedNodes: Option[Seq[Either[MptNodeHash, ByteValueHash]]] = None
//  var mptNodes: Map[ByteString, Either[MptNode, ByteString]] = Map()

  def handleTerminated: Receive = {
    case _: Terminated =>
      log.info("FastSync actor terminated")
      context stop self
  }

  def waitForTargetBlockHeader(startBlockHash: ByteString, targetBlockHash: ByteString): Receive = handleTerminated orElse {
        case m: BlockHeaders =>
    //      currentBlockHeaders = Some(m)
    //      peerActor ! PeerActor.SendMessage(GetBlockBodies(m.headers.map(_.blockHash)))
    //      peerActor ! PeerActor.SendMessage(GetReceipts(m.headers.map(_.blockHash)))
  }

  override def receive: Receive = handleTerminated orElse {
    case StartSync(startBlockHash, targetBlockHash) =>
      //peerActor ! PeerActor.SendMessage(GetBlockHeaders(Right(startBlockHash), blocksPerMessage, 0, reverse = false))
      peerActor ! PeerActor.SendMessage(GetBlockHeaders(Right(targetBlockHash), 1, 0, reverse = false))
      //currentNodeHash = Some(Seq(targetBlockHash))

//    case m: BlockHeaders =>
//      currentBlockHeaders = Some(m)
//      peerActor ! PeerActor.SendMessage(GetBlockBodies(m.headers.map(_.blockHash)))
//      peerActor ! PeerActor.SendMessage(GetReceipts(m.headers.map(_.blockHash)))
//
//    case m: BlockBodies =>
//      currentBlockBodies = Some(m)
//      self ! PartialDownloadDone
//
//    case m: Receipts =>
//      currentReceipts = Some(m)
//      self ! PartialDownloadDone
//
//    case PartialDownloadDone =>
//      (currentBlockHeaders, currentBlockBodies, currentReceipts) match {
//        case (Some(headers), Some(bodies), Some(receipts)) =>
//          log.info("got complete info \n{}\n{}\n{}", headers, bodies, receipts)
//
//        case _ =>
//      }
//
//    case m: NodeData =>

    //      m.mptNodes.zipWithIndex.map{
    //        case (TrySuccess(mptNode), idx) =>
    //        case (TryFailure(mptNode), idx) =>
    //      }


    //      val v: Seq[(ByteString, ByteString)] = m.mptNodes.zip(currentNodeHash.get).map { case (value, hash) => hash -> value }
    //      mptNodes ++ Map[ByteString, ByteString](v: _*)
    //      m.values.collect { case mptNode => mptNode }.map {
    //        case n: MptLeaf =>
    //
    //        case n: MptBranch =>
    //          val hashes = n.children.collect { case Left(MptHash(hash)) => hash }
    //          currentNodeHash = Some(hashes)
    //          peerActor ! PeerActor.SendMessage(GetNodeData(hashes))
    //        case n: MptExtension =>
    //          n.child.fold({ a =>
    //            currentNodeHash = Some(Seq(a.hash))
    //            peerActor ! PeerActor.SendMessage(GetNodeData(Seq(a.hash)))
    //          }, _)
    //      }
  }
}

object FastSyncActor {
  def props(peerActor: ActorRef): Props = Props(new FastSyncActor(peerActor))

  case class StartSync(startBlockHash: ByteString, targetBlockHash: ByteString)

  case object PartialDownloadDone

  private case class MptNodeHash(v: ByteString)

  private case class ByteValueHash(v: ByteString)

}
