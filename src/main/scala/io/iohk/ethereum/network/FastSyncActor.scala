package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.util.ByteString
import io.iohk.ethereum.network.FastSyncActor.{PartialDownloadDone, StartSync}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockHeaders, GetBlockBodies, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.PV63.{GetReceipts, Receipts}

class FastSyncActor(peerActor: ActorRef) extends Actor with ActorLogging {

  val blocksPerMessage = 10

  var currentBlockHeaders: Option[BlockHeaders] = None
  var currentBlockBodies: Option[BlockBodies] = None
  var currentReceipts: Option[Receipts] = None

  def handleTerminated: Receive = {
    case _: Terminated =>
      log.info("FastSync actor terminated")
      context stop self
  }

  override def receive: Receive = handleTerminated orElse {
    case StartSync(startBlockHash, targetBlockHash) =>
      peerActor ! PeerActor.SendMessage(GetBlockHeaders(Right(startBlockHash), blocksPerMessage, 0, reverse = false))

    case m: BlockHeaders =>
      currentBlockHeaders = Some(m)
      peerActor ! PeerActor.SendMessage(GetBlockBodies(m.headers.map(_.blockHash)))
      peerActor ! PeerActor.SendMessage(GetReceipts(m.headers.map(_.blockHash)))

    case m: BlockBodies =>
      currentBlockBodies = Some(m)
      self ! PartialDownloadDone

    case m: Receipts =>
      currentReceipts = Some(m)
      self ! PartialDownloadDone

    case PartialDownloadDone =>
      (currentBlockHeaders, currentBlockBodies, currentReceipts) match {
        case (Some(headers), Some(bodies), Some(receipts)) =>
          log.info("got complete info \n{}\n{}\n{}", headers, bodies, receipts)

        case _ =>
      }
  }
}

object FastSyncActor {
  def props(peerActor: ActorRef): Props = Props(new FastSyncActor(peerActor))

  case class StartSync(startBlockHash: ByteString, targetBlockHash: ByteString)

  case object PartialDownloadDone

}
