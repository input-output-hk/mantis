package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}
import java.util.UUID

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.util.ByteString
import org.spongycastle.util.encoders.Hex

import scala.collection.immutable.HashMap

class PeerManagerActor(nodeInfo: NodeInfo) extends Actor with ActorLogging {

  import PeerManagerActor._

  override val supervisorStrategy =
    OneForOneStrategy() {
      case _ => Stop
    }

  var actorsMap: HashMap[URI, ActorRef] = HashMap()

  override def receive: Receive = {
    case HandlePeerConnection(connection, remoteAddress) =>
      val peer = createPeer()
      log.info("Peer {} handling incoming peer connection from {}", peer.path.name, remoteAddress)
      peer ! PeerActor.HandleConnection(connection)

    case ConnectToPeer(uri) =>
      val actor = createPeer()
      actor ! PeerActor.ConnectTo(uri)
      actorsMap += (uri -> actor)

    case StartFastDownload(uri) =>
      actorsMap.get(uri).foreach { a =>
        a ! PeerActor.StartFastSync(
          startBlockHash = ByteString(Hex.decode("d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3")), //genesis block
          targetBlockHash = ByteString(Hex.decode("ca2b65cf841b7acc2548977ad69a3e118940d0934cdbf2d3645c44bdf5023465"))
        )
      }
  }

  def createPeer(): ActorRef = {
    val id = UUID.randomUUID.toString
    context.actorOf(PeerActor.props(nodeInfo), id)
  }
}

object PeerManagerActor {
  def props(nodeInfo: NodeInfo): Props =
    Props(new PeerManagerActor(nodeInfo))

  case class HandlePeerConnection(connection: ActorRef, remoteAddress: InetSocketAddress)

  case class ConnectToPeer(uri: URI)

  case class StartFastDownload(uri: URI)

}
