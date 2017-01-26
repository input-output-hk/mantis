package io.iohk.ethereum

import java.net.{InetSocketAddress, URI}

import akka.actor.ActorSystem
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network.{NodeInfo, PeerManagerActor, ServerActor}

object App {

  val nodeKey = generateKeyPair()

  def main(args: Array[String]): Unit = {
    val listenHostname = "127.0.0.1"
    val listenPort = 9076
    val listenAddress = new InetSocketAddress(listenHostname, listenPort)
    val nodeInfo = NodeInfo(nodeKey, listenAddress)

    val actorSystem = ActorSystem("etc-client_system")

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(nodeInfo))
    val server = actorSystem.actorOf(ServerActor.props(nodeInfo, peerManager))

    server ! ServerActor.StartServer(listenAddress)

    if (args.length > 0) {
      val peerUri = new URI(args(0))
      peerManager ! PeerManagerActor.ConnectToPeer(peerUri)
      Thread.sleep(5 * 1000)
      peerManager ! PeerManagerActor.StartFastDownload(peerUri)
    }
  }
}
