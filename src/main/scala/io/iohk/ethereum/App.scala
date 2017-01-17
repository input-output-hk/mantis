package io.iohk.ethereum

import java.net.{InetSocketAddress, URI}

import akka.actor.ActorSystem
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network.{ServerActor, PeerManagerActor}

object App {

  val nodeKey = generateKeyPair()

  def main(args: Array[String]): Unit = {

    val actorSystem = ActorSystem("etc-client_system")

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(nodeKey))
    val server = actorSystem.actorOf(ServerActor.props(nodeKey, peerManager))

    server ! ServerActor.StartServer(new InetSocketAddress("127.0.0.1", 9076))

    if (args.length > 0) {
      val peerUri = new URI(args(0))
      peerManager ! PeerManagerActor.ConnectToPeer(peerUri)
    }
  }

}
