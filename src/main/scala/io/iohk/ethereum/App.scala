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
    val server = actorSystem.actorOf(ServerActor.props(peerManager))

    server ! ServerActor.StartServer(new InetSocketAddress("127.0.0.1", 9076))

    val peerUri = new URI("enode://8e5e9fcbd8e292de71052e6d075112a893bbc4993529098cd974474b4df14685dcfc4221d601a73fdcff398b4c6c4f5d8d7f1d8c06d23db9086d9c9796b9c671@192.168.3.154:30303")
    peerManager ! PeerManagerActor.ConnectToPeer(peerUri)
  }

}
