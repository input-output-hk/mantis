package io.iohk.ethereum

import akka.actor.ActorSystem
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network.{ServerActor, PeerManagerActor}

object App {

  val nodeKey = generateKeyPair()

  def main(args: Array[String]): Unit = {

    val actorSystem = ActorSystem("etc-client_system")

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(nodeKey))
    val server = actorSystem.actorOf(ServerActor.props(peerManager))
  }

}
