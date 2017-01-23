package io.iohk.ethereum

import java.net.{InetSocketAddress, URI}

import akka.actor.ActorSystem
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network.{NodeInfo, ServerActor, PeerManagerActor}
import com.typesafe.config.ConfigFactory
import scala.collection.JavaConversions._

object App {

  val nodeKey = generateKeyPair()

  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load()

    val listenHostname = config.getConfig("serverAddress").getString("IP")
    val listenPort = config.getConfig("serverAddress").getInt("Port")
    val listenAddress = new InetSocketAddress(listenHostname, listenPort)
    val nodeInfo = NodeInfo(nodeKey, listenAddress)

    val actorSystem = ActorSystem("etc-client_system")

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(nodeInfo))
    val server = actorSystem.actorOf(ServerActor.props(nodeInfo, peerManager))

    server ! ServerActor.StartServer(listenAddress)

    val bootstrapNodes: List[URI] = config.getConfig("discovery").getStringList("bootstrapNodes").toList.map{ item =>
      new URI(item)
    }
    bootstrapNodes.foreach{node =>
      peerManager ! PeerManagerActor.ConnectToPeer(node)
    }
  }
}
