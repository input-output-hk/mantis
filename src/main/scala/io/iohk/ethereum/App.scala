package io.iohk.ethereum

import java.net.{InetSocketAddress, URI}

import akka.actor.ActorSystem
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network.{NodeInfo, ServerActor, PeerManagerActor}
import com.typesafe.config.ConfigFactory
import scala.collection.JavaConversions._
import io.iohk.ethereum.network.{NodeInfo, PeerManagerActor, ServerActor}

object App {

  val nodeKey = generateKeyPair()

  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load().getConfig("etc-client")

    val listenHostname = config.getConfig("server-address").getString("interface")
    val listenPort = config.getConfig("server-address").getInt("port")
    val listenAddress = new InetSocketAddress(listenHostname, listenPort)
    val nodeInfo = NodeInfo(nodeKey, listenAddress)

    val actorSystem = ActorSystem("etc-client_system")

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(nodeInfo))
    val server = actorSystem.actorOf(ServerActor.props(nodeInfo, peerManager))

    server ! ServerActor.StartServer(listenAddress)

    val bootstrapNodes: List[URI] = config.getConfig("discovery").getStringList("bootstrap-nodes").toList.map{ item =>
      new URI(item)
    }
    bootstrapNodes.foreach{node =>
      peerManager ! PeerManagerActor.ConnectToPeer(node)
      Thread.sleep(5 * 1000)
      peerManager ! PeerManagerActor.StartFastDownload(node)
    }
  }
}
