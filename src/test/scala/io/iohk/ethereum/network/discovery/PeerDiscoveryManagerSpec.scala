package io.iohk.ethereum.network.discovery

import java.net.{InetAddress, InetSocketAddress}
import java.time.{Clock, Instant, ZoneId}

import akka.actor.ActorSystem
import akka.agent.Agent
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.network.discovery.DiscoveryListener._
import io.iohk.ethereum.network.discovery.PeerDiscoveryManager.DiscoveryNodeInfo
import io.iohk.ethereum.nodebuilder.{NodeKeyBuilder, SecureRandomBuilder}
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus}
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global

class PeerDiscoveryManagerSpec extends FlatSpec with Matchers with MockFactory with ScalaFutures with NormalPatience {

  it should "correctly respond to Ping Message" in new TestSetup {
    val pingMessageReceived = MessageReceived(ping, remoteUdpAddress, pingPingPacketDecoded)

    discoveryPeerManager ! pingMessageReceived

    val expectedPongResponse = SendMessage(Pong(remoteEndpoint, pingPingPacketDecoded.mdc, expectedTime), remoteUdpAddress)

    dicoveryListner.expectMsg(expectedPongResponse)
  }

  it should "correctly respond to Pong Message" in new TestSetup {
    val pong = Pong(toEndpoint, pingPingPacketDecoded.mdc, timestamp)
    val pongDecoded = getPacket(pong)
    val pongMessageReceiced = MessageReceived(pong, remoteUdpAddress, pongDecoded)

    val nodeInfo =  DiscoveryNodeInfo.fromNode(Node(pongDecoded.nodeId, remoteUdpAddress))

    discoveryPeerManager ! pongMessageReceiced

    val expectedFindNodeResponse = SendMessage(FindNode(ByteString(nodeStatus.nodeId), expectedTime), remoteUdpAddress)

    Thread.sleep(1000)
    dicoveryListner.expectMsg(expectedFindNodeResponse)
    discoveryPeerManager.underlyingActor.nodesInfo.size shouldEqual 2
    discoveryPeerManager.underlyingActor.nodesInfo.values.toSet should contain (nodeInfo)
  }

  it should "correctly respond to FindNode Message" in new TestSetup {
    val findNode = FindNode(ByteString.empty, timestamp)
    val findeNodeDecoded = getPacket(findNode)
    val findNodeMessageReceived = MessageReceived(findNode, remoteUdpAddress, findeNodeDecoded)

    discoveryPeerManager ! findNodeMessageReceived

    val expectedFindNodeResponse = SendMessage(Neighbours(Nil, expectedTime), remoteUdpAddress)

    dicoveryListner.expectMsg(expectedFindNodeResponse)
  }

  it should "correctly respond to neighbours Message" in new TestSetup {
    val neighboursm = Neighbours(neighbours, timestamp)
    val neighboursDecoded = getPacket(neighboursm)
    val neighboursMessageReceived = MessageReceived(neighboursm, remoteUdpAddress, neighboursDecoded)

    discoveryPeerManager ! neighboursMessageReceived

    expectedMes.foreach(mess => dicoveryListner.expectMsg(mess))
  }



  // scalastyle:off magic.number
  trait TestSetup extends MockFactory with SecureRandomBuilder with NodeKeyBuilder with EphemBlockchainTestSetup  {
    import DiscoveryListener._
    implicit val system = ActorSystem("DiscoverySpec_System")
    val time = new VirtualTime

    def getPacket[M <: Message](m: M)(implicit rlpEnc: RLPEncoder[M]): Packet = {
      val encoded = encodePacket(m, nodeKey)
      decodePacket(encoded).get
    }

    val discoveryConfig = DiscoveryConfig(Config.config)
    val expTimeSec = discoveryConfig.messageExpiration.toSeconds
    val dicoveryListner = TestProbe()

    val address = InetAddress.getLocalHost
    val port = 30303
    val localAddress = new InetSocketAddress(address, port)

    val remoteAddr = "31.178.1.7"
    val remoteUdpPort = 30303
    val remoteTcpPort = 9076
    val remoteUdpAddress = new InetSocketAddress(remoteAddr, remoteUdpPort)

    val nodeStatus =
      NodeStatus(
        key = nodeKey,
        serverStatus = ServerStatus.Listening(localAddress),
        discoveryStatus = ServerStatus.Listening(localAddress))

    val nodeStatusHolder = Agent(nodeStatus)
    val fakeClock = Clock.fixed(Instant.ofEpochSecond(0), ZoneId.systemDefault())
    val discoveryPeerManager = TestActorRef[PeerDiscoveryManager](PeerDiscoveryManager.props(
      dicoveryListner.ref,
      discoveryConfig,
      storagesInstance.storages.knownNodesStorage,
      nodeStatusHolder,
      fakeClock
    ))

    val expectedTime = fakeClock.instant().plusSeconds(expTimeSec).getEpochSecond
    dicoveryListner.expectMsg(Subscribe)

    val version = 4
    val toEndpoint = Endpoint.makeEndpoint(localAddress, port)
    val remoteEndpoint = Endpoint.makeEndpoint(remoteUdpAddress, remoteTcpPort)
    val timestamp = Long.MaxValue

    val ping = Ping(version, remoteEndpoint, toEndpoint, timestamp)
    val pingPingPacketDecoded = getPacket(ping)

    val neighboursCount = 9
    val neighbours = (1 to 9).map{n =>
      val newAddress: Array[Byte] = Array(31, 178, 1, n).map(_.toByte)
      val newId: Array[Byte] = Array.fill(64){n.toByte}
      val socketAddress = new InetSocketAddress(InetAddress.getByAddress(newAddress), remoteUdpPort)
      val nodeId = Node(ByteString(newId), socketAddress).id
      val neighbourEndpoint = Endpoint.makeEndpoint(socketAddress, remoteTcpPort)
      Neighbour(neighbourEndpoint, nodeId)
    }.toSeq

    val expectedMes = neighbours.map {n =>
      val ping = Ping(version, toEndpoint, n.endpoint, expectedTime)
      SendMessage(ping, new InetSocketAddress(InetAddress.getByAddress(n.endpoint.address.toArray), n.endpoint.udpPort))
    }
  }
}
