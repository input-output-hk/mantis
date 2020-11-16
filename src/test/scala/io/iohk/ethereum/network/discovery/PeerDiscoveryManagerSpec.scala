package io.iohk.ethereum.network.discovery

import java.net.{InetAddress, InetSocketAddress}
import java.time.{Clock, Instant, ZoneId}

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.network.discovery.DiscoveryListener._
import io.iohk.ethereum.network.discovery.PeerDiscoveryManager.{DiscoveryNodeInfo, PingInfo}
import io.iohk.ethereum.nodebuilder.NodeKeyBuilder
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus}
import java.util.concurrent.atomic.AtomicReference

import io.iohk.ethereum.jsonrpc.security.SecureRandomBuilder
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures

import scala.util.Success
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PeerDiscoveryManagerSpec
    extends AnyFlatSpec
    with Matchers
    with MockFactory
    with ScalaFutures
    with NormalPatience {

  it should "correctly respond to Ping Message" in new TestSetup {
    val pingMessageReceived = MessageReceived(ping, remoteUdpAddress, pingPingPacketDecoded)

    discoveryPeerManager ! pingMessageReceived

    val packet =
      Packet(encodePacket(Pong(remoteEndpoint, pingPingPacketDecoded.mdc, expectedTime), nodeStatusHolder.get().key))

    val expectedPongResponse = SendPacket(packet, remoteUdpAddress)

    discoveryListner.expectMsg(expectedPongResponse)
  }

  it should "correctly respond to Pong Message" in new TestSetup {
    val pong = Pong(toEndpoint, pingPingPacketDecoded.mdc, timestamp)
    val pongDecoded = getPacket(pong)
    val pongMessageReceiced = MessageReceived(pong, remoteUdpAddress, pongDecoded)

    pongDecoded.validated().isDefined should be(true)

    val nodeInfo = DiscoveryNodeInfo.fromNode(
      Node(pongDecoded.nodeId.get, remoteUdpAddress.getAddress, remoteUdpPort, remoteUdpPort)
    )

    discoveryPeerManager.underlyingActor.pingedNodes += pingPingPacketDecoded.mdc -> PingInfo(nodeInfo, timestamp)

    discoveryPeerManager ! pongMessageReceiced

    val expectedFindNodeResponse =
      SendPacket(
        Packet(encodePacket(FindNode(ByteString(nodeStatus.nodeId), expectedTime), nodeStatusHolder.get().key)),
        remoteUdpAddress
      )

    Thread.sleep(1500)
    discoveryListner.expectMsg(expectedFindNodeResponse)
    discoveryPeerManager.underlyingActor.nodesInfo.size shouldEqual 3 // 2 bootstraps + 1 new node
    discoveryPeerManager.underlyingActor.nodesInfo.values.toSet should contain(nodeInfo.copy(addTimestamp = 0))
  }

  it should "correctly respond to FindNode Message" in new TestSetup {
    val findNode = FindNode(ByteString.empty, timestamp)
    val findeNodeDecoded = getPacket(findNode)
    val findNodeMessageReceived = MessageReceived(findNode, remoteUdpAddress, findeNodeDecoded)

    discoveryPeerManager ! findNodeMessageReceived

    val expectedFindNodeResponse = (
      Packet(encodePacket(Neighbours(bootNeighbours, expectedTime), nodeStatusHolder.get().key)),
      remoteUdpAddress
    )

    val r = discoveryListner.expectMsgType[SendPacket]

    extractMessage(r.packet) match {
      case Success(Neighbours(received, _)) =>
        received should contain theSameElementsAs bootNeighbours
      case _ => fail("Wrong message")
    }
  }

  it should "correctly respond to neighbours Message" in new TestSetup {
    val neighboursm = Neighbours(neighbours, timestamp)
    val neighboursDecoded = getPacket(neighboursm)
    val neighboursMessageReceived = MessageReceived(neighboursm, remoteUdpAddress, neighboursDecoded)

    discoveryPeerManager ! neighboursMessageReceived

    expectedMes.foreach(mess => discoveryListner.expectMsg(mess))
    // necessery doubling because of different pong validations in parity and geth
    discoveryPeerManager.underlyingActor.pingedNodes.size shouldEqual (neighbours.size * 2)
  }

  it should "correctly scan bootstrap nodes" in new TestSetup {
    discoveryPeerManager ! PeerDiscoveryManager.Scan
    Thread.sleep(500)
    // necessery doubling because of different pong validations in parity and geth
    discoveryPeerManager.underlyingActor.pingedNodes.size shouldEqual (bootstrapNodes.size * 2)
    expectedBootStrapPings.foreach(mes => discoveryListner.expectMsg(mes._2))
  }

  // scalastyle:off magic.number
  trait TestSetup extends MockFactory with SecureRandomBuilder with NodeKeyBuilder with EphemBlockchainTestSetup {

    import DiscoveryListener._

    override implicit lazy val system = ActorSystem("DiscoverySpec_System")
    val time = new VirtualTime

    def getPacket[M <: Message](m: M)(implicit rlpEnc: RLPEncoder[M]): Packet = {
      val encoded = encodePacket(m, nodeKey)
      decodePacket(encoded).get
    }

    val discoveryConfig = DiscoveryConfig(Config.config, Config.blockchains.blockchainConfig.bootstrapNodes)

    val bootstrapNodes = discoveryConfig.bootstrapNodes.map(DiscoveryNodeInfo.fromNode).toSeq

    val bootNeighbours = bootstrapNodes
      .map(node => Neighbour(Endpoint.makeEndpoint(node.node.udpSocketAddress, node.node.tcpPort), node.node.id))
      .toList

    val expTimeSec = discoveryConfig.messageExpiration.toSeconds
    val discoveryListner = TestProbe()

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
        discoveryStatus = ServerStatus.Listening(localAddress)
      )

    val nodeStatusHolder = new AtomicReference(nodeStatus)
    val fakeClock = Clock.fixed(Instant.ofEpochSecond(0), ZoneId.systemDefault())
    val discoveryPeerManager = TestActorRef[PeerDiscoveryManager](
      PeerDiscoveryManager.props(
        discoveryListner.ref,
        discoveryConfig,
        storagesInstance.storages.knownNodesStorage,
        nodeStatusHolder,
        fakeClock
      )
    )

    val expectedTime = fakeClock.instant().plusSeconds(expTimeSec).getEpochSecond
    discoveryListner.expectMsg(Subscribe)

    val version = 4
    val toEndpoint = Endpoint.makeEndpoint(localAddress, port)
    val remoteEndpoint = Endpoint.makeEndpoint(remoteUdpAddress, remoteTcpPort)
    val timestamp = Long.MaxValue

    val ping = Ping(version, remoteEndpoint, toEndpoint, timestamp)
    val pingPingPacketDecoded = getPacket(ping)

    val neighboursCount = 9
    val neighbours = (1 to 9).map { n =>
      val newAddress: Array[Byte] = Array(31, 178, 1, n).map(_.toByte)
      val newId: Array[Byte] = Array.fill(64) {
        n.toByte
      }
      val socketAddress = new InetSocketAddress(InetAddress.getByAddress(newAddress), remoteUdpPort)
      val nodeId = Node(ByteString(newId), InetAddress.getByAddress(newAddress), remoteTcpPort, remoteUdpPort).id
      val neighbourEndpoint = Endpoint.makeEndpoint(socketAddress, remoteTcpPort)
      Neighbour(neighbourEndpoint, nodeId)
    }.toSeq

    val expectedMes = neighbours.map { n =>
      val ping = Ping(version, toEndpoint, n.endpoint, expectedTime)
      SendPacket(
        Packet(encodePacket(ping, nodeStatusHolder.get().key)),
        new InetSocketAddress(InetAddress.getByAddress(n.endpoint.address.toArray), n.endpoint.udpPort)
      )
    }

    val expectedBootStrapPings = bootstrapNodes.map { node =>
      (
        node,
        SendPacket(
          Packet(
            encodePacket(
              Ping(
                version,
                Endpoint.makeEndpoint(localAddress, port),
                Endpoint.makeEndpoint(node.node.udpSocketAddress, node.node.tcpPort),
                expectedTime
              ),
              nodeStatusHolder.get().key
            )
          ),
          node.node.udpSocketAddress
        )
      )
    }
  }

}
