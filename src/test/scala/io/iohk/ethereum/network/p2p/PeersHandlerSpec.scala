package io.iohk.ethereum.network.p2p

import java.net.InetSocketAddress

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorSystem
import akka.agent.Agent
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.crypto
import io.iohk.ethereum.network.BlockBroadcastActor.StartBlockBroadcast
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peer, PeersResponse}
import io.iohk.ethereum.network.PeersHandler.RequestPeers
import io.iohk.ethereum.network.{BlockBroadcastActor, PeerActor}
import io.iohk.ethereum.utils.{BlockchainStatus, NodeStatus, ServerStatus}
import org.scalatest.{FlatSpec, Matchers}

class PeersHandlerSpec extends FlatSpec with Matchers {

  val NumberPeers = 5

  it should "send no block if there are no blocks to send" in new TestSetup {
    blockBroadcastSetup()

    blockBroadcast ! RequestPeers

    peerManager.expectMsg(GetPeers)
    peerManager.reply(blockBroadcast, PeersResponse(peer +: otherPeers))

    peerProbe.expectNoMsg()
    otherPeersProbes.foreach { p => p.expectNoMsg() }
  }

  trait TestSetup extends EphemBlockchainTestSetup {
    implicit val system = ActorSystem("PeersHandlerSpec_System")

    val nodeKey = crypto.generateKeyPair()

    val nodeStatus = NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening,
      blockchainStatus = BlockchainStatus(0, ByteString("changeme"), 0))

    val nodeStatusHolder = Agent(nodeStatus)

    val peerProbe = TestProbe()
    val peer = Peer(new InetSocketAddress("localhost", NumberPeers), peerProbe.ref)
    val otherPeersProbes: Seq[TestProbe] = (0 until NumberPeers).map(_ => TestProbe())
    val otherPeers: Seq[Peer] = otherPeersProbes.zipWithIndex.map { case (testProbe, i) =>
      Peer(new InetSocketAddress("localhost", i), testProbe.ref)
    }
    val peerManager = TestProbe()

    val blockBroadcast = TestActorRef(BlockBroadcastActor.props(nodeStatusHolder,
      peerProbe.ref,
      peerManager.ref,
      blockchain))

    def blockBroadcastSetup(): Unit = {
      blockBroadcast ! StartBlockBroadcast
      peerProbe.expectMsgClass(classOf[PeerActor.Subscribe])
    }
  }
}
