package io.iohk.ethereum.jsonrpc

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.agent.Agent
import akka.testkit.TestProbe
import io.iohk.ethereum.{NormalPatience, crypto}
import io.iohk.ethereum.jsonrpc.NetService._
import io.iohk.ethereum.network.{Peer, PeerActor, PeerManagerActor}
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global

class NetServiceSpec extends FlatSpec with Matchers with ScalaFutures with NormalPatience {

  "NetService" should "return handshaked peer count" in new TestSetup {
    val resF = netService.peerCount(PeerCountRequest())

    peerManager.expectMsg(PeerManagerActor.GetPeers)
    peerManager.reply(PeerManagerActor.Peers(Map(
      Peer(new InetSocketAddress(1), testRef) -> PeerActor.Status.Handshaked,
      Peer(new InetSocketAddress(2), testRef) -> PeerActor.Status.Handshaked,
      Peer(new InetSocketAddress(3), testRef) -> PeerActor.Status.Connecting)))

    resF.futureValue shouldBe Right(PeerCountResponse(2))
  }

  it should "return listening response" in new TestSetup {
    netService.listening(ListeningRequest()).futureValue shouldBe Right(ListeningResponse(true))
  }

  it should "return version response" in new TestSetup {
    netService.version(VersionRequest()).futureValue shouldBe Right(VersionResponse("1"))
  }

  trait TestSetup {
    implicit val system = ActorSystem("Testsystem")

    val testRef = TestProbe().ref

    val peerManager = TestProbe()

    val nodeStatus = NodeStatus(crypto.generateKeyPair(), ServerStatus.Listening(new InetSocketAddress(9000)),
      discoveryStatus = ServerStatus.NotListening)
    val netService = new NetService(Agent(nodeStatus), peerManager.ref)
  }

}
