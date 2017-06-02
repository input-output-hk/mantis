package io.iohk.ethereum.jsonrpc

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.agent.Agent
import akka.testkit.TestProbe
import io.iohk.ethereum.crypto
import io.iohk.ethereum.jsonrpc.NetService._
import io.iohk.ethereum.network.PeerManagerActor.Peers
import io.iohk.ethereum.network.{Network, PeerActor, PeerImpl}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class NetServiceSpec extends FlatSpec with Matchers with MockFactory {

  "NetService" should "return handshaked peer count" in new TestSetup {

    (network.peers _).expects().returning(
      Future.successful(Peers(Map(
        PeerImpl(new InetSocketAddress(1), testRef, peerEventBus.ref) -> PeerActor.Status.Handshaked(mock[Status], true, 0),
        PeerImpl(new InetSocketAddress(2), testRef, peerEventBus.ref) -> PeerActor.Status.Handshaked(mock[Status], true, 0),
        PeerImpl(new InetSocketAddress(3), testRef, peerEventBus.ref) -> PeerActor.Status.Connecting)))
    )
    val resF = netService.peerCount(PeerCountRequest())

    Await.result(resF, 3.seconds) shouldBe Right(PeerCountResponse(2))
  }

  it should "return listening response" in new TestSetup {
    Await.result(netService.listening(ListeningRequest()), 3.seconds) shouldBe Right(ListeningResponse(true))
  }

  it should "return version response" in new TestSetup {
    Await.result(netService.version(VersionRequest()), 3.seconds) shouldBe Right(VersionResponse("1"))
  }

  trait TestSetup extends MockFactory {
    implicit val system = ActorSystem("Testsystem")

    val testRef = TestProbe().ref

    val peerEventBus = TestProbe()

    val nodeStatus = NodeStatus(crypto.generateKeyPair(), ServerStatus.Listening(new InetSocketAddress(9000)))
    val network = mock[Network]
    val netService = new NetService(Agent(nodeStatus), network)
  }

}
