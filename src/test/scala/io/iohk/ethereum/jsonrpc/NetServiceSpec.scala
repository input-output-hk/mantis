package io.iohk.ethereum.jsonrpc

import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import io.iohk.ethereum.jsonrpc.NetService._
import io.iohk.ethereum.jsonrpc.security.SecureRandomBuilder
import io.iohk.ethereum.network.{Peer, PeerActor, PeerManagerActor}
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}
import io.iohk.ethereum.{NormalPatience, crypto}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class NetServiceSpec extends AnyFlatSpec with Matchers with ScalaFutures with NormalPatience with SecureRandomBuilder {

  "NetService" should "return handshaked peer count" in new TestSetup {
    val resF = netService
      .peerCount(PeerCountRequest())
      .runToFuture

    peerManager.expectMsg(PeerManagerActor.GetPeers)
    peerManager.reply(
      PeerManagerActor.Peers(
        Map(
          Peer(new InetSocketAddress(1), testRef, false) -> PeerActor.Status.Handshaked,
          Peer(new InetSocketAddress(2), testRef, false) -> PeerActor.Status.Handshaked,
          Peer(new InetSocketAddress(3), testRef, false) -> PeerActor.Status.Connecting
        )
      )
    )

    resF.futureValue shouldBe Right(PeerCountResponse(2))
  }

  it should "return listening response" in new TestSetup {
    netService.listening(ListeningRequest()).runSyncUnsafe() shouldBe Right(ListeningResponse(true))
  }

  it should "return version response" in new TestSetup {
    netService.version(VersionRequest()).runSyncUnsafe() shouldBe Right(VersionResponse("42"))
  }

  trait TestSetup {
    implicit val system = ActorSystem("Testsystem")

    val testRef = TestProbe().ref

    val peerManager = TestProbe()

    val nodeStatus = NodeStatus(
      crypto.generateKeyPair(secureRandom),
      ServerStatus.Listening(new InetSocketAddress(9000)),
      discoveryStatus = ServerStatus.NotListening
    )
    val netService =
      new NetService(new AtomicReference[NodeStatus](nodeStatus), peerManager.ref, NetServiceConfig(5.seconds))
  }
}
