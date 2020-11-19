package io.iohk.ethereum.network.discovery

import akka.pattern.ask
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestKit}
import akka.util.{ByteString, Timeout}
import cats.effect.Resource
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.db.storage.KnownNodesStorage
import io.iohk.scalanet.discovery.crypto.PublicKey
import io.iohk.scalanet.discovery.ethereum.v4.DiscoveryService
import io.iohk.scalanet.discovery.ethereum.{Node => ENode}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.ScalaFutures
import org.scalamock.scalatest.MockFactory
import scala.concurrent.duration._
import scala.concurrent.Await
import scodec.bits.BitVector
import scala.util.control.NoStackTrace
import io.iohk.ethereum.NormalPatience

class PeerDiscoveryManagerSpec
    extends TestKit(ActorSystem("PeerDiscoveryManagerSpec_System"))
    with AnyFlatSpecLike
    with Matchers
    with Eventually
    with MockFactory
    with ScalaFutures
    with NormalPatience {

  implicit val scheduler = Scheduler.Implicits.global
  implicit val timeout: Timeout = 1.second

  val defaultConfig = DiscoveryConfig(Config.config, Set.empty)

  val sampleKnownUris = Set(
    "enode://a59e33ccd2b3e52d578f1fbd70c6f9babda2650f0760d6ff3b37742fdcdfdb3defba5d56d315b40c46b70198c7621e63ffa3f987389c7118634b0fefbbdfa7fd@51.158.191.43:38556?discport=38556",
    "enode://651b484b652c07c72adebfaaf8bc2bd95b420b16952ef3de76a9c00ef63f07cca02a20bd2363426f9e6fe372cef96a42b0fec3c747d118f79fd5e02f2a4ebd4e@51.158.190.99:45678?discport=45678",
    "enode://9b1bf9613d859ac2071d88509ab40a111b75c1cfc51f4ad78a1fdbb429ff2405de0dc5ea8ae75e6ac88e03e51a465f0b27b517e78517f7220ae163a2e0692991@51.158.190.99:30426?discport=30426"
  ).map(new java.net.URI(_))

  val sampleBootstrapNodes = Set(
    "enode://111bd28d5b2c1378d748383fd83ff59572967c317c3063a9f475a26ad3f1517642a164338fb5268d4e32ea1cc48e663bd627dec572f1d201c7198518e5a506b1@88.99.216.30:45834?discport=45834",
    "enode://2b69a3926f36a7748c9021c34050be5e0b64346225e477fe7377070f6289bd363b2be73a06010fd516e6ea3ee90778dd0399bc007bb1281923a79374f842675a@51.15.116.226:30303?discport=30303"
  ).map(new java.net.URI(_)).map(Node.fromUri)

  trait Fixture {
    lazy val discoveryConfig = defaultConfig
    lazy val knownNodesStorage = mock[KnownNodesStorage]
    lazy val discoveryService = mock[DiscoveryService]
    lazy val discoveryServiceResource = Resource.pure[Task, DiscoveryService](discoveryService)

    lazy val peerDiscoveryManager =
      TestActorRef[PeerDiscoveryManager](
        PeerDiscoveryManager.props(
          localNodeId = ByteString.fromString("test-node"),
          discoveryConfig = discoveryConfig,
          knownNodesStorage = knownNodesStorage,
          discoveryServiceResource = discoveryServiceResource
        )
      )

    def getPeers =
      (peerDiscoveryManager ? PeerDiscoveryManager.GetDiscoveredNodesInfo)
        .mapTo[PeerDiscoveryManager.DiscoveredNodesInfo]

    def test(): Unit
  }

  def test(fixture: Fixture): Unit = {
    try {
      fixture.test()
    } finally {
      system.stop(fixture.peerDiscoveryManager)
    }
  }

  def toENode(node: Node): ENode =
    ENode(
      id = PublicKey(BitVector(node.id.toArray[Byte])),
      address = ENode.Address(ip = node.addr, udpPort = node.udpPort, tcpPort = node.tcpPort)
    )

  behavior of "PeerDiscoveryManager"

  it should "serve no peers if discovery is disabled and known peers are disabled and the manager isn't started" in test {
    new Fixture {
      override lazy val discoveryConfig =
        defaultConfig.copy(discoveryEnabled = false, reuseKnownNodes = false, bootstrapNodes = Set.empty)

      override def test(): Unit = {
        getPeers.futureValue.nodes shouldBe empty
      }
    }
  }

  it should "serve the bootstrap nodes if known peers are reused even discovery isn't enabled and the manager isn't started" in test {
    new Fixture {
      override lazy val discoveryConfig =
        defaultConfig.copy(discoveryEnabled = false, reuseKnownNodes = true, bootstrapNodes = sampleBootstrapNodes)

      override def test(): Unit = {
        getPeers.futureValue.nodes should contain theSameElementsAs (sampleBootstrapNodes)
      }
    }
  }

  it should "serve the known peers if discovery is enabled and the manager isn't started" in test {
    new Fixture {
      override lazy val discoveryConfig =
        defaultConfig.copy(discoveryEnabled = true, reuseKnownNodes = true, bootstrapNodes = Set.empty)

      (knownNodesStorage.getKnownNodes _)
        .expects()
        .returning(sampleKnownUris)
        .once()

      override def test(): Unit = {
        getPeers.futureValue.nodes.map(_.toUri) should contain theSameElementsAs (sampleKnownUris)
      }
    }
  }

  it should "merge the known peers with the service if it's started" in test {
    new Fixture {
      override lazy val discoveryConfig =
        defaultConfig.copy(discoveryEnabled = true, reuseKnownNodes = true, bootstrapNodes = Set.empty)

      val sampleNodes = sampleBootstrapNodes

      (knownNodesStorage.getKnownNodes _)
        .expects()
        .returning(sampleKnownUris)
        .once()

      (discoveryService.getNodes _)
        .expects()
        .returning(Task(sampleNodes.map(toENode)))
        .once()

      val expected = sampleKnownUris ++ sampleNodes.map(_.toUri)

      override def test(): Unit = {
        peerDiscoveryManager ! PeerDiscoveryManager.Start
        eventually {
          getPeers.futureValue.nodes.map(_.toUri) should contain theSameElementsAs (expected)
        }
      }
    }
  }

  it should "keep serving the known peers if the service fails to start" in test {
    new Fixture {
      override lazy val discoveryConfig =
        defaultConfig.copy(discoveryEnabled = true, reuseKnownNodes = true, bootstrapNodes = Set.empty)

      @volatile var started = false

      override lazy val discoveryServiceResource: Resource[Task, DiscoveryService] =
        Resource.liftF {
          Task { started = true } >>
            Task.raiseError[DiscoveryService](new RuntimeException("Oh no!") with NoStackTrace)
        }

      (knownNodesStorage.getKnownNodes _)
        .expects()
        .returning(sampleKnownUris)
        .once()

      override def test(): Unit = {
        peerDiscoveryManager ! PeerDiscoveryManager.Start
        eventually {
          started shouldBe true
        }
        getPeers.futureValue.nodes should have size (sampleKnownUris.size)
      }
    }
  }

  it should "stop using the service after it is stopped" in test {
    new Fixture {
      override lazy val discoveryConfig =
        defaultConfig.copy(discoveryEnabled = true, reuseKnownNodes = true, bootstrapNodes = Set.empty)

      (knownNodesStorage.getKnownNodes _)
        .expects()
        .returning(sampleKnownUris)
        .once()

      (discoveryService.getNodes _)
        .expects()
        .returning(Task(sampleBootstrapNodes.map(toENode)))
        .once()

      override def test(): Unit = {
        peerDiscoveryManager ! PeerDiscoveryManager.Start
        eventually {
          getPeers.futureValue.nodes should have size (sampleKnownUris.size + sampleBootstrapNodes.size)
        }
        peerDiscoveryManager ! PeerDiscoveryManager.Stop
        eventually {
          getPeers.futureValue.nodes should have size (sampleKnownUris.size)
        }
      }
    }
  }

  it should "propagate any error from the service to the caller" in test {
    new Fixture {
      override lazy val discoveryConfig =
        defaultConfig.copy(discoveryEnabled = true, reuseKnownNodes = false, bootstrapNodes = Set.empty)

      (discoveryService.getNodes _)
        .expects()
        .returning(Task.raiseError(new RuntimeException("Oh no!") with NoStackTrace))
        .atLeastOnce()

      override def test(): Unit = {
        peerDiscoveryManager ! PeerDiscoveryManager.Start
        eventually {
          a[RuntimeException] shouldBe thrownBy(Await.result(getPeers, 50.millis))
        }
      }
    }
  }
}
