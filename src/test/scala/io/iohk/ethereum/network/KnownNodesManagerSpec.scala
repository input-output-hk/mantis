package io.iohk.ethereum.network

import java.net.URI

import akka.actor.{ActorSystem, Props}
import akka.testkit.TestProbe
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.network.KnownNodesManager.KnownNodesManagerConfig

import scala.concurrent.duration._
import org.scalatest.{FlatSpec, Matchers}

class KnownNodesManagerSpec extends FlatSpec with Matchers {

  "KnownNodesManager" should "keep a list of nodes and persist changes" in new TestSetup {
    val uri1 = new URI("enode://test1@test1.com:9000")
    val uri2 = new URI("enode://test2@test2.com:9000")
    val uri3 = new URI("enode://test3@test3.com:9000")
    val uri4 = new URI("enode://test4@test4.com:9000")

    knownNodesManager.tell(KnownNodesManager.GetKnownNodes, client.ref)
    client.expectMsg(KnownNodesManager.KnownNodes(Set.empty))

    knownNodesManager.tell(KnownNodesManager.AddKnownNode(uri1), client.ref)
    knownNodesManager.tell(KnownNodesManager.AddKnownNode(uri2), client.ref)
    knownNodesManager.tell(KnownNodesManager.GetKnownNodes, client.ref)
    client.expectMsg(KnownNodesManager.KnownNodes(Set(uri1, uri2)))
    storagesInstance.storages.knownNodesStorage.getKnownNodes() shouldBe Set.empty

    time.advance(config.persistInterval + 1.seconds)

    storagesInstance.storages.knownNodesStorage.getKnownNodes() shouldBe Set(uri1, uri2)

    knownNodesManager.tell(KnownNodesManager.AddKnownNode(uri3), client.ref)
    knownNodesManager.tell(KnownNodesManager.AddKnownNode(uri4), client.ref)
    knownNodesManager.tell(KnownNodesManager.RemoveKnownNode(uri1), client.ref)
    knownNodesManager.tell(KnownNodesManager.RemoveKnownNode(uri4), client.ref)
    knownNodesManager.tell(KnownNodesManager.GetKnownNodes, client.ref)
    client.expectMsg(KnownNodesManager.KnownNodes(Set(uri2, uri3)))

    time.advance(config.persistInterval + 1.seconds)

    storagesInstance.storages.knownNodesStorage.getKnownNodes() shouldBe Set(uri2, uri3)
  }

  trait TestSetup {
    implicit val system = ActorSystem("KnownNodesManagerSpec_System")

    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages

    val time = new VirtualTime
    val config = KnownNodesManagerConfig(5.seconds)

    val client = TestProbe()

    val knownNodesManager = system.actorOf(Props(new KnownNodesManager(config, storagesInstance.storages.knownNodesStorage, Some(time.scheduler))))
  }

}
