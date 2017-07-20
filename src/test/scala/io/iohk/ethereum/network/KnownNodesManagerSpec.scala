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
    knownNodesManager.tell(KnownNodesManager.GetKnownNodes, client.ref)
    client.expectMsg(KnownNodesManager.KnownNodes(Set.empty))

    knownNodesManager.tell(KnownNodesManager.AddKnownNode(uri(1)), client.ref)
    knownNodesManager.tell(KnownNodesManager.AddKnownNode(uri(2)), client.ref)
    knownNodesManager.tell(KnownNodesManager.GetKnownNodes, client.ref)
    client.expectMsg(KnownNodesManager.KnownNodes(Set(uri(1), uri(2))))
    storagesInstance.storages.knownNodesStorage.getKnownNodes() shouldBe Set.empty

    time.advance(config.persistInterval + 10.seconds)

    knownNodesManager.tell(KnownNodesManager.GetKnownNodes, client.ref)
    client.expectMsg(KnownNodesManager.KnownNodes(Set(uri(1), uri(2))))
    storagesInstance.storages.knownNodesStorage.getKnownNodes() shouldBe Set(uri(1), uri(2))

    knownNodesManager.tell(KnownNodesManager.AddKnownNode(uri(3)), client.ref)
    knownNodesManager.tell(KnownNodesManager.AddKnownNode(uri(4)), client.ref)
    knownNodesManager.tell(KnownNodesManager.RemoveKnownNode(uri(1)), client.ref)
    knownNodesManager.tell(KnownNodesManager.RemoveKnownNode(uri(4)), client.ref)

    time.advance(config.persistInterval + 10.seconds)

    knownNodesManager.tell(KnownNodesManager.GetKnownNodes, client.ref)
    client.expectMsg(KnownNodesManager.KnownNodes(Set(uri(2), uri(3))))

    storagesInstance.storages.knownNodesStorage.getKnownNodes() shouldBe Set(uri(2), uri(3))
  }

  it should "respect max nodes limit" in new TestSetup {
    knownNodesManager.tell(KnownNodesManager.GetKnownNodes, client.ref)
    client.expectMsg(KnownNodesManager.KnownNodes(Set.empty))

    (1 to 10).foreach { n =>
      knownNodesManager.tell(KnownNodesManager.AddKnownNode(uri(n)), client.ref)
    }
    time.advance(config.persistInterval + 1.seconds)

    knownNodesManager.tell(KnownNodesManager.GetKnownNodes, client.ref)
    client.expectMsgClass(classOf[KnownNodesManager.KnownNodes])

    storagesInstance.storages.knownNodesStorage.getKnownNodes().size shouldBe 5
  }

  trait TestSetup {
    implicit val system = ActorSystem("KnownNodesManagerSpec_System")

    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages

    val time = new VirtualTime
    val config = KnownNodesManagerConfig(persistInterval = 5.seconds, maxPersistedNodes = 5)

    val client = TestProbe()

    def uri(n: Int): URI = new URI(s"enode://test$n@test$n.com:9000")

    val knownNodesManager = system.actorOf(Props(new KnownNodesManager(config, storagesInstance.storages.knownNodesStorage, Some(time.scheduler))))
  }

}
