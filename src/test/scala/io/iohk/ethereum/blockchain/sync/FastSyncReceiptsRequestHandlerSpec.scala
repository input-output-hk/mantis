package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import scala.concurrent.duration._
import akka.actor.{ActorSystem, PoisonPill, Terminated}
import akka.testkit.TestProbe
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{MessageFromPeer, PeerDisconnected}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerDisconnectedClassifier}
import io.iohk.ethereum.network.{PeerActor, PeerImpl}
import io.iohk.ethereum.network.PeerEventBusActor._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetReceipts, Receipts}
import org.scalatest.{FlatSpec, Matchers}

class FastSyncReceiptsRequestHandlerSpec extends FlatSpec with Matchers {

  "FastSyncReceiptsRequestHandler" should "handle successful response (and enqueue remaining receipts)" in new TestSetup {
    peerTestProbe.expectMsg(PeerActor.SendMessage(GetReceipts(requestedHashes)))
    peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))

    val responseReceipts = Seq(Seq(Receipt(ByteString(""), 0, ByteString(""), Nil)))
    peerEventBus.reply(MessageFromPeer(Receipts(responseReceipts), peer.id))

    parent.expectMsg(FastSync.EnqueueReceipts(requestedHashes.drop(1)))
    parent.expectMsg(SyncRequestHandler.Done)

    blockchain.getReceiptsByHash(requestedHashes.head) shouldBe Some(responseReceipts.head)
    blockchain.getReceiptsByHash(requestedHashes(1)) shouldBe None

    peerEventBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Unsubscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))
  }

  it should "handle timeout" in new TestSetup {
    peerTestProbe.expectMsg(PeerActor.SendMessage(GetReceipts(requestedHashes)))
    peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))

    time.advance(10.seconds)

    parent.expectMsg(BlacklistSupport.BlacklistPeer(peer.id, "time out on receipts response for known hashes: List(31, 32)"))
    parent.expectMsg(FastSync.EnqueueReceipts(requestedHashes))
    parent.expectMsg(SyncRequestHandler.Done)

    peerEventBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Unsubscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))
  }

  it should "handle peer termination" in new TestSetup {
    peerTestProbe.expectMsg(PeerActor.SendMessage(GetReceipts(requestedHashes)))
    peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))

    peer.ref ! PoisonPill

    parent.expectTerminated(peer.ref)
    peerEventBus.send(fastSyncReceiptsRequestHandler, PeerDisconnected(peer.id))
    parent.expectMsg(FastSync.EnqueueReceipts(requestedHashes))
    parent.expectMsg(SyncRequestHandler.Done)
  }

  trait TestSetup extends EphemBlockchainTestSetup  {
    implicit val system = ActorSystem("FastSyncReceiptsRequestHandlerSpec_System")

    val time = new VirtualTime

    val requestedHashes = Seq(ByteString("1"), ByteString("2"))

    val parent = TestProbe()

    val peerEventBus = TestProbe()

    val peerTestProbe = TestProbe()
    val peer = new PeerImpl(peerTestProbe.ref, peerEventBus.ref)

    parent watch peer.ref
    val fastSyncReceiptsRequestHandler =
      parent.childActorOf(FastSyncReceiptsRequestHandler.props(
        peer,
        requestedHashes,
        storagesInstance.storages.appStateStorage,
        blockchain)(time.scheduler))
  }

}
