package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import scala.concurrent.duration._
import akka.actor.{ActorSystem, PoisonPill}
import akka.testkit.TestProbe
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerActor}
import io.iohk.ethereum.network.p2p.messages.PV63.{GetReceipts, Receipts}
import org.scalatest.{FlatSpec, Matchers}

class FastSyncReceiptsRequestHandlerSpec extends FlatSpec with Matchers {

  "FastSyncReceiptsRequestHandler" should "handle successful response (and enqueue remaining receipts)" in new TestSetup {
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetReceipts(requestedHashes), peer.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))

    val responseReceipts = Seq(Seq(Receipt(ByteString(""), 0, ByteString(""), Nil)))
    peerMessageBus.reply(MessageFromPeer(Receipts(responseReceipts), peer.id))

    parent.expectMsg(FastSync.EnqueueReceipts(requestedHashes.drop(1)))
    parent.expectMsg(SyncRequestHandler.Done)

    blockchain.getReceiptsByHash(requestedHashes.head) shouldBe Some(responseReceipts.head)
    blockchain.getReceiptsByHash(requestedHashes(1)) shouldBe None

    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))
  }

  it should "handle timeout" in new TestSetup {
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetReceipts(requestedHashes), peer.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))

    time.advance(10.seconds)

    parent.expectMsg(BlacklistSupport.BlacklistPeer(peer.id, "time out on receipts response for known hashes: List(31, 32)"))
    parent.expectMsg(FastSync.EnqueueReceipts(requestedHashes))
    parent.expectMsg(SyncRequestHandler.Done)

    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))
  }

  it should "handle peer termination" in new TestSetup {
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetReceipts(requestedHashes), peer.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))

    peer.ref ! PoisonPill

    parent.expectMsg(FastSync.EnqueueReceipts(requestedHashes))
    parent.expectMsg(SyncRequestHandler.Done)
  }

  trait TestSetup extends EphemBlockchainTestSetup  {
    implicit val system = ActorSystem("FastSyncReceiptsRequestHandlerSpec_System")

    val time = new VirtualTime

    val peerTestProbe = TestProbe()
    val peer = Peer(new InetSocketAddress("127.0.0.1", 8900), peerTestProbe.ref)

    val requestedHashes = Seq(ByteString("1"), ByteString("2"))

    val parent = TestProbe()

    val etcPeerManager = TestProbe()

    val peerMessageBus = TestProbe()

    val fastSyncReceiptsRequestHandler =
      parent.childActorOf(FastSyncReceiptsRequestHandler.props(
        peer,
        etcPeerManager.ref,
        peerMessageBus.ref,
        requestedHashes,
        storagesInstance.storages.appStateStorage,
        blockchain)(time.scheduler))
  }

}
