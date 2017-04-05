package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration._
import akka.actor.{ActorSystem, PoisonPill}
import akka.testkit.TestProbe
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.network.p2p.messages.PV63.{GetReceipts, Receipts}
import org.scalatest.{FlatSpec, Matchers}

class FastSyncReceiptsRequestHandlerSpec extends FlatSpec with Matchers {

  "FastSyncReceiptsRequestHandler" should "handle successful response (and enqueue remaining receipts)" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetReceipts(requestedHashes)))
    peer.expectMsg(PeerActor.Subscribe(Set(Receipts.code)))

    val responseReceipts = Seq(Seq(Receipt(ByteString(""), 0, ByteString(""), Nil)))
    peer.reply(PeerActor.MessageReceived(Receipts(responseReceipts)))

    parent.expectMsg(FastSync.EnqueueReceipts(requestedHashes.drop(1)))
    parent.expectMsg(SyncRequestHandler.Done)

    blockchain.getReceiptsByHash(requestedHashes.head) shouldBe Some(responseReceipts.head)
    blockchain.getReceiptsByHash(requestedHashes(1)) shouldBe None

    peer.expectMsg(PeerActor.Unsubscribe)
  }

  it should "handle timeout" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetReceipts(requestedHashes)))
    peer.expectMsg(PeerActor.Subscribe(Set(Receipts.code)))

    time.advance(10.seconds)

    parent.expectMsg(BlacklistSupport.BlacklistPeer(peer.ref, "time out on receipts response for known hashes: List(31, 32)"))
    parent.expectMsg(FastSync.EnqueueReceipts(requestedHashes))
    parent.expectMsg(SyncRequestHandler.Done)

    peer.expectMsg(PeerActor.Unsubscribe)
  }

  it should "handle peer termination" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetReceipts(requestedHashes)))
    peer.expectMsg(PeerActor.Subscribe(Set(Receipts.code)))

    peer.ref ! PoisonPill

    parent.expectMsg(FastSync.EnqueueReceipts(requestedHashes))
    parent.expectMsg(SyncRequestHandler.Done)
  }

  trait TestSetup extends EphemBlockchainTestSetup  {
    implicit val system = ActorSystem("FastSyncReceiptsRequestHandlerSpec_System")

    val time = new VirtualTime

    val peer = TestProbe()

    val requestedHashes = Seq(ByteString("1"), ByteString("2"))

    val parent = TestProbe()

    val fastSyncReceiptsRequestHandler =
      parent.childActorOf(FastSyncReceiptsRequestHandler.props(
        peer.ref,
        requestedHashes,
        storagesInstance.storages.appStateStorage,
        blockchain)(time.scheduler))
  }

}
