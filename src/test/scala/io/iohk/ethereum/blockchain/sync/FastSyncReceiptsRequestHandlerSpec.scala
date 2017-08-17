package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import scala.concurrent.duration._
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.{Fixtures, Mocks}
import io.iohk.ethereum.domain.{Block, Receipt}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{MessageFromPeer, PeerDisconnected}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerDisconnectedClassifier}
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}
import io.iohk.ethereum.network.p2p.messages.PV63.{GetReceipts, Receipts}
import io.iohk.ethereum.validators.BlockValidator
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class FastSyncReceiptsRequestHandlerSpec extends FlatSpec with Matchers {

  "FastSyncReceiptsRequestHandler" should "handle successful response (and enqueue remaining receipts)" in new TestSetup {
    val fastSyncReceiptsRequestHandler = fastSyncReceiptsRequestHandlerBuilder(blockValidatorAlwaysSucceed)

    blockchain.save(block1)
    blockchain.save(block2)

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetReceipts(requestedHashes), peer.id))
    peerMessageBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))

    val responseReceipts = Seq(Seq(Receipt(ByteString(""), 0, ByteString(""), Nil)))
    peerMessageBus.reply(MessageFromPeer(Receipts(responseReceipts), peer.id))

    parent.expectMsg(FastSync.EnqueueReceipts(requestedHashes.drop(1)))
    parent.expectMsg(SyncRequestHandler.Done)

    blockchain.getReceiptsByHash(requestedHashes.head) shouldBe Some(responseReceipts.head)
    blockchain.getReceiptsByHash(requestedHashes(1)) shouldBe None

    peerMessageBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))
  }

  it should "handle timeout" in new TestSetup {
    val fastSyncReceiptsRequestHandler = fastSyncReceiptsRequestHandlerBuilder(blockValidatorAlwaysSucceed)

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetReceipts(requestedHashes), peer.id))
    peerMessageBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))

    time.advance(10.seconds)

    parent.expectMsg(BlacklistSupport.BlacklistPeer(peer.id,
      s"time out on receipts response for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}"))
    parent.expectMsg(FastSync.EnqueueReceipts(requestedHashes))
    parent.expectMsg(SyncRequestHandler.Done)

    peerMessageBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))
  }

  it should "handle peer termination" in new TestSetup {
    val fastSyncReceiptsRequestHandler = fastSyncReceiptsRequestHandlerBuilder(blockValidatorAlwaysSucceed)

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetReceipts(requestedHashes), peer.id))
    peerMessageBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))

    peerMessageBus.send(fastSyncReceiptsRequestHandler, PeerDisconnected(peer.id))

    parent.expectMsg(FastSync.EnqueueReceipts(requestedHashes))
    parent.expectMsg(SyncRequestHandler.Done)
  }

  it should "handle invalid receipts" in new TestSetup {
    val fastSyncReceiptsRequestHandler = fastSyncReceiptsRequestHandlerBuilder(blockValidatorAlwaysFail)

    blockchain.save(block1)
    blockchain.save(block2)

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetReceipts(requestedHashes), peer.id))
    peerMessageBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))

    val blockReceipts = Seq(Receipt(ByteString(""), 0, ByteString(""), Nil))
    val responseReceipts = List.fill(requestedHashes.size)(blockReceipts)
    peerMessageBus.reply(MessageFromPeer(Receipts(responseReceipts), peer.id))

    parent.expectMsgPF(){ case BlacklistSupport.BlacklistPeer(peerId, _) if peerId == peer.id => () }
    parent.expectMsg(FastSync.EnqueueReceipts(requestedHashes))
    parent.expectMsg(SyncRequestHandler.Done)

    peerMessageBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))
  }

  it should "handle block header not found on blockchain" in new TestSetup {
    val fastSyncReceiptsRequestHandler = fastSyncReceiptsRequestHandlerBuilder(blockValidatorAlwaysSucceed)

    blockchain.save(blockHeader1)

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetReceipts(requestedHashes), peer.id))
    peerMessageBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))

    val responseReceipts = Seq(Seq(Receipt(ByteString(""), 0, ByteString(""), Nil)), Nil)
    peerMessageBus.reply(MessageFromPeer(Receipts(responseReceipts), peer.id))

    parent.expectMsg(FastSync.RedownloadBlockchain)
    parent.expectMsg(SyncRequestHandler.Done)

    peerMessageBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer.id))))
  }

  trait TestSetup extends EphemBlockchainTestSetup {
    implicit val system = ActorSystem("FastSyncReceiptsRequestHandlerSpec_System")

    val time = new VirtualTime

    val peerTestProbe = TestProbe()
    val peer = Peer(new InetSocketAddress("127.0.0.1", 8900), peerTestProbe.ref, false)

    val parent = TestProbe()
    val etcPeerManager = TestProbe()
    val peerMessageBus = TestProbe()

    val blockValidatorAlwaysSucceed = Mocks.MockValidatorsAlwaysSucceed.blockValidator
    val blockValidatorAlwaysFail = Mocks.MockValidatorsAlwaysFail.blockValidator

    val blockHeader1 = Fixtures.Blocks.DaoForkBlock.header
    val blockBody1 = Fixtures.Blocks.DaoForkBlock.body
    val block1 = Block(blockHeader1, blockBody1)

    val blockHeader2 = Fixtures.Blocks.Block3125369.header
    val blockBody2 = Fixtures.Blocks.Block3125369.body
    val block2 = Block(blockHeader2, blockBody2)

    val requestedHashes = Seq(blockHeader1.hash, blockHeader2.hash)

    def fastSyncReceiptsRequestHandlerBuilder(blockValidator: BlockValidator): ActorRef =
      parent.childActorOf(FastSyncReceiptsRequestHandler.props(
        peer,
        etcPeerManager.ref,
        peerMessageBus.ref,
        requestedHashes,
        storagesInstance.storages.appStateStorage,
        blockchain,
        blockValidator)(time.scheduler))
  }

}
