package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerDisconnectedClassifier}
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class FastSyncBlockHeadersRequestHandlerSpec extends FlatSpec with Matchers {

  "FastSyncBlockHeadersRequestHandler" should "handle successful response (and enqueue remaining receipts)" in new TestSetup {
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(block), maxHeaders, 0, reverse = false), peer.id))
    peerMessageBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))

    val responseHeaders = Seq(BlockHeader(testGenesisHash, ByteString(""), ByteString(""),
      ByteString(""), ByteString(""), ByteString(""),
      ByteString(""), 0, block, 0, 0, 0, ByteString(""), ByteString(""), ByteString("")))

    peerMessageBus.reply(MessageFromPeer(BlockHeaders(responseHeaders), peer.id))

    parent.expectMsgAllOf(
      SyncController.BlockHeadersReceived(peer, responseHeaders),
      FastSync.EnqueueBlockBodies(Seq(responseHeaders.head.hash)),
      FastSync.EnqueueReceipts(Seq(responseHeaders.head.hash)))

    parent.expectMsg(SyncRequestHandler.Done)

    peerMessageBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
  }

  it should "handle block header resolution request" in new TestSetup {
    etcPeerManager.expectMsgPF(){case s: EtcPeerManagerActor.SendMessage if s.peerId == peer.id => true}

    val request = GetBlockHeaders(Left(block), maxHeaders, skip = 0, reverse = true)
    val resolverPeerTestProbe = TestProbe()
    val resolverPeer = Peer(new InetSocketAddress("127.0.0.2", 9000), resolverPeerTestProbe.ref, false)

    val resolver: ActorRef = {
      parent.childActorOf(SyncBlockHeadersRequestHandler.props(
        resolverPeer,
        etcPeerManager.ref,
        peerMessageBus.ref,
        request,
        resolveBranches = true)(time.scheduler))
    }

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(request, resolverPeer.id))
    peerMessageBus.expectMsgAllOf(
      Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))),
      Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(resolverPeer.id))),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(resolverPeer.id))))

    val responseHeaders = Seq(BlockHeader(testGenesisHash, ByteString(""), ByteString(""),
      ByteString(""), ByteString(""), ByteString(""),
      ByteString(""), 0, block, 0, 0, 0, ByteString(""), ByteString(""), ByteString("")))

    resolver ! MessageFromPeer(BlockHeaders(responseHeaders), resolverPeer.id)

    parent.expectMsg(SyncController.BlockHeadersToResolve(resolverPeer, responseHeaders))

    parent.expectMsg(SyncRequestHandler.Done)

    peerMessageBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(resolverPeer.id))))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(resolverPeer.id))))
  }

  it should "handle timeout" in new TestSetup {
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(block), maxHeaders, 0, reverse = false), peer.id))
    peerMessageBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))

    time.advance(10.seconds)

    parent.expectMsg(BlacklistSupport.BlacklistPeer(peer.id, "got time out waiting for block headers response for requested: Left(1)"))
    parent.expectMsg(SyncRequestHandler.Done)

    peerMessageBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
  }

  trait TestSetup extends EphemBlockchainTestSetup {
    implicit val system = ActorSystem("FastSyncBlockHeadersRequestHandlerSpec_System")

    val testGenesisHash = ByteString("123")
    blockchain.save(testGenesisHash, 123)

    val time = new VirtualTime

    val peerTestProbe = TestProbe()

    val peer = Peer(new InetSocketAddress("127.0.0.1", 8000), peerTestProbe.ref, false)

    val etcPeerManager = TestProbe()

    val peerMessageBus = TestProbe()

    val requestedHashes = Seq(ByteString("1"), ByteString("2"))

    val parent = TestProbe()

    val block = BigInt(1)
    val maxHeaders = 1

    val fastSyncBlockHeadersRequestHandler: ActorRef = {
      val request = GetBlockHeaders(Left(block), maxHeaders, skip = 0, reverse = false)
      parent.childActorOf(SyncBlockHeadersRequestHandler.props(
        peer,
        etcPeerManager.ref,
        peerMessageBus.ref,
        request,
        resolveBranches = false)(time.scheduler))
    }
  }

}
