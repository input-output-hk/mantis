package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.agent.Agent
import akka.testkit.TestProbe
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerDisconnectedClassifier}
import io.iohk.ethereum.network.PeerEventBusActor._
import io.iohk.ethereum.network.{PeerActor, PeerImpl}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.crypto.AsymmetricCipherKeyPair

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class FastSyncBlockHeadersRequestHandlerSpec extends FlatSpec with Matchers {

  "FastSyncBlockHeadersRequestHandler" should "handle successful response (and enqueue remaining receipts)" in new TestSetup {
    peerTestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(block), maxHeaders, 0, reverse = false)))
    peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))

    val responseHeaders = Seq(BlockHeader(testGenesisHash, ByteString(""), ByteString(""),
      ByteString(""), ByteString(""), ByteString(""),
      ByteString(""), 0, block, 0, 0, 0, ByteString(""), ByteString(""), ByteString("")))

    peerEventBus.reply(MessageFromPeer(BlockHeaders(responseHeaders), peer.id))

    parent.expectMsgAllOf(
      SyncController.BlockHeadersReceived(peer, responseHeaders),
      FastSync.EnqueueBlockBodies(Seq(responseHeaders.head.hash)),
      FastSync.EnqueueReceipts(Seq(responseHeaders.head.hash)))

    parent.expectMsg(SyncRequestHandler.Done)

    peerEventBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
  }

  it should "handle block header resolution request" in new TestSetup {
    val request = GetBlockHeaders(Left(block), maxHeaders, skip = 0, reverse = true)
    val resolverPeerTestProbe = TestProbe()
    val resolverPeer = new PeerImpl(new InetSocketAddress("127.0.0.2", 9000), resolverPeerTestProbe.ref, peerEventBus.ref)

    val resolver: ActorRef = {
      parent.childActorOf(SyncBlockHeadersRequestHandler.props(
        resolverPeer,
        request,
        resolveBranches = true)(time.scheduler))
    }

    resolverPeerTestProbe.expectMsg(PeerActor.SendMessage(request))
    peerEventBus.expectMsgAllOf(
      Subscribe(PeerDisconnectedClassifier(peer.id)),
      Subscribe(PeerDisconnectedClassifier(resolverPeer.id)),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(resolverPeer.id))))

    val responseHeaders = Seq(BlockHeader(testGenesisHash, ByteString(""), ByteString(""),
      ByteString(""), ByteString(""), ByteString(""),
      ByteString(""), 0, block, 0, 0, 0, ByteString(""), ByteString(""), ByteString("")))

    resolver ! MessageFromPeer(BlockHeaders(responseHeaders), resolverPeer.id)

    parent.expectMsg(SyncController.BlockHeadersToResolve(resolverPeer, responseHeaders))

    parent.expectMsg(SyncRequestHandler.Done)

    peerEventBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(resolverPeer.id)))
    peerEventBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(resolverPeer.id))))
  }

  it should "handle timeout" in new TestSetup {
    peerTestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(block), maxHeaders, 0, reverse = false)))
    peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))

    time.advance(10.seconds)

    parent.expectMsg(BlacklistSupport.BlacklistPeer(peer.id, "got time out waiting for block headers response for requested: Left(1)"))
    parent.expectMsg(SyncRequestHandler.Done)

    peerEventBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
  }

  trait TestSetup extends EphemBlockchainTestSetup {
    implicit val system = ActorSystem("FastSyncBlockHeadersRequestHandlerSpec_System")

    val testGenesisHash = ByteString("123")
    blockchain.save(testGenesisHash, 123)

    val time = new VirtualTime

    val peerEventBus = TestProbe()

    val peerTestProbe = TestProbe()
    val peer = new PeerImpl(new InetSocketAddress("127.0.0.1", 8000), peerTestProbe.ref, peerEventBus.ref)

    val requestedHashes = Seq(ByteString("1"), ByteString("2"))

    val parent = TestProbe()

    val block = BigInt(1)
    val maxHeaders = 1

    val fastSyncBlockHeadersRequestHandler: ActorRef = {
      val request = GetBlockHeaders(Left(block), maxHeaders, skip = 0, reverse = false)
      parent.childActorOf(SyncBlockHeadersRequestHandler.props(
        peer,
        request,
        resolveBranches = false)(time.scheduler))
    }
  }

}
