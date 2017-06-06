package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import scala.concurrent.duration._
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.network.{PeerActor, PeerImpl}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockBody, GetBlockBodies}
import org.scalatest.{FlatSpec, Matchers}
import io.iohk.ethereum.blockchain.sync.SyncController.BlockBodiesReceived
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerDisconnectedClassifier}
import io.iohk.ethereum.network.PeerEventBusActor._

class FastSyncBlockBodiesRequestHandlerSpec extends FlatSpec with Matchers {

  "FastSyncBlockBodiesRequestHandler" should "handle successful response (and enqueue remaining hashes)" in new TestSetup {
    peerTestProbe.expectMsg(PeerActor.SendMessage(GetBlockBodies(requestedHashes)))

    peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))))

    val responseBodies = Seq(BlockBody(Nil, Nil))
    peerEventBus.reply(MessageFromPeer(BlockBodies(responseBodies), peer.id))

    parent.expectMsg(BlockBodiesReceived(peer, requestedHashes, responseBodies))
    parent.expectMsg(SyncRequestHandler.Done)

    peerEventBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))))
  }

  it should "blacklist if the response is empty" in new TestSetup {
    peerTestProbe.expectMsg(PeerActor.SendMessage(GetBlockBodies(requestedHashes)))
    peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))))

    val responseBodies = Nil
    peerEventBus.reply(MessageFromPeer(BlockBodies(responseBodies), peer.id))

    parent.expectMsg(BlacklistSupport.BlacklistPeer(peer.id, "got empty block bodies response for known hashes: List(31, 32)"))
    parent.expectMsg(SyncRequestHandler.Done)

    peerEventBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))))
  }

  it should "handle timeout" in new TestSetup {
    peerTestProbe.expectMsg(PeerActor.SendMessage(GetBlockBodies(requestedHashes)))
    peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))))

    time.advance(10.seconds)

    parent.expectMsg(BlacklistSupport.BlacklistPeer(peer.id, "time out on block bodies response for known hashes: List(31, 32)"))
    parent.expectMsg(FastSync.EnqueueBlockBodies(requestedHashes))
    parent.expectMsg(SyncRequestHandler.Done)

    peerEventBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(peer.id)))
    peerEventBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))))
  }

  trait TestSetup {
    implicit val system = ActorSystem("FastSyncBlockBodiesRequestHandlerSpec_System")

    val time = new VirtualTime

    val peerEventBus = TestProbe()

    val peerTestProbe = TestProbe()
    val peer = new PeerImpl(new InetSocketAddress("127.0.0.1", 8000), peerTestProbe.ref, peerEventBus.ref)

    val requestedHashes = Seq(ByteString("1"), ByteString("2"))

    val parent = TestProbe()

    val fastSyncBlockBodiesRequestHandler: ActorRef =
      parent.childActorOf(SyncBlockBodiesRequestHandler.props(
        peer,
        requestedHashes)(time.scheduler))
  }

}
