package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration._
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockBody, GetBlockBodies}
import org.scalatest.{FlatSpec, Matchers}
import io.iohk.ethereum.blockchain.sync.SyncController.BlockBodiesReceived

class FastSyncBlockBodiesRequestHandlerSpec extends FlatSpec with Matchers {

  "FastSyncBlockBodiesRequestHandler" should "handle successful response (and enqueue remaining hashes)" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetBlockBodies(requestedHashes)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockBodies.code)))

    val responseBodies = Seq(BlockBody(Nil, Nil))
    peer.reply(PeerActor.MessageReceived(BlockBodies(responseBodies)))

    parent.expectMsg(BlockBodiesReceived(peer.ref, requestedHashes, responseBodies))
    parent.expectMsg(SyncRequestHandler.Done)

    peer.expectMsg(PeerActor.Unsubscribe)
  }

  it should "blacklist if the response is empty" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetBlockBodies(requestedHashes)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockBodies.code)))

    val responseBodies = Nil
    peer.reply(PeerActor.MessageReceived(BlockBodies(responseBodies)))

    parent.expectMsg(BlacklistSupport.BlacklistPeer(peer.ref, "got empty block bodies response for known hashes: List(31, 32)"))
    parent.expectMsg(SyncRequestHandler.Done)

    peer.expectMsg(PeerActor.Unsubscribe)
  }

  it should "handle timeout" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetBlockBodies(requestedHashes)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockBodies.code)))

    time.advance(10.seconds)

    parent.expectMsg(BlacklistSupport.BlacklistPeer(peer.ref, "time out on block bodies response for known hashes: List(31, 32)"))
    parent.expectMsg(FastSync.EnqueueBlockBodies(requestedHashes))
    parent.expectMsg(SyncRequestHandler.Done)

    peer.expectMsg(PeerActor.Unsubscribe)
  }

  trait TestSetup {
    implicit val system = ActorSystem("FastSyncBlockBodiesRequestHandlerSpec_System")

    val time = new VirtualTime

    val peer = TestProbe()

    val requestedHashes = Seq(ByteString("1"), ByteString("2"))

    val parent = TestProbe()

    val fastSyncBlockBodiesRequestHandler: ActorRef =
      parent.childActorOf(SyncBlockBodiesRequestHandler.props(
        peer.ref,
        requestedHashes)(time.scheduler))
  }

}
