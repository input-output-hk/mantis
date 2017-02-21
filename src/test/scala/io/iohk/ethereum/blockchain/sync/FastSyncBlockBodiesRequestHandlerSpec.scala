package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration._

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.BlockBodiesStorage
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, GetBlockBodies, BlockBodies}
import org.scalatest.{FlatSpec, Matchers}

class FastSyncBlockBodiesRequestHandlerSpec extends FlatSpec with Matchers {

  "FastSyncBlockBodiesRequestHandler" should "handle successful response (and enqueue remaining hashes)" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetBlockBodies(requestedHashes)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockBodies.code)))

    val responseBodies = Seq(BlockBody(Nil, Nil))
    peer.reply(PeerActor.MessageReceived(BlockBodies(responseBodies)))

    parent.expectMsg(FastSyncController.EnqueueBlockBodies(requestedHashes.drop(1)))
    parent.expectMsg(FastSyncRequestHandler.Done)

    blockBodiesStorage.get(requestedHashes.head) shouldBe Some(responseBodies.head)
    blockBodiesStorage.get(requestedHashes(1)) shouldBe None

    peer.expectMsg(PeerActor.Unsubscribe)
  }

  it should "blacklist if the response is empty" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetBlockBodies(requestedHashes)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockBodies.code)))

    val responseBodies = Nil
    peer.reply(PeerActor.MessageReceived(BlockBodies(responseBodies)))

    parent.expectMsg(BlacklistSupport.BlacklistPeer(peer.ref))
    parent.expectMsg(FastSyncController.EnqueueBlockBodies(requestedHashes))
    parent.expectMsg(FastSyncRequestHandler.Done)

    peer.expectMsg(PeerActor.Unsubscribe)
  }

  it should "handle timeout" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetBlockBodies(requestedHashes)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockBodies.code)))

    time.advance(10.seconds)

    parent.expectMsg(BlacklistSupport.BlacklistPeer(peer.ref))
    parent.expectMsg(FastSyncController.EnqueueBlockBodies(requestedHashes))
    parent.expectMsg(FastSyncRequestHandler.Done)

    peer.expectMsg(PeerActor.Unsubscribe)
  }

  trait TestSetup {
    implicit val system = ActorSystem("FastSyncBlockBodiesRequestHandlerSpec_System")

    val time = new VirtualTime

    val peer = TestProbe()

    val requestedHashes = Seq(ByteString("1"), ByteString("2"))

    val dataSource = EphemDataSource()
    val blockBodiesStorage = new BlockBodiesStorage(dataSource)

    val parent = TestProbe()

    val fastSyncBlockBodiesRequestHandler =
      parent.childActorOf(FastSyncBlockBodiesRequestHandler.props(
        peer.ref,
        requestedHashes,
        blockBodiesStorage)(time.scheduler))
  }

}
