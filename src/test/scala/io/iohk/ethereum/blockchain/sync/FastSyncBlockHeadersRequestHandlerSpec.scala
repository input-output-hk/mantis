package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import org.scalatest.{FlatSpec, Matchers}

class FastSyncBlockHeadersRequestHandlerSpec extends FlatSpec with Matchers {

  "FastSyncBlockHeadersRequestHandler" should "handle successful response (and enqueue remaining receipts)" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(block), maxHeaders, 0, reverse = false)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))

    val responseHeaders = Seq(BlockHeader(testGenesisHash, ByteString(""), ByteString(""),
      ByteString(""), ByteString(""), ByteString(""),
      ByteString(""), 0, block, 0, 0, 0, ByteString(""), ByteString(""), ByteString("")))

    peer.reply(PeerActor.MessageReceived(BlockHeaders(responseHeaders)))

    parent.expectMsgAllOf(
      FastSync.EnqueueBlockBodies(Seq(responseHeaders.head.hash)),
      FastSync.EnqueueReceipts(Seq(responseHeaders.head.hash)),
      FastSync.UpdateBestBlockHeaderNumber(responseHeaders.last.number))

    parent.expectMsg(FastSyncRequestHandler.Done)

    blockchain.getBlockHeaderByHash(responseHeaders.head.hash) shouldBe Some(responseHeaders.head)

    peer.expectMsg(PeerActor.Unsubscribe)
  }

  it should "handle timeout" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(block), maxHeaders, 0, false)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))

    time.advance(10.seconds)

    parent.expectMsg(BlacklistSupport.BlacklistPeer(peer.ref))
    parent.expectMsg(FastSyncRequestHandler.Done)

    peer.expectMsg(PeerActor.Unsubscribe)
  }

  trait TestSetup extends EphemBlockchainTestSetup {
    implicit val system = ActorSystem("FastSyncBlockHeadersRequestHandlerSpec_System")

    val testGenesisHash = ByteString("123")
    blockchain.save(testGenesisHash, 123)

    val time = new VirtualTime

    val peer = TestProbe()

    val requestedHashes = Seq(ByteString("1"), ByteString("2"))

    val parent = TestProbe()

    val block = BigInt(1)
    val maxHeaders = 1

    val fastSyncBlockHeadersRequestHandler =
      parent.childActorOf(FastSyncBlockHeadersRequestHandler.props(
        peer.ref,
        block,
        maxHeaders,
        blockchain)(time.scheduler))
  }

}
