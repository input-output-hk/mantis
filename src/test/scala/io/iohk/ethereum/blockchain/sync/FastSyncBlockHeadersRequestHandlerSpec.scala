package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.agent.Agent
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.crypto
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import org.scalatest.{FlatSpec, Matchers}

class FastSyncBlockHeadersRequestHandlerSpec extends FlatSpec with Matchers {

  "FastSyncBlockHeadersRequestHandler" should "handle successful response (and enqueue remaining receipts)" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(block), maxHeaders, 0, reverse = false)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))

    val responseHeaders = Seq(BlockHeader(Config.Blockchain.genesisHash, ByteString(""), ByteString(""),
      ByteString(""), ByteString(""), ByteString(""),
      ByteString(""), 0, block, 0, 0, 0, ByteString(""), ByteString(""), ByteString("")))

    peer.reply(PeerActor.MessageReceived(BlockHeaders(responseHeaders)))

    parent.expectMsgAllOf(
      SyncController.EnqueueBlockBodies(Seq(responseHeaders.head.hash)),
      SyncController.EnqueueReceipts(Seq(responseHeaders.head.hash)),
      SyncController.UpdateBestBlockHeaderNumber(responseHeaders.last.number))

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

    val time = new VirtualTime

    val peer = TestProbe()

    val requestedHashes = Seq(ByteString("1"), ByteString("2"))

    val parent = TestProbe()

    val block = BigInt(1)
    val maxHeaders = 1

    val nodeKey = crypto.generateKeyPair()

    val nodeStatus = NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening)

    val nodeStatusHolder = Agent(nodeStatus)

    val fastSyncBlockHeadersRequestHandler: ActorRef = {
      val request = GetBlockHeaders(Left(block), maxHeaders, skip = 0, reverse = false)
      parent.childActorOf(FastSyncBlockHeadersRequestHandler.props(
        peer.ref,
        request,
        resolveBranches = false)(time.scheduler))
    }
  }

}
