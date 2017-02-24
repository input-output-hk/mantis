package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import akka.agent.Agent
import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.{BlockHeadersStorage, TotalDifficultyStorage}
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.crypto
import io.iohk.ethereum.utils.{BlockchainStatus, NodeStatus, ServerStatus}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import org.scalatest.{FlatSpec, Matchers}

class FastSyncBlockHeadersRequestHandlerSpec extends FlatSpec with Matchers {

  "FastSyncBlockHeadersRequestHandler" should "handle successful response (and enqueue remaining receipts)" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(block), maxHeaders, 0, false)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))

    val responseHeaders = Seq(BlockHeader(ByteString(""), ByteString(""), ByteString(""),
      ByteString(""), ByteString(""), ByteString(""),
      ByteString(""), 0, 0, 0, 0, 0, ByteString(""), ByteString(""), ByteString("")))

    peer.reply(PeerActor.MessageReceived(BlockHeaders(responseHeaders)))

    parent.expectMsgAllOf(
      FastSyncController.EnqueueBlockBodies(Seq(responseHeaders.head.hash)),
      FastSyncController.EnqueueReceipts(Seq(responseHeaders.head.hash)))
    parent.expectMsg(FastSyncRequestHandler.Done)

    blockHeadersStorage.get(responseHeaders.head.hash) shouldBe Some(responseHeaders.head)

    nodeStatusHolder().blockchainStatus.bestNumber shouldBe responseHeaders.last.number

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

  trait TestSetup {
    implicit val system = ActorSystem("FastSyncBlockHeadersRequestHandlerSpec_System")

    val time = new VirtualTime

    val peer = TestProbe()

    val requestedHashes = Seq(ByteString("1"), ByteString("2"))

    val dataSource = EphemDataSource()
    val blockHeadersStorage = new BlockHeadersStorage(dataSource)
    val totalDifficultyStorage = new TotalDifficultyStorage(dataSource){
      override def get(blockHash: ByteString): Option[BigInt] = Some(BigInt(0))
    }

    val parent = TestProbe()

    val block = BigInt(1)
    val maxHeaders = 10

    val nodeKey = crypto.generateKeyPair()

    val nodeStatus = NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening,
      blockchainStatus = BlockchainStatus(0, ByteString("123"), 0))

    val nodeStatusHolder = Agent(nodeStatus)

    val fastSyncBlockHeadersRequestHandler =
      parent.childActorOf(FastSyncBlockHeadersRequestHandler.props(
        peer.ref,
        block,
        maxHeaders,
        nodeStatusHolder,
        blockHeadersStorage,
        totalDifficultyStorage)(time.scheduler))
  }

}
