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
import org.spongycastle.crypto.AsymmetricCipherKeyPair

class FastSyncBlockHeadersRequestHandlerSpec extends FlatSpec with Matchers {

  "FastSyncBlockHeadersRequestHandler" should "handle successful response (and enqueue remaining receipts)" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(block), maxHeaders, 0, reverse = false)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))

    val responseHeaders = Seq(BlockHeader(Config.Blockchain.genesisHash, ByteString(""), ByteString(""),
      ByteString(""), ByteString(""), ByteString(""),
      ByteString(""), 0, block, 0, 0, 0, ByteString(""), ByteString(""), ByteString("")))

    peer.reply(PeerActor.MessageReceived(BlockHeaders(responseHeaders)))

    parent.expectMsgAllOf(
      SyncController.BlockHeadersReceived(peer.ref, responseHeaders),
      SyncController.EnqueueBlockBodies(Seq(responseHeaders.head.hash)),
      SyncController.EnqueueReceipts(Seq(responseHeaders.head.hash)))

    parent.expectMsg(FastSyncRequestHandler.Done)

    peer.expectMsg(PeerActor.Unsubscribe)
  }

  it should "handle block header resolution request" in new TestSetup {
    val request = GetBlockHeaders(Left(block), maxHeaders, skip = 0, reverse = true)
    val resolverPeer = TestProbe()
    val resolver: ActorRef = {
      parent.childActorOf(FastSyncBlockHeadersRequestHandler.props(
        resolverPeer.ref,
        request,
        resolveBranches = true)(time.scheduler))
    }

    resolverPeer.expectMsg(PeerActor.SendMessage(request))
    resolverPeer.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))

    val responseHeaders = Seq(BlockHeader(Config.Blockchain.genesisHash, ByteString(""), ByteString(""),
      ByteString(""), ByteString(""), ByteString(""),
      ByteString(""), 0, block, 0, 0, 0, ByteString(""), ByteString(""), ByteString("")))

    resolverPeer.reply(PeerActor.MessageReceived(BlockHeaders(responseHeaders)))

    parent.expectMsg(SyncController.BlockHeadersToResolve(resolverPeer.ref, responseHeaders))

    parent.expectMsg(FastSyncRequestHandler.Done)

    resolverPeer.expectMsg(PeerActor.Unsubscribe)
  }

  it should "handle timeout" in new TestSetup {
    peer.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(block), maxHeaders, 0, reverse = false)))
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

    val nodeKey: AsymmetricCipherKeyPair = crypto.generateKeyPair()

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
