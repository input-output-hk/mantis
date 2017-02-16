package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import akka.actor.{ActorSystem, Props}
import akka.agent.Agent
import akka.testkit.{TestProbe, TestActorRef}
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.crypto
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.network.PeerManagerActor.{Peer, PeersResponse, GetPeers}
import io.iohk.ethereum.network.p2p.messages.PV62.{GetBlockHeaders, BlockHeaders}
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.utils.{BlockchainStatus, ServerStatus, NodeStatus}
import org.scalatest.{Matchers, FlatSpec}

class FastSyncControllerSpec extends FlatSpec with Matchers {

  "FastSyncController" should "download target block and request state nodes" in new TestSetup {
    val peer = TestProbe()(system)

    val targetBlockHeader = baseBlockHeader.copy(number = 100)

    nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
      targetBlockHeader.difficulty,
      targetBlockHeader.hash,
      targetBlockHeader.number)))

    time.advance(1.seconds)

    peerManager.expectMsg(GetPeers)
    peerManager.reply(PeersResponse(Seq(Peer(new InetSocketAddress("127.0.0.1", 0), peer.ref))))

    peer.expectMsg(PeerActor.GetStatus)
    peer.reply(PeerActor.StatusResponse(PeerActor.Status.Handshaked))

    fastSyncController ! FastSyncController.StartFastSync(targetBlockHeader.hash)

    peer.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(targetBlockHeader.hash), 1, 0, reverse = false)))

    peer.reply(PeerActor.MessageReceived(BlockHeaders(Seq(targetBlockHeader))))

    peer.expectMsg(PeerActor.Unsubscribe)

    peer.expectMsg(PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peer.expectMsg(PeerActor.Subscribe(Set(NodeData.code)))
  }

  it should "download target block and request state and blocks from two peers" in new TestSetup {
    val peer1 = TestProbe()(system)
    val peer2 = TestProbe()(system)

    val targetBlockHeader = baseBlockHeader.copy(number = 100)

    nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
      10,
      ByteString("bestHash"),
      5)))

    time.advance(1.seconds)

    peerManager.expectMsg(GetPeers)
    peerManager.reply(PeersResponse(Seq(Peer(new InetSocketAddress("127.0.0.1", 0), peer1.ref))))

    peer1.expectMsg(PeerActor.GetStatus)
    peer1.reply(PeerActor.StatusResponse(PeerActor.Status.Handshaked))

    fastSyncController ! FastSyncController.StartFastSync(targetBlockHeader.hash)

    peer1.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer1.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(targetBlockHeader.hash), 1, 0, reverse = false)))

    peer1.reply(PeerActor.MessageReceived(BlockHeaders(Seq(targetBlockHeader))))
    peer1.expectMsg(PeerActor.Unsubscribe)

    peer1.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(6), 10, 0, false)))
    peer1.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))

    time.advance(2.seconds)

    peerManager.expectMsg(GetPeers)
    peerManager.reply(PeersResponse(Seq(
      Peer(new InetSocketAddress("127.0.0.1", 0), peer1.ref),
      Peer(new InetSocketAddress("127.0.0.1", 0), peer2.ref))))

    peer2.expectMsg(PeerActor.GetStatus)
    peer2.reply(PeerActor.StatusResponse(PeerActor.Status.Handshaked))

    time.advance(1.seconds)

    peer2.expectMsg(PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peer2.expectMsg(PeerActor.Subscribe(Set(NodeData.code)))
  }

  it should "not use (blacklist) a peer that fails to respond within time limit" in new TestSetup {
    val peer = TestProbe()(system)

    val targetBlockHeader = baseBlockHeader.copy(number = 100)

    nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
      targetBlockHeader.difficulty,
      targetBlockHeader.hash,
      targetBlockHeader.number)))

    time.advance(1.seconds)

    peerManager.expectMsg(GetPeers)
    peerManager.reply(PeersResponse(Seq(Peer(new InetSocketAddress("127.0.0.1", 0), peer.ref))))

    peer.expectMsg(PeerActor.GetStatus)
    peer.reply(PeerActor.StatusResponse(PeerActor.Status.Handshaked))

    fastSyncController ! FastSyncController.StartFastSync(targetBlockHeader.hash)

    peer.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(targetBlockHeader.hash), 1, 0, reverse = false)))

    peer.reply(PeerActor.MessageReceived(BlockHeaders(Seq(targetBlockHeader))))

    peer.expectMsg(PeerActor.Unsubscribe)

    peer.expectMsg(PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peer.expectMsg(PeerActor.Subscribe(Set(NodeData.code)))

    // response timeout
    time.advance(2.seconds)
    peer.expectMsg(PeerActor.Unsubscribe)
    peer.expectNoMsg()

    // wait for blacklist timeout
    time.advance(6.seconds)
    peer.expectNoMsg()

    // wait for next sync retry
    time.advance(3.seconds)

    // peer should not be blacklisted anymore
    peer.expectMsg(PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peer.expectMsg(PeerActor.Subscribe(Set(NodeData.code)))
  }

  trait TestSetup {
    implicit val system = ActorSystem("FastSyncControllerSpec_System")

    val nodeKey = crypto.generateKeyPair()

    val nodeStatus = NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening,
      blockchainStatus = BlockchainStatus(0, ByteString("changeme"), 0))

    val nodeStatusHolder = Agent(nodeStatus)

    val time = new VirtualTime
    val peerManager = TestProbe()

    val dataSource = EphemDataSource()

    val fastSyncController = TestActorRef(Props(new FastSyncController(peerManager.ref, nodeStatusHolder,
      new MptNodeStorage(dataSource),
      new BlockHeadersStorage(dataSource),
      new BlockBodiesStorage(dataSource),
      new ReceiptStorage(dataSource),
      new EvmCodeStorage(dataSource),
      externalSchedulerOpt = Some(time.scheduler))))

    val baseBlockHeader = BlockHeader(
      parentHash = ByteString("unused"),
      ommersHash = ByteString("unused"),
      beneficiary = ByteString("unused"),
      stateRoot = ByteString("unused"),
      transactionsRoot = ByteString("unused"),
      receiptsRoot = ByteString("unused"),
      logsBloom = ByteString("unused"),
      difficulty = 0,
      number = 0,
      gasLimit = 0,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = ByteString("unused"),
      mixHash = ByteString("unused"),
      nonce = ByteString("unused"))
  }

}
