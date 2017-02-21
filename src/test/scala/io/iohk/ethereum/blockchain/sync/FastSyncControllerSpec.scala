package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import akka.actor.{Terminated, ActorSystem, Props}
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
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{Receipts, GetReceipts, GetNodeData, NodeData}
import io.iohk.ethereum.utils.{BlockchainStatus, ServerStatus, NodeStatus}
import org.scalatest.{Matchers, FlatSpec}
import org.spongycastle.util.encoders.Hex

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

  it should "stop syncing when chain and mpt is downloaded" in new TestSetup {
    val peer = TestProbe()(system)

    val targetBlockHeader = baseBlockHeader.copy(
      number = 6,
      stateRoot = ByteString(Hex.decode("deae1dfad5ec8dcef15915811e1f044d2543674fd648f94345231da9fc2646cc")))

    nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
      10,
      ByteString("bestHash"),
      5)))

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

    peer.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(6), 10, 0, false)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer.reply(PeerActor.MessageReceived(BlockHeaders(Seq(targetBlockHeader))))
    peer.expectMsg(PeerActor.Unsubscribe)

    peer.expectMsg(PeerActor.SendMessage(GetReceipts(Seq(targetBlockHeader.hash))))
    peer.expectMsg(PeerActor.Subscribe(Set(Receipts.code)))
    peer.reply(PeerActor.MessageReceived(Receipts(Seq(Seq()))))
    peer.expectMsg(PeerActor.Unsubscribe)

    peer.expectMsg(PeerActor.SendMessage(GetBlockBodies(Seq(targetBlockHeader.hash))))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockBodies.code)))
    peer.reply(PeerActor.MessageReceived(BlockBodies(Seq(BlockBody(Nil, Nil)))))
    peer.expectMsg(PeerActor.Unsubscribe)

    val stateMptLeafWithAccount =
      ByteString(Hex.decode("f86d9e328415c225a782bb339b22acad1c739e42277bc7ef34de3623114997ce78b84cf84a0186cb7d8738d800a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

    val watcher = TestProbe()
    watcher.watch(fastSyncController)

    peer.expectMsg(PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peer.expectMsg(PeerActor.Subscribe(Set(NodeData.code)))
    peer.reply(PeerActor.MessageReceived(NodeData(Seq(stateMptLeafWithAccount))))
    peer.expectMsg(PeerActor.Unsubscribe)

    watcher.expectMsgPF(2.seconds) { case Terminated(`fastSyncController`) => () }
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
