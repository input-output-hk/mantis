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
<<<<<<< HEAD
import io.iohk.ethereum.network.p2p.messages.PV62.{GetBlockHeaders, BlockHeaders}
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
=======
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{Receipts, GetReceipts, GetNodeData, NodeData}
>>>>>>> origin/feature/fastSyncControllerWithChild
import io.iohk.ethereum.utils.{BlockchainStatus, ServerStatus, NodeStatus}
import org.scalatest.{Matchers, FlatSpec}
import org.spongycastle.util.encoders.Hex

class FastSyncControllerSpec extends FlatSpec with Matchers {

  "FastSyncController" should "download target block and request state nodes" in new TestSetup {
    val peer1 = TestProbe()(system)
    val peer2 = TestProbe()(system)

    time.advance(1.seconds)

    peerManager.expectMsg(GetPeers)
    peerManager.reply(PeersResponse(Seq(
      Peer(new InetSocketAddress("127.0.0.1", 0), peer1.ref),
      Peer(new InetSocketAddress("127.0.0.1", 0), peer2.ref))))

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    peer1.expectMsg(PeerActor.GetStatus)
    peer1.reply(PeerActor.StatusResponse(PeerActor.Status.Handshaked(peer1Status)))

    val peer2Status= Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))
    peer2.expectMsg(PeerActor.GetStatus)
    peer2.reply(PeerActor.StatusResponse(PeerActor.Status.Handshaked(peer2Status)))

    fastSyncController ! FastSyncController.StartFastSync

    peer1.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer1.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(ByteString("peer1_bestHash")), 1, 0, reverse = false)))
    peer1.reply(PeerActor.MessageReceived(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000)))))
    peer1.expectMsg(PeerActor.Unsubscribe)

    peer2.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer2.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(ByteString("peer2_bestHash")), 1, 0, reverse = false)))
    peer2.reply(PeerActor.MessageReceived(BlockHeaders(Seq(baseBlockHeader.copy(number = 400000)))))
    peer2.expectMsg(PeerActor.Unsubscribe)

    val expectedTargetBlock = 399500

    peer1.expectNoMsg()

    val targetBlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)
    peer2.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer2.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedTargetBlock), 1, 0, reverse = false)))
    peer2.reply(PeerActor.MessageReceived(BlockHeaders(Seq(targetBlockHeader))))

    nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
      targetBlockHeader.difficulty,
      targetBlockHeader.hash,
      targetBlockHeader.number)))

    peer2.expectMsg(PeerActor.Unsubscribe)

    peer2.expectMsg(PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peer2.expectMsg(PeerActor.Subscribe(Set(NodeData.code)))
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

    val peerStatus= Status(1, 1, 1, ByteString("best hash"), ByteString("genesis hash"))
    peer1.expectMsg(PeerActor.GetStatus)
    peer1.reply(PeerActor.StatusResponse(PeerActor.Status.Handshaked(peerStatus)))

    fastSyncController ! FastSyncController.StartFastSync

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

    val peer2Status= Status(1, 1, 1, ByteString("best hash"), ByteString("genesis hash"))
    peer2.expectMsg(PeerActor.GetStatus)
    peer2.reply(PeerActor.StatusResponse(PeerActor.Status.Handshaked(peer2Status)))

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

    val peerStatus = Status(1, 1, 1, ByteString("best hash"), ByteString("genesis hash"))
    peer.expectMsg(PeerActor.GetStatus)
    peer.reply(PeerActor.StatusResponse(PeerActor.Status.Handshaked(peerStatus)))

    fastSyncController ! FastSyncController.StartFastSync

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
