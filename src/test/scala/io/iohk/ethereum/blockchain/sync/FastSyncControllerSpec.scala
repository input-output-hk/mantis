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

  "FastSyncController" should "download target block and request state nodes" in {
    implicit val system = ActorSystem("FastSyncControllerSpec_System")

    val time = new VirtualTime

    val peerManager = TestProbe()
    val peer = TestProbe()

    val targetBlockHeader = BlockHeader(
      parentHash = ByteString("unused"),
      ommersHash = ByteString("unused"),
      beneficiary = ByteString("unused"),
      stateRoot = ByteString("unused"),
      transactionsRoot = ByteString("unused"),
      receiptsRoot = ByteString("unused"),
      logsBloom = ByteString("unused"),
      difficulty = 100,
      number = 100,
      gasLimit = 100,
      gasUsed = 100,
      unixTimestamp = 100,
      extraData = ByteString("unused"),
      mixHash = ByteString("unused"),
      nonce = ByteString("unused"))

    val nodeKey = crypto.generateKeyPair()

    val nodeStatus = NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening,
      blockchainStatus = BlockchainStatus(0, targetBlockHeader.hash, targetBlockHeader.number))

    val nodeStatusHolder = Agent(nodeStatus)

    val dataSource = EphemDataSource()

    val fastSyncController = TestActorRef(Props(new FastSyncController(peerManager.ref, nodeStatusHolder,
      new MptNodeStorage(dataSource),
      new BlockHeadersStorage(dataSource),
      new BlockBodiesStorage(dataSource),
      new ReceiptStorage(dataSource),
      new EvmCodeStorage(dataSource),
      externalSchedulerOpt = Some(time.scheduler))))

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

}
