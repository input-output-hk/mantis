package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, NewBlockHashes}
import io.iohk.ethereum.network.p2p.messages.{PV62, Versions}
import org.scalatest.{FlatSpec, Matchers}

class BlockBroadcastSpec extends FlatSpec with Matchers  {

  it should "send a new block when it is not known by the peer (known by comparing total difficulties)" in new TestSetup {
    //given
    //Block that should be sent as it's total difficulty is higher than known by peer
    val blockHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber - 3)
    val newBlockNewHashes = NewBlockHashes(Seq(PV62.BlockHash(blockHeader.hash, blockHeader.number)))
    val newBlock = NewBlock(Block(blockHeader, BlockBody(Nil, Nil)), initialPeerInfo.totalDifficulty + 2)

    //when
    blockBroadcast.broadcastBlock(newBlock, Map(peer -> initialPeerInfo))

    //then
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(newBlock, peer.id))
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(newBlockNewHashes, peer.id))
    etcPeerManagerProbe.expectNoMsg()
  }

  it should "not send a new block when it is not known by the peer (known by comparing total difficulties)" in new TestSetup {
    //given
    //Block that shouldn't be sent as it's number and total difficulty is lower than known by peer
    val blockHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber - 2)
    val newBlock = NewBlock(Block(blockHeader, BlockBody(Nil, Nil)), initialPeerInfo.totalDifficulty - 2)

    //when
    blockBroadcast.broadcastBlock(newBlock, Map(peer -> initialPeerInfo))

    //then
    etcPeerManagerProbe.expectNoMsg()
  }

  it should "send a new block when it is not known by the peer (known by comparing max block number)" in new TestSetup {
    //given
    val blockHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 4)
    val newBlockNewHashes = NewBlockHashes(Seq(PV62.BlockHash(blockHeader.hash, blockHeader.number)))
    val newBlock = NewBlock(Block(blockHeader, BlockBody(Nil, Nil)), initialPeerInfo.totalDifficulty - 2)

    //when
    blockBroadcast.broadcastBlock(newBlock, Map(peer -> initialPeerInfo))

    //then
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(newBlock, peer.id))
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(newBlockNewHashes, peer.id))
    etcPeerManagerProbe.expectNoMsg()
  }

  it should "not send a new block only when it is not known by the peer (known by comparing max block number)" in new TestSetup {
    //given
    //Block should already be known by the peer due to max block known
    val blockHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber - 2)
    val newBlock = NewBlock(Block(blockHeader, BlockBody(Nil, Nil)), initialPeerInfo.totalDifficulty - 2)

    //when
    blockBroadcast.broadcastBlock(newBlock, Map(peer -> initialPeerInfo))

    //then
    etcPeerManagerProbe.expectNoMsg()
  }

  it should "send block hashes to all peers while the blocks only to sqrt of them" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 4)
    val firstBlockNewHashes = NewBlockHashes(Seq(PV62.BlockHash(firstHeader.hash, firstHeader.number)))
    val firstBlock = NewBlock(Block(firstHeader, BlockBody(Nil, Nil)), initialPeerInfo.totalDifficulty - 2)

    val peer2Probe = TestProbe()
    val peer2 = Peer(new InetSocketAddress("127.0.0.1", 0), peer2Probe.ref, false)
    val peer3Probe = TestProbe()
    val peer3 = Peer(new InetSocketAddress("127.0.0.1", 0), peer3Probe.ref, false)
    val peer4Probe = TestProbe()
    val peer4 = Peer(new InetSocketAddress("127.0.0.1", 0), peer4Probe.ref, false)

    //when
    val peers = Seq(peer, peer2, peer3, peer4)
    val peersIds = peers.map(_.id)
    val peersWithInfo = peers.map(_ -> initialPeerInfo).toMap
    blockBroadcast.broadcastBlock(firstBlock, peersWithInfo)

    //then
    //Only two peers receive the complete block
    etcPeerManagerProbe.expectMsgPF(){ case EtcPeerManagerActor.SendMessage(b, p) if b.underlyingMsg==firstBlock && peersIds.contains(p) => () }
    etcPeerManagerProbe.expectMsgPF(){ case EtcPeerManagerActor.SendMessage(b, p) if b.underlyingMsg==firstBlock && peersIds.contains(p) => () }

    //All the peers should receive the block hashes
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(firstBlockNewHashes, peer.id))
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(firstBlockNewHashes, peer2.id))
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(firstBlockNewHashes, peer3.id))
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(firstBlockNewHashes, peer4.id))
    etcPeerManagerProbe.expectNoMsg()
  }

  trait TestSetup {
    implicit val system = ActorSystem("BlockBroadcastSpec_System")

    val etcPeerManagerProbe = TestProbe()

    val blockBroadcast = new BlockBroadcast {
      override val etcPeerManager: ActorRef = etcPeerManagerProbe.ref
    }

    val baseBlockHeader = Fixtures.Blocks.Block3125369.header

    val peerStatus = Status(
      protocolVersion = Versions.PV63,
      networkId = 1,
      totalDifficulty = BigInt(10000),
      bestHash = Fixtures.Blocks.Block3125369.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val initialPeerInfo = PeerInfo(
      remoteStatus = peerStatus,
      totalDifficulty = peerStatus.totalDifficulty,
      forkAccepted = false,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number
    )

    val peerProbe = TestProbe()
    val peer = Peer(new InetSocketAddress("127.0.0.1", 0), peerProbe.ref, false)
  }

}
