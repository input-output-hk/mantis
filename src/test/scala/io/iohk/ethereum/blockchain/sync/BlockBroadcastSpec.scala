package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestProbe

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.WithActorSystemShutDown
import io.iohk.ethereum.blockchain.sync.PeerListSupportNg.PeerWithInfo
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcast
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcast.BlockToBroadcast
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.network.EtcPeerManagerActor
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.RemoteStatus
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.p2p.messages.ETC64.NewBlock
import io.iohk.ethereum.network.p2p.messages.ETH62
import io.iohk.ethereum.network.p2p.messages.ETH62.NewBlockHashes

class BlockBroadcastSpec
    extends TestKit(ActorSystem("BlockBroadcastSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers {

  it should "send a new block when it is not known by the peer (known by comparing chain weights)" in new TestSetup {
    //given
    //Block that should be sent as it's total difficulty is higher than known by peer
    val blockHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber - 3)
    val newBlockNewHashes = NewBlockHashes(Seq(ETH62.BlockHash(blockHeader.hash, blockHeader.number)))
    val newBlock =
      NewBlock(Block(blockHeader, BlockBody(Nil, Nil)), initialPeerInfo.chainWeight.increaseTotalDifficulty(2))

    //when
    blockBroadcast.broadcastBlock(
      BlockToBroadcast(newBlock.block, newBlock.chainWeight),
      Map(peer.id -> PeerWithInfo(peer, initialPeerInfo))
    )

    //then
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(newBlock, peer.id))
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(newBlockNewHashes, peer.id))
    etcPeerManagerProbe.expectNoMessage()
  }

  it should "send a new block when it is not known by the peer (known by comparing chain weights) (ETH63)" in new TestSetup {
    //given
    //Block that should be sent as it's total difficulty is higher than known by peer
    val blockHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber - 3)
    val newBlockNewHashes = NewBlockHashes(Seq(ETH62.BlockHash(blockHeader.hash, blockHeader.number)))
    val peerInfo = initialPeerInfo
      .copy(remoteStatus = peerStatus.copy(capability = Capability.ETH63))
      .withChainWeight(ChainWeight.totalDifficultyOnly(initialPeerInfo.chainWeight.totalDifficulty))
    val newBlock =
      BaseETH6XMessages.NewBlock(Block(blockHeader, BlockBody(Nil, Nil)), peerInfo.chainWeight.totalDifficulty + 2)

    //when
    blockBroadcast.broadcastBlock(
      BlockToBroadcast(newBlock.block, ChainWeight.totalDifficultyOnly(newBlock.totalDifficulty)),
      Map(peer.id -> PeerWithInfo(peer, peerInfo))
    )

    //then
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(newBlock, peer.id))
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(newBlockNewHashes, peer.id))
    etcPeerManagerProbe.expectNoMessage()
  }

  it should "not send a new block when it is known by the peer (known by comparing total difficulties)" in new TestSetup {
    //given
    //Block that shouldn't be sent as it's number and total difficulty is lower than known by peer
    val blockHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber - 2)
    val newBlock =
      NewBlock(Block(blockHeader, BlockBody(Nil, Nil)), initialPeerInfo.chainWeight.increaseTotalDifficulty(-2))

    //when
    blockBroadcast.broadcastBlock(
      BlockToBroadcast(newBlock.block, newBlock.chainWeight),
      Map(peer.id -> PeerWithInfo(peer, initialPeerInfo))
    )

    //then
    etcPeerManagerProbe.expectNoMessage()
  }

  it should "send a new block when it is not known by the peer (known by comparing max block number)" in new TestSetup {
    //given
    val blockHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 4)
    val newBlockNewHashes = NewBlockHashes(Seq(ETH62.BlockHash(blockHeader.hash, blockHeader.number)))
    val newBlock =
      NewBlock(Block(blockHeader, BlockBody(Nil, Nil)), initialPeerInfo.chainWeight.increaseTotalDifficulty(-2))

    //when
    blockBroadcast.broadcastBlock(
      BlockToBroadcast(newBlock.block, newBlock.chainWeight),
      Map(peer.id -> PeerWithInfo(peer, initialPeerInfo))
    )

    //then
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(newBlock, peer.id))
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(newBlockNewHashes, peer.id))
    etcPeerManagerProbe.expectNoMessage()
  }

  it should "not send a new block only when it is known by the peer (known by comparing max block number)" in new TestSetup {
    //given
    //Block should already be known by the peer due to max block known
    val blockHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber - 2)
    val newBlock =
      NewBlock(Block(blockHeader, BlockBody(Nil, Nil)), initialPeerInfo.chainWeight.increaseTotalDifficulty(-2))

    //when
    blockBroadcast.broadcastBlock(
      BlockToBroadcast(newBlock.block, newBlock.chainWeight),
      Map(peer.id -> PeerWithInfo(peer, initialPeerInfo))
    )

    //then
    etcPeerManagerProbe.expectNoMessage()
  }

  it should "send block hashes to all peers while the blocks only to sqrt of them" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 4)
    val firstBlockNewHashes = NewBlockHashes(Seq(ETH62.BlockHash(firstHeader.hash, firstHeader.number)))
    val firstBlock =
      NewBlock(Block(firstHeader, BlockBody(Nil, Nil)), initialPeerInfo.chainWeight.increaseTotalDifficulty(-2))

    val peer2Probe = TestProbe()
    val peer2 = Peer(PeerId("peer2"), new InetSocketAddress("127.0.0.1", 0), peer2Probe.ref, false)
    val peer3Probe = TestProbe()
    val peer3 = Peer(PeerId("peer3"), new InetSocketAddress("127.0.0.1", 0), peer3Probe.ref, false)
    val peer4Probe = TestProbe()
    val peer4 = Peer(PeerId("peer4"), new InetSocketAddress("127.0.0.1", 0), peer4Probe.ref, false)

    //when
    val peers = Seq(peer, peer2, peer3, peer4)
    val peersIds = peers.map(_.id)
    val peersWithInfo = peers.map(peer => peer.id -> PeerWithInfo(peer, initialPeerInfo)).toMap
    blockBroadcast.broadcastBlock(BlockToBroadcast(firstBlock.block, firstBlock.chainWeight), peersWithInfo)

    //then
    //Only two peers receive the complete block
    etcPeerManagerProbe.expectMsgPF() {
      case EtcPeerManagerActor.SendMessage(b, p) if b.underlyingMsg == firstBlock && peersIds.contains(p) => ()
    }
    etcPeerManagerProbe.expectMsgPF() {
      case EtcPeerManagerActor.SendMessage(b, p) if b.underlyingMsg == firstBlock && peersIds.contains(p) => ()
    }

    //All the peers should receive the block hashes
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(firstBlockNewHashes, peer.id))
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(firstBlockNewHashes, peer2.id))
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(firstBlockNewHashes, peer3.id))
    etcPeerManagerProbe.expectMsg(EtcPeerManagerActor.SendMessage(firstBlockNewHashes, peer4.id))
    etcPeerManagerProbe.expectNoMessage()
  }

  class TestSetup(implicit system: ActorSystem) {
    val etcPeerManagerProbe: TestProbe = TestProbe()

    val blockBroadcast = new BlockBroadcast(etcPeerManagerProbe.ref)

    val baseBlockHeader = Fixtures.Blocks.Block3125369.header

    val peerStatus: RemoteStatus = RemoteStatus(
      capability = Capability.ETC64,
      networkId = 1,
      chainWeight = ChainWeight(10, 10000),
      bestHash = Fixtures.Blocks.Block3125369.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val initialPeerInfo: PeerInfo = PeerInfo(
      remoteStatus = peerStatus,
      chainWeight = peerStatus.chainWeight,
      forkAccepted = false,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number,
      bestBlockHash = peerStatus.bestHash
    )

    val peerProbe: TestProbe = TestProbe()
    val peer: Peer = Peer(PeerId("peer"), new InetSocketAddress("127.0.0.1", 0), peerProbe.ref, false)
  }
}
