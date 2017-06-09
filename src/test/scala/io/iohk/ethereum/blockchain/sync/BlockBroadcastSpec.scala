package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.network.{Peer, PeerActor}
import io.iohk.ethereum.network.PeersInfoHolderActor.PeerInfo
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.p2p.messages.Versions
import org.scalatest.{FlatSpec, Matchers}

class BlockBroadcastSpec extends FlatSpec with Matchers  {

  it should "send a new block only when it is not known by the peer (known by comparing total difficulties)" in new TestSetup {
    //given
    //Block that shouldn't be sent as it's number and total difficulty is lower than known by peer
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber - 2)
    val firstBlock = NewBlock(Block(firstHeader, BlockBody(Nil, Nil)), initialPeerInfo.totalDifficulty - 2)

    //Block that should be sent as it's total difficulty is higher than known by peer
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber - 3)
    val secondBlock = NewBlock(Block(secondHeader, BlockBody(Nil, Nil)), initialPeerInfo.totalDifficulty + 2)

    //when
    blockBroadcast.broadcastNewBlocks(Seq(firstBlock, secondBlock), Map(peer -> initialPeerInfo))

    //then
    peerProbe.expectMsg(PeerActor.SendMessage(secondBlock))
    peerProbe.expectNoMsg()
  }

  it should "send a new block only when it is not known by the peer (known by comparing max block number)" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 4)
    val firstBlock = NewBlock(Block(firstHeader, BlockBody(Nil, Nil)), initialPeerInfo.totalDifficulty - 2)

    //Block should already be known by the peer due to max block known
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber - 2)
    val secondBlock = NewBlock(Block(secondHeader, BlockBody(Nil, Nil)), initialPeerInfo.totalDifficulty - 2)

    //when
    blockBroadcast.broadcastNewBlocks(Seq(firstBlock, secondBlock), Map(peer -> initialPeerInfo))

    //then
    peerProbe.expectMsg(PeerActor.SendMessage(firstBlock))
    peerProbe.expectNoMsg()
  }

  trait TestSetup {
    implicit val system = ActorSystem("BlockBroadcastSpec_System")

    val blockBroadcast = new BlockBroadcast {}

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
    val peer = Peer(new InetSocketAddress("127.0.0.1", 0), peerProbe.ref)
  }

}
