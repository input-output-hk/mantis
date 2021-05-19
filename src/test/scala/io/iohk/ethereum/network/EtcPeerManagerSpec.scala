package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.Fixtures.Blocks.{DaoForkBlock, Genesis}
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.{Block, BlockBody, BlockHeader, ChainWeight}
import io.iohk.ethereum.network.EtcPeerManagerActor._
import io.iohk.ethereum.network.PeerActor.DisconnectPeer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{MessageFromPeer, PeerDisconnected, PeerHandshakeSuccessful}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier._
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.network.p2p.messages.{Codes, PV164, ProtocolVersions}
import io.iohk.ethereum.utils.Config
import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EtcPeerManagerSpec extends AnyFlatSpec with Matchers {

  it should "start with the peers initial info as provided" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1Info)
    setupNewPeer(peer2, peer2Probe, peer2Info)

    //PeersInfoRequest should work properly
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(PeerInfoResponse(Some(peer1Info)))
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer2.id))
    requestSender.expectMsg(PeerInfoResponse(Some(peer2Info)))
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer3.id))
    requestSender.expectMsg(PeerInfoResponse(None))

    //GetHandshakedPeers should work properly
    requestSender.send(peersInfoHolder, GetHandshakedPeers)
    requestSender.expectMsg(HandshakedPeers(Map(peer1 -> peer1Info, peer2 -> peer2Info)))
  }

  it should "update max peer when receiving new block PV63" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1Info)

    //given
    val newBlockWeight = ChainWeight.totalDifficultyOnly(300)
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = peer1Info.maxBlockNumber + 4)
    val firstBlock = NewBlock(Block(firstHeader, BlockBody(Nil, Nil)), newBlockWeight.totalDifficulty)

    val secondHeader: BlockHeader = baseBlockHeader.copy(number = peer2Info.maxBlockNumber + 2)
    val secondBlock = NewBlock(Block(secondHeader, BlockBody(Nil, Nil)), newBlockWeight.totalDifficulty)

    //when
    peersInfoHolder ! MessageFromPeer(firstBlock, peer1.id)
    peersInfoHolder ! MessageFromPeer(secondBlock, peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    val expectedPeerInfo = initialPeerInfo
      .withBestBlockData(initialPeerInfo.maxBlockNumber + 4, firstHeader.hash)
      .withChainWeight(newBlockWeight)
    requestSender.expectMsg(PeerInfoResponse(Some(expectedPeerInfo)))
  }

  it should "update max peer when receiving new block PV164" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1InfoPV164)

    //given
    val newBlockWeight = ChainWeight.totalDifficultyOnly(300)
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = peer1Info.maxBlockNumber + 4)
    val firstBlock = PV164.NewBlock(Block(firstHeader, BlockBody(Nil, Nil)), newBlockWeight)

    val secondHeader: BlockHeader = baseBlockHeader.copy(number = peer2Info.maxBlockNumber + 2)
    val secondBlock = PV164.NewBlock(Block(secondHeader, BlockBody(Nil, Nil)), newBlockWeight)

    //when
    peersInfoHolder ! MessageFromPeer(firstBlock, peer1.id)
    peersInfoHolder ! MessageFromPeer(secondBlock, peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    val expectedPeerInfo = initialPeerInfoPV164
      .withBestBlockData(initialPeerInfo.maxBlockNumber + 4, firstHeader.hash)
      .withChainWeight(newBlockWeight)
    requestSender.expectMsg(PeerInfoResponse(Some(expectedPeerInfo)))
  }

  it should "update max peer when receiving block header" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1Info)

    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = peer1Info.maxBlockNumber + 4)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = peer1Info.maxBlockNumber + 2)

    //when
    peersInfoHolder ! MessageFromPeer(BlockHeaders(Seq(firstHeader, secondHeader, blockchain.genesisHeader)), peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(
      PeerInfoResponse(Some(peer1Info.withBestBlockData(initialPeerInfo.maxBlockNumber + 4, firstHeader.hash)))
    )
  }

  it should "update max peer when receiving new block hashes" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1Info)

    //given
    val firstBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), peer1Info.maxBlockNumber + 2)
    val secondBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("01" * 32)), peer1Info.maxBlockNumber + 5)

    //when
    peersInfoHolder ! MessageFromPeer(NewBlockHashes(Seq(firstBlockHash, secondBlockHash)), peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(
      PeerInfoResponse(Some(peer1Info.withBestBlockData(peer1Info.maxBlockNumber + 5, secondBlockHash.hash)))
    )
  }

  it should "update the peer total difficulty when receiving a NewBlock" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1Info)

    //given
    val newBlock = NewBlock(baseBlock, initialPeerInfo.chainWeight.totalDifficulty + 1)

    //when
    peersInfoHolder ! MessageFromPeer(newBlock, peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(
      PeerInfoResponse(Some(peer1Info.withChainWeight(ChainWeight.totalDifficultyOnly(newBlock.totalDifficulty))))
    )
  }

  it should "update the peer chain weight when receiving a PV164.NewBlock" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1InfoPV164)

    //given
    val newBlock = PV164.NewBlock(
      baseBlock,
      initialPeerInfoPV164.chainWeight
        .increaseTotalDifficulty(1)
        .copy(lastCheckpointNumber = initialPeerInfoPV164.chainWeight.lastCheckpointNumber + 1)
    )

    //when
    peersInfoHolder ! MessageFromPeer(newBlock, peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(PeerInfoResponse(Some(peer1InfoPV164.withChainWeight(newBlock.chainWeight))))
  }

  it should "update the fork accepted when receiving the fork block" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1Info)

    //given
    val blockHeaders = BlockHeaders(Seq(DaoForkBlock.header))

    //when
    peersInfoHolder ! MessageFromPeer(blockHeaders, peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(PeerInfoResponse(Some(peer1Info.withForkAccepted(true))))
  }

  it should "disconnect from a peer with different fork block" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1Info)

    //given
    val blockHeaders = BlockHeaders(Seq(Genesis.header.copy(number = Fixtures.Blocks.DaoForkBlock.header.number)))

    //when
    peersInfoHolder ! MessageFromPeer(blockHeaders, peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(PeerInfoResponse(Some(peer1Info)))
    peer1Probe.expectMsg(DisconnectPeer(Disconnect.Reasons.UselessPeer))
  }

  it should "remove peers information when a peers is disconnected" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))

    setupNewPeer(peer1, peer1Probe, peer1Info)
    setupNewPeer(peer2, peer2Probe, peer2Info)

    peersInfoHolder ! PeerDisconnected(peer2.id)

    //PeersInfoRequest should work properly
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(PeerInfoResponse(Some(peer1Info)))
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer2.id))
    requestSender.expectMsg(PeerInfoResponse(None))
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer3.id))
    requestSender.expectMsg(PeerInfoResponse(None))

    //GetHandshakedPeers should work properly
    requestSender.send(peersInfoHolder, GetHandshakedPeers)
    requestSender.expectMsg(HandshakedPeers(Map(peer1 -> peer1Info)))

    peersInfoHolder ! PeerDisconnected(peer1.id)

    //PeersInfoRequest should work properly
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(PeerInfoResponse(None))
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer2.id))
    requestSender.expectMsg(PeerInfoResponse(None))
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer3.id))
    requestSender.expectMsg(PeerInfoResponse(None))

    //GetHandshakedPeers should work properly
    requestSender.send(peersInfoHolder, GetHandshakedPeers)
    requestSender.expectMsg(HandshakedPeers(Map.empty))
  }

  it should "provide handshaked peers only with best block number determined" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    // Freshly handshaked peer without best block determined
    setupNewPeer(freshPeer, freshPeerProbe, freshPeerInfo.copy(maxBlockNumber = 0))

    requestSender.send(peersInfoHolder, GetHandshakedPeers)
    requestSender.expectMsg(HandshakedPeers(Map.empty))

    val newMaxBlock = freshPeerInfo.maxBlockNumber + 1
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = newMaxBlock)

    // Fresh peer received best block
    peersInfoHolder ! MessageFromPeer(BlockHeaders(Seq(firstHeader)), freshPeer.id)

    // After receiving peer best block number, peer should be provided as handshaked peer
    requestSender.send(peersInfoHolder, GetHandshakedPeers)
    requestSender.expectMsg(
      HandshakedPeers(Map(freshPeer -> freshPeerInfo.withBestBlockData(newMaxBlock, firstHeader.hash)))
    )
  }

  it should "provide handshaked peers only with best block number determined even if peers best block is its genesis" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))

    val genesisStatus = peerStatus.copy(bestHash = Fixtures.Blocks.Genesis.header.hash)
    val genesisInfo = initialPeerInfo.copy(
      remoteStatus = genesisStatus,
      maxBlockNumber = Fixtures.Blocks.Genesis.header.number,
      bestBlockHash = Fixtures.Blocks.Genesis.header.hash
    )

    // Freshly handshaked peer without best block determined
    setupNewPeer(freshPeer, freshPeerProbe, genesisInfo)

    // if peer best block is its genesis block then it is available right from the start
    requestSender.send(peersInfoHolder, GetHandshakedPeers)
    requestSender.expectMsg(HandshakedPeers(Map(freshPeer -> genesisInfo)))

    // Fresh peer received best block
    peersInfoHolder ! MessageFromPeer(BlockHeaders(Seq(Fixtures.Blocks.Genesis.header)), freshPeer.id)

    // receiving best block does not change a thing, as peer best block is it genesis
    requestSender.send(peersInfoHolder, GetHandshakedPeers)
    requestSender.expectMsg(HandshakedPeers(Map(freshPeer -> genesisInfo)))
  }

  trait TestSetup extends EphemBlockchainTestSetup {
    override implicit lazy val system = ActorSystem("PeersInfoHolderSpec_System")

    blockchain.storeBlockHeader(Fixtures.Blocks.Genesis.header).commit()

    override lazy val blockchainConfig = Config.blockchains.blockchainConfig
    val forkResolver = new ForkResolver.EtcForkResolver(blockchainConfig.daoForkConfig.get)

    val peerStatus = RemoteStatus(
      protocolVersion = ProtocolVersions.PV63,
      networkId = 1,
      chainWeight = ChainWeight.totalDifficultyOnly(10000),
      bestHash = Fixtures.Blocks.Block3125369.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )

    val initialPeerInfo = PeerInfo(
      remoteStatus = peerStatus,
      chainWeight = peerStatus.chainWeight,
      forkAccepted = false,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number,
      bestBlockHash = peerStatus.bestHash
    )

    val initialPeerInfoPV164 = PeerInfo(
      remoteStatus = peerStatus.copy(protocolVersion = ProtocolVersions.PV164),
      chainWeight = peerStatus.chainWeight,
      forkAccepted = false,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number,
      bestBlockHash = peerStatus.bestHash
    )

    val fakeNodeId = ByteString()

    val peer1Probe = TestProbe()
    val peer1 = Peer(PeerId("peer1"), new InetSocketAddress("127.0.0.1", 1), peer1Probe.ref, false, Some(fakeNodeId))
    val peer1Info = initialPeerInfo.withForkAccepted(false)
    val peer1InfoPV164 = initialPeerInfoPV164.withForkAccepted(false)
    val peer2Probe = TestProbe()
    val peer2 = Peer(PeerId("peer2"), new InetSocketAddress("127.0.0.1", 2), peer2Probe.ref, false, Some(fakeNodeId))
    val peer2Info = initialPeerInfo.withForkAccepted(false)
    val peer3Probe = TestProbe()
    val peer3 = Peer(PeerId("peer3"), new InetSocketAddress("127.0.0.1", 3), peer3Probe.ref, false, Some(fakeNodeId))

    val freshPeerProbe = TestProbe()
    val freshPeer =
      Peer(PeerId(""), new InetSocketAddress("127.0.0.1", 4), freshPeerProbe.ref, false, Some(fakeNodeId))
    val freshPeerInfo = initialPeerInfo.withForkAccepted(false)

    val peerManager = TestProbe()
    val peerEventBus = TestProbe()

    val peersInfoHolder = TestActorRef(
      Props(
        new EtcPeerManagerActor(
          peerManager.ref,
          peerEventBus.ref,
          storagesInstance.storages.appStateStorage,
          Some(forkResolver)
        )
      )
    )

    val requestSender = TestProbe()

    val baseBlockHeader = Fixtures.Blocks.Block3125369.header
    val baseBlockBody = BlockBody(Nil, Nil)
    val baseBlock = Block(baseBlockHeader, baseBlockBody)

    def setupNewPeer(peer: Peer, peerProbe: TestProbe, peerInfo: PeerInfo): Unit = {

      peersInfoHolder ! PeerHandshakeSuccessful(peer, peerInfo)

      peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))

      peerEventBus.expectMsg(
        Subscribe(
          MessageClassifier(
            Set(Codes.BlockHeadersCode, Codes.NewBlockCode, Codes.NewBlockHashesCode),
            PeerSelector.WithId(peer.id)
          )
        )
      )

      //Peer should receive request for highest block
      peerProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(peerInfo.remoteStatus.bestHash), 1, 0, false)))
    }
  }

}
