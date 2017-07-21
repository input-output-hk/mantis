package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.Fixtures.Blocks.{DaoForkBlock, Genesis}
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.domain.{Block, BlockHeader, BlockchainImpl}
import io.iohk.ethereum.network.PeerActor.DisconnectPeer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{MessageFromPeer, PeerDisconnected, PeerHandshakeSuccessful}
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier._
import io.iohk.ethereum.network.EtcPeerManagerActor._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class EtcPeerManagerSpec extends FlatSpec with Matchers {

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

  it should "update max peer when receiving new block" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1Info)

    //given
    val newBlockTD = 300
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = peer1Info.maxBlockNumber + 4)
    val firstBlock = NewBlock(Block(firstHeader, BlockBody(Nil, Nil)), newBlockTD)

    val secondHeader: BlockHeader = baseBlockHeader.copy(number = peer2Info.maxBlockNumber + 2)
    val secondBlock = NewBlock(Block(secondHeader, BlockBody(Nil, Nil)), newBlockTD)

    //when
    peersInfoHolder ! MessageFromPeer(firstBlock, peer1.id)
    peersInfoHolder ! MessageFromPeer(secondBlock, peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    val expectedPeerInfo = initialPeerInfo
      .withMaxBlockNumber(initialPeerInfo.maxBlockNumber + 4)
      .withTotalDifficulty(newBlockTD)
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
    requestSender.expectMsg(PeerInfoResponse(Some(peer1Info.withMaxBlockNumber(initialPeerInfo.maxBlockNumber + 4))))
  }

  it should "update max peer when receiving new block hashes" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1Info)

    //given
    val firstBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), peer1Info.maxBlockNumber + 2)
    val secondBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), peer1Info.maxBlockNumber + 5)

    //when
    peersInfoHolder ! MessageFromPeer(NewBlockHashes(Seq(firstBlockHash, secondBlockHash)), peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(PeerInfoResponse(Some(peer1Info.withMaxBlockNumber(peer1Info.maxBlockNumber + 5))))
  }

  it should "update max peer when sending new block" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1Info)

    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = peer1Info.maxBlockNumber + 4)
    val firstBlock = NewBlock(Block(firstHeader, BlockBody(Nil, Nil)), 300)

    val secondHeader: BlockHeader = baseBlockHeader.copy(number = peer1Info.maxBlockNumber + 2)
    val secondBlock = NewBlock(Block(secondHeader, BlockBody(Nil, Nil)), 300)

    //when
    peersInfoHolder ! EtcPeerManagerActor.SendMessage(firstBlock, peer1.id)
    peersInfoHolder ! EtcPeerManagerActor.SendMessage(secondBlock, peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(PeerInfoResponse(Some(peer1Info.withMaxBlockNumber(peer1Info.maxBlockNumber + 4))))
    peerManager.expectMsgAllOf(
      PeerManagerActor.SendMessage(firstBlock, peer1.id),
      PeerManagerActor.SendMessage(secondBlock, peer1.id)
    )
  }

  it should "update max peer when sending block header" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1Info)

    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = peer1Info.maxBlockNumber + 4)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = peer1Info.maxBlockNumber + 2)

    //when
    peersInfoHolder ! EtcPeerManagerActor.SendMessage(BlockHeaders(Seq(firstHeader)), peer1.id)
    peersInfoHolder ! EtcPeerManagerActor.SendMessage(BlockHeaders(Seq(secondHeader)), peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(PeerInfoResponse(Some(peer1Info.withMaxBlockNumber(peer1Info.maxBlockNumber + 4))))
    peerManager.expectMsgAllOf(
      PeerManagerActor.SendMessage(BlockHeaders(Seq(firstHeader)), peer1.id),
      PeerManagerActor.SendMessage(BlockHeaders(Seq(secondHeader)), peer1.id)
    )
  }

  it should "update max peer when sending new block hashes" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1Info)

    //given
    val firstBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), peer1Info.maxBlockNumber + 2)
    val secondBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), peer1Info.maxBlockNumber + 5)

    //when
    peersInfoHolder ! EtcPeerManagerActor.SendMessage(NewBlockHashes(Seq(firstBlockHash)), peer1.id)
    peersInfoHolder ! EtcPeerManagerActor.SendMessage(NewBlockHashes(Seq(secondBlockHash)), peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(PeerInfoResponse(Some(peer1Info.withMaxBlockNumber(peer1Info.maxBlockNumber + 5))))
    peerManager.expectMsgAllOf(
      PeerManagerActor.SendMessage(NewBlockHashes(Seq(firstBlockHash)), peer1.id),
      PeerManagerActor.SendMessage(NewBlockHashes(Seq(secondBlockHash)), peer1.id)
    )
  }

  it should "update the peer total difficulty when receiving a NewBlock" in new TestSetup {
    peerEventBus.expectMsg(Subscribe(PeerHandshaked))
    setupNewPeer(peer1, peer1Probe, peer1Info)

    //given
    val newBlock = NewBlock(baseBlock, initialPeerInfo.totalDifficulty + 1)

    //when
    peersInfoHolder ! MessageFromPeer(newBlock, peer1.id)

    //then
    requestSender.send(peersInfoHolder, PeerInfoRequest(peer1.id))
    requestSender.expectMsg(PeerInfoResponse(Some(peer1Info.withTotalDifficulty(newBlock.totalDifficulty))))
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

  trait TestSetup extends EphemBlockchainTestSetup {
    implicit val system = ActorSystem("PeersInfoHolderSpec_System")

    blockchain.save(Fixtures.Blocks.Genesis.header)

    val blockchainConfig = BlockchainConfig(Config.config)
    val forkResolver = new ForkResolver.EtcForkResolver(blockchainConfig)

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

    val peer1Probe = TestProbe()
    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 1), peer1Probe.ref)
    val peer1Info = initialPeerInfo.withForkAccepted(false)
    val peer2Probe = TestProbe()
    val peer2 = Peer(new InetSocketAddress("127.0.0.1", 2), peer2Probe.ref)
    val peer2Info = initialPeerInfo.withForkAccepted(false)
    val peer3Probe = TestProbe()
    val peer3 = Peer(new InetSocketAddress("127.0.0.1", 3), peer3Probe.ref)

    val peerManager = TestProbe()
    val peerEventBus = TestProbe()

    val peersInfoHolder = TestActorRef(Props(new EtcPeerManagerActor(
      peerManager.ref, peerEventBus.ref, storagesInstance.storages.appStateStorage, Some(forkResolver))))

    val requestSender = TestProbe()

    val baseBlockHeader = Fixtures.Blocks.Block3125369.header
    val baseBlockBody = BlockBody(Nil, Nil)
    val baseBlock = Block(baseBlockHeader, baseBlockBody)

    def setupNewPeer(peer: Peer, peerProbe: TestProbe, peerInfo: PeerInfo): Unit ={

      peersInfoHolder ! PeerHandshakeSuccessful(peer, peerInfo)

      peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))

      peerEventBus.expectMsg(Subscribe(MessageClassifier(
        Set(BlockHeaders.code, NewBlock.code, NewBlockHashes.code), PeerSelector.WithId(peer.id))))

      //Peer should receive request for highest block
      peerProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(peerInfo.remoteStatus.bestHash), 1, 0, false)))
    }
  }

}
