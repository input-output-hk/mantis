package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.{Fixtures, Mocks, Timeouts}
import io.iohk.ethereum.blockchain.sync.FastSync.{StateMptNodeHash, SyncState}
import io.iohk.ethereum.blockchain.sync.SyncController.MinedBlock
import io.iohk.ethereum.domain.{Account, Block, BlockHeader}
import io.iohk.ethereum.ledger.{BloomFilter, Ledger}
import io.iohk.ethereum.network.EtcPeerManagerActor.{GetHandshakedPeers, HandshakedPeers, PeerInfo}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerDisconnectedClassifier}
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, _}
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, GetReceipts, NodeData, Receipts}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}
import io.iohk.ethereum.ommers.OmmersPool.{AddOmmers, RemoveOmmers}
import io.iohk.ethereum.transactions.PendingTransactionsManager.{AddTransactions, RemoveTransactions}
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Config.SyncConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

import scala.collection.immutable.Queue
import scala.concurrent.duration._

// scalastyle:off file.size.limit
class SyncControllerSpec extends FlatSpec with Matchers {

  "SyncController" should "download target block and request state nodes" in new TestSetup() {

    val peer1TestProbe: TestProbe = TestProbe("peer1")(system)
    val peer2TestProbe: TestProbe = TestProbe("peer2")(system)

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref, false)
    val peer2 = Peer(new InetSocketAddress("127.0.0.2", 0), peer2TestProbe.ref, false)

    time.advance(1.seconds)

    val peer1Status = Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status = Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer1 -> PeerInfo(peer1Status, peer1Status.totalDifficulty, forkAccepted = true, maxBlockNumber = 0),
      peer2 -> PeerInfo(peer2Status, peer1Status.totalDifficulty, forkAccepted = true, maxBlockNumber = 0))))

    syncController ! SyncController.StartSync

    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Right(ByteString("peer1_bestHash")), 1, 0, reverse = false), peer1.id))
    syncController ! MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000))), peer1.id)

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Right(ByteString("peer2_bestHash")), 1, 0, reverse = false), peer2.id))
    syncController ! MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 400000))), peer2.id)

    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedTargetBlock), 1, 0, reverse = false), peer2.id))

    storagesInstance.storages.appStateStorage.putBestBlockNumber(targetBlockHeader.number)
    syncController ! MessageFromPeer(BlockHeaders(Seq(targetBlockHeader)), peer2.id)

    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot)), peer1.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(1), 10, 0, reverse = false), peer2.id)
    )
    etcPeerManager.expectNoMsg()

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer1.id)))
    )
  }

  it should "download target block, request state, blocks and finish when downloaded" in new TestSetup() {
    val peer2TestProbe: TestProbe = TestProbe()(system)
    val peer2 = Peer(new InetSocketAddress("127.0.0.1", 0), peer2TestProbe.ref, false)

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(
      number = expectedTargetBlock,
      stateRoot = ByteString(Hex.decode("deae1dfad5ec8dcef15915811e1f044d2543674fd648f94345231da9fc2646cc")))
    val bestBlockHeaderNumber: BigInt = targetBlockHeader.number - 1
    storagesInstance.storages.fastSyncStateStorage.putSyncState(SyncState(targetBlockHeader)
      .copy(bestBlockHeaderNumber = bestBlockHeaderNumber,
        mptNodesQueue = Seq(StateMptNodeHash(targetBlockHeader.stateRoot))))

    time.advance(1.seconds)

    val peer2Status = Status(1, 1, 20, ByteString("peer2_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer2 -> PeerInfo(peer2Status, forkAccepted = true, totalDifficulty = peer2Status.totalDifficulty, maxBlockNumber = 0))))

    syncController ! SyncController.StartSync

    val stateMptLeafWithAccount =
      ByteString(Hex.decode("f86d9e328415c225a782bb339b22acad1c739e42277bc7ef34de3623114997ce78b84cf84a0186cb7d8738d800a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

    val watcher = TestProbe()
    watcher.watch(syncController)

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(NodeData(Seq(stateMptLeafWithAccount)), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))

    //wait for peers throttle
    Thread.sleep(Config.Sync.fastSyncThrottle.toMillis)
    //trigger scheduling
    time.advance(2.second)
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(targetBlockHeader.number), expectedTargetBlock - bestBlockHeaderNumber, 0, reverse = false),
      peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq(targetBlockHeader)), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    Thread.sleep(Config.Sync.fastSyncThrottle.toMillis)
    time.advance(2.second)
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetReceipts(Seq(targetBlockHeader.hash)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(Receipts(Seq(Nil)), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer2.id))))

    Thread.sleep(Config.Sync.fastSyncThrottle.toMillis)
    time.advance(2.second)
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockBodies(Seq(targetBlockHeader.hash)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil))), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer2.id))))

    //switch to regular download
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(targetBlockHeader.number + 1), Config.Sync.blockHeadersPerRequest, 0, reverse = false),
      peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))
  }

  it should "request for block bodies again if block bodies validation fails" in new TestSetup() {
    override val syncController = TestActorRef(Props(new SyncController(
      storagesInstance.storages.appStateStorage,
      blockchain,
      storagesInstance.storages,
      storagesInstance.storages.fastSyncStateStorage,
      ledger,
      new Mocks.MockValidatorsFailingOnBlockBodies,
      peerMessageBus.ref, pendingTransactionsManager.ref, ommersPool.ref, etcPeerManager.ref,
      syncConfig(Config.Sync.doFastSync),
      externalSchedulerOpt = Some(time.scheduler))))


    val peer1TestProbe: TestProbe = TestProbe()(system)
    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref, incomingConnection = false)
    val peer2TestProbe: TestProbe = TestProbe()(system)
    val peer2 = Peer(new InetSocketAddress("127.0.0.1", 0), peer2TestProbe.ref, incomingConnection = false)

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(
      number = expectedTargetBlock,
      stateRoot = ByteString(Hex.decode("deae1dfad5ec8dcef15915811e1f044d2543674fd648f94345231da9fc2646cc")))
    val bestBlockHeaderNumber: BigInt = targetBlockHeader.number - 1
    storagesInstance.storages.fastSyncStateStorage.putSyncState(SyncState(targetBlockHeader)
      .copy(bestBlockHeaderNumber = bestBlockHeaderNumber,
        mptNodesQueue = Seq(StateMptNodeHash(targetBlockHeader.stateRoot))))

    time.advance(1.seconds)

    val peerStatus = Status(1, 1, 20, ByteString("peer2_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer2 -> PeerInfo(peerStatus, forkAccepted = true, totalDifficulty = peerStatus.totalDifficulty, maxBlockNumber = 0))))

    syncController ! SyncController.StartSync

    val stateMptLeafWithAccount =
      ByteString(Hex.decode("f86d9e328415c225a782bb339b22acad1c739e42277bc7ef34de3623114997ce78b84cf84a0186cb7d8738d800a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

    val watcher = TestProbe()
    watcher.watch(syncController)

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(NodeData(Seq(stateMptLeafWithAccount)), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))

    Thread.sleep(Config.Sync.fastSyncThrottle.toMillis)
    time.advance(2.second)
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(targetBlockHeader.number), expectedTargetBlock - bestBlockHeaderNumber, 0, reverse = false),
      peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq(targetBlockHeader)), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    Thread.sleep(Config.Sync.fastSyncThrottle.toMillis)
    time.advance(2.second)
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetReceipts(Seq(targetBlockHeader.hash)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(Receipts(Seq(Nil)), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer2.id))))

    Thread.sleep(2.second.toMillis)
    time.advance(2.second)
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockBodies(Seq(targetBlockHeader.hash)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil))), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer2.id))))

    //peer was blacklisted for bad block bodies. connecting second peer
    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer1 -> PeerInfo(peerStatus, forkAccepted = true, totalDifficulty = peerStatus.totalDifficulty, maxBlockNumber = 0))))

    time.advance(1.seconds)

    //ask different peer for block bodies again
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetBlockBodies(Seq(targetBlockHeader.hash)), peer1.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer1.id))))
  }

  it should "throttle requests to peer" in  new TestSetup() {
    val peerTestProbe: TestProbe = TestProbe()(system)
    val peer = Peer(new InetSocketAddress("127.0.0.1", 0), peerTestProbe.ref, incomingConnection = false)

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(
      number = expectedTargetBlock,
      stateRoot = ByteString(Hex.decode("deae1dfad5ec8dcef15915811e1f044d2543674fd648f94345231da9fc2646cc")))
    val bestBlockHeaderNumber: BigInt = targetBlockHeader.number - 1
    storagesInstance.storages.fastSyncStateStorage.putSyncState(SyncState(targetBlockHeader)
      .copy(bestBlockHeaderNumber = bestBlockHeaderNumber,
        mptNodesQueue = Seq(StateMptNodeHash(targetBlockHeader.stateRoot))))

    time.advance(1.seconds)

    val peerStatus = Status(1, 1, 20, ByteString("peer2_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer -> PeerInfo(peerStatus, forkAccepted = true, totalDifficulty = peerStatus.totalDifficulty, maxBlockNumber = 0))))

    syncController ! SyncController.StartSync

    val stateMptLeafWithAccount =
      ByteString(Hex.decode("f86d9e328415c225a782bb339b22acad1c739e42277bc7ef34de3623114997ce78b84cf84a0186cb7d8738d800a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

    val watcher = TestProbe()
    watcher.watch(syncController)

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot)), peer.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer.id))))
    peerMessageBus.reply(MessageFromPeer(NodeData(Seq(stateMptLeafWithAccount)), peer.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer.id))))

    //trigger scheduling
    time.advance(2.second)
    etcPeerManager.expectNoMsg()

    //wait for peers throttle
    Thread.sleep(Config.Sync.fastSyncThrottle.toMillis)

    //trigger scheduling again
    time.advance(2.second)
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(targetBlockHeader.number), expectedTargetBlock - bestBlockHeaderNumber, 0, reverse = false),
      peer.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq(targetBlockHeader)), peer.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
  }

  it should "not use (blacklist) a peer that fails to respond within time limit" in new TestSetup() {
    val peer2TestProbe: TestProbe = TestProbe()(system)

    val peer2 = Peer(new InetSocketAddress("127.0.0.1", 0), peer2TestProbe.ref, false)

    time.advance(1.seconds)

    val peer2Status = Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer2 -> PeerInfo(peer2Status, forkAccepted = true, totalDifficulty = peer2Status.totalDifficulty, maxBlockNumber = 0))))

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)

    storagesInstance.storages.fastSyncStateStorage.putSyncState(SyncState(targetBlockHeader)
      .copy(bestBlockHeaderNumber = targetBlockHeader.number,
        mptNodesQueue = Seq(StateMptNodeHash(targetBlockHeader.stateRoot))))

    syncController ! SyncController.StartSync

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))

    // response timeout
    time.advance(2.seconds)
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))
    etcPeerManager.expectNoMsg()

    // wait for blacklist timeout
    time.advance(6.seconds)
    etcPeerManager.expectNoMsg()

    // wait for next sync retry
    time.advance(3.seconds)

    // peer should not be blacklisted anymore
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))
  }

  it should "start regular download " in new TestSetup() {
    val peerTestProbe: TestProbe = TestProbe()(system)

    val peer = Peer(new InetSocketAddress("127.0.0.1", 0), peerTestProbe.ref, false)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0))))

    val expectedMaxBlock = 399500
    val newBlockDifficulty = 23
    val maxBlocTotalDifficulty = 12340
    val maxBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedMaxBlock)
    val newBlockHeader: BlockHeader = baseBlockHeader
      .copy(number = expectedMaxBlock + 1, parentHash = maxBlockHeader.hash, difficulty = newBlockDifficulty,
        stateRoot = ByteString(Hex.decode("d0aedc3838a3d7f9a526bdd642b55fb1b6292596985cfab2eedb751da19b8bb4")))

    storagesInstance.storages.appStateStorage.putBestBlockNumber(maxBlockHeader.number)
    storagesInstance.storages.blockHeadersStorage.put(maxBlockHeader.hash, maxBlockHeader)
    storagesInstance.storages.blockNumberMappingStorage.put(maxBlockHeader.number, maxBlockHeader.hash)
    storagesInstance.storages.totalDifficultyStorage.put(maxBlockHeader.hash, maxBlocTotalDifficulty)

    storagesInstance.storages.appStateStorage.fastSyncDone()

    syncController ! SyncController.StartSync

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(expectedMaxBlock + 1), Config.Sync.blockHeadersPerRequest, 0, reverse = false),
      peer.id))
    syncController.children.last ! MessageFromPeer(BlockHeaders(Seq(newBlockHeader)), peer.id)

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetBlockBodies(Seq(newBlockHeader.hash)), peer.id))
    syncController.children.last ! MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil))), peer.id)

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))),
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))),
      Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))),
      Unsubscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id)))
    )

    etcPeerManager.expectMsgAllOf(Timeouts.normalTimeout,
      EtcPeerManagerActor.SendMessage(
        GetBlockHeaders(Left(expectedMaxBlock + 2), Config.Sync.blockHeadersPerRequest, 0, reverse = false),
        peer.id),
      EtcPeerManagerActor.SendMessage(
        NewBlock(Block(newBlockHeader, BlockBody(Nil, Nil)), maxBlocTotalDifficulty + newBlockDifficulty), peer.id)
    )
    etcPeerManager.expectNoMsg()

    blockchain.getBlockByNumber(expectedMaxBlock + 1) shouldBe Some(Block(newBlockHeader, BlockBody(Nil, Nil)))
    blockchain.getTotalDifficultyByHash(newBlockHeader.hash) shouldBe Some(maxBlocTotalDifficulty + newBlockHeader.difficulty)

    ommersPool.expectMsg(RemoveOmmers(newBlockHeader))
    pendingTransactionsManager.expectMsg(AddTransactions(Nil))
    pendingTransactionsManager.expectMsg(RemoveTransactions(Nil))
    ommersPool.expectNoMsg()
    pendingTransactionsManager.expectNoMsg()
  }

  it should "resolve branch conflict" in new TestSetup() {
    val peerTestProbe: TestProbe = TestProbe()(system)
    val peer = Peer(new InetSocketAddress("127.0.0.1", 0), peerTestProbe.ref, false)

    time.advance(1.seconds)

    val peer1Status = Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0))))

    val expectedMaxBlock = 399500
    val newBlockDifficulty = 23
    val commonRootTotalDifficulty = 12340

    val commonRoot: BlockHeader = baseBlockHeader.copy(number = expectedMaxBlock - 1)
    val maxBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedMaxBlock, parentHash = commonRoot.hash, difficulty = 5)

    val newBlockHeaderParent: BlockHeader = baseBlockHeader.copy(number = expectedMaxBlock, parentHash = commonRoot.hash, difficulty = newBlockDifficulty,
      stateRoot = ByteString(Hex.decode("d0aedc3838a3d7f9a526bdd642b55fb1b6292596985cfab2eedb751da19b8bb4")))
    val newBlockHeader: BlockHeader = baseBlockHeader
      .copy(number = expectedMaxBlock + 1, parentHash = newBlockHeaderParent.hash, difficulty = newBlockDifficulty,
        stateRoot = ByteString(Hex.decode("36c8b1c29ea8aeee08516f182721a9e0af77f924f7fc8d7db60a11e3223d11ee")))
    val nextNewBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedMaxBlock + 2, parentHash = newBlockHeader.hash, difficulty = newBlockDifficulty,
      stateRoot = ByteString(Hex.decode("f5915b81ca32d039e187b92a0d63b8c545f0496ade014f86afaaa596696c45cf")))

    storagesInstance.storages.appStateStorage.putBestBlockNumber(maxBlockHeader.number)

    storagesInstance.storages.blockHeadersStorage.put(maxBlockHeader.hash, maxBlockHeader)
    storagesInstance.storages.blockBodiesStorage.put(maxBlockHeader.hash, BlockBody(Nil, Nil))
    storagesInstance.storages.blockNumberMappingStorage.put(maxBlockHeader.number, maxBlockHeader.hash)

    storagesInstance.storages.blockHeadersStorage.put(commonRoot.hash, commonRoot)
    storagesInstance.storages.blockNumberMappingStorage.put(commonRoot.number, commonRoot.hash)

    storagesInstance.storages.totalDifficultyStorage.put(commonRoot.hash, commonRootTotalDifficulty)
    storagesInstance.storages.totalDifficultyStorage.put(maxBlockHeader.hash, commonRootTotalDifficulty + maxBlockHeader.difficulty)

    storagesInstance.storages.appStateStorage.fastSyncDone()

    syncController ! SyncController.StartSync

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(expectedMaxBlock + 1), Config.Sync.blockHeadersPerRequest, 0, reverse = false),
      peer.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Queue(newBlockHeader)), peer.id))

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
    syncController.children.last ! MessageFromPeer(BlockHeaders(Queue(newBlockHeaderParent)), peer.id)

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))),
      Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))))

    syncController.children.last ! MessageFromPeer(BlockBodies(Queue(BlockBody(Nil, Nil), BlockBody(Nil, Nil))), peer.id)

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Right(newBlockHeader.parentHash), Config.Sync.blockResolveDepth, 0, reverse = true),
      peer.id))
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockBodies(Queue(newBlockHeaderParent.hash, newBlockHeader.hash)),
      peer.id))

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))),
      Unsubscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))))
    syncController.children.last ! MessageFromPeer(BlockHeaders(Queue(nextNewBlockHeader)), peer.id)

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))),
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
    syncController.children.last ! MessageFromPeer(BlockBodies(Queue(BlockBody(Nil, Nil))), peer.id)

    //start next download cycle

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(
        GetBlockHeaders(Left(expectedMaxBlock + 2), Config.Sync.blockHeadersPerRequest, 0, reverse = false),
        peer.id),
      EtcPeerManagerActor.SendMessage(
        NewBlock(Block(newBlockHeaderParent, BlockBody(Nil, Nil)), commonRootTotalDifficulty + newBlockDifficulty),
        peer.id),
      EtcPeerManagerActor.SendMessage(
        NewBlock(Block(newBlockHeader, BlockBody(Nil, Nil)), commonRootTotalDifficulty + 2 * newBlockDifficulty),
        peer.id)
    )
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetBlockBodies(Seq(nextNewBlockHeader.hash)), peer.id))

    //wait for actor to insert data
    Thread.sleep(Timeouts.normalTimeout.toMillis)

    blockchain.getBlockByNumber(expectedMaxBlock) shouldBe Some(Block(newBlockHeaderParent, BlockBody(Nil, Nil)))
    blockchain.getTotalDifficultyByHash(newBlockHeaderParent.hash) shouldBe Some(commonRootTotalDifficulty + newBlockHeaderParent.difficulty)

    blockchain.getBlockByNumber(expectedMaxBlock + 1) shouldBe Some(Block(newBlockHeader, BlockBody(Nil, Nil)))
    blockchain.getTotalDifficultyByHash(newBlockHeader.hash) shouldBe Some(commonRootTotalDifficulty + newBlockHeaderParent.difficulty + newBlockHeader.difficulty)

    blockchain.getBlockByNumber(expectedMaxBlock + 2) shouldBe Some(Block(nextNewBlockHeader, BlockBody(Nil, Nil)))
    blockchain.getTotalDifficultyByHash(nextNewBlockHeader.hash) shouldBe Some(commonRootTotalDifficulty + newBlockHeaderParent.difficulty + newBlockHeader.difficulty + nextNewBlockHeader.difficulty)

    storagesInstance.storages.appStateStorage.getBestBlockNumber() shouldBe nextNewBlockHeader.number

    blockchain.getBlockHeaderByHash(maxBlockHeader.hash) shouldBe None
    blockchain.getBlockBodyByHash(maxBlockHeader.hash) shouldBe None
    blockchain.getTotalDifficultyByHash(maxBlockHeader.hash) shouldBe None

    ommersPool.expectMsg(AddOmmers(maxBlockHeader))
    ommersPool.expectMsg(RemoveOmmers(newBlockHeaderParent))
    ommersPool.expectMsg(RemoveOmmers(newBlockHeader))
    ommersPool.expectMsg(RemoveOmmers(nextNewBlockHeader))

    pendingTransactionsManager.expectMsg(AddTransactions(Nil))
    pendingTransactionsManager.expectMsg(RemoveTransactions(Nil))
    pendingTransactionsManager.expectMsg(RemoveTransactions(Nil))

    pendingTransactionsManager.expectMsg(AddTransactions(Nil))
    pendingTransactionsManager.expectMsg(RemoveTransactions(Nil))

    ommersPool.expectNoMsg()
    pendingTransactionsManager.expectNoMsg()
  }

  it should "only use ETC peer to choose target block" in new TestSetup() {
    val peer1TestProbe: TestProbe = TestProbe()(system)
    val peer2TestProbe: TestProbe = TestProbe()(system)
    val peer3TestProbe: TestProbe = TestProbe()(system)
    val peer4TestProbe: TestProbe = TestProbe()(system)

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref, false)
    val peer2 = Peer(new InetSocketAddress("127.0.0.2", 0), peer2TestProbe.ref, false)
    val peer3 = Peer(new InetSocketAddress("127.0.0.3", 0), peer3TestProbe.ref, false)
    val peer4 = Peer(new InetSocketAddress("127.0.0.4", 0), peer4TestProbe.ref, false)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status= Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))
    val peer3Status= Status(1, 1, 1, ByteString("peer3_bestHash"), ByteString("unused"))
    val peer4Status= Status(1, 1, 1, ByteString("peer4_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer1 -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0),
      peer2 -> PeerInfo(peer2Status, forkAccepted = false, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0),
      peer3 -> PeerInfo(peer3Status, forkAccepted = false, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0),
      peer4 -> PeerInfo(peer4Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0))))

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)
    storagesInstance.storages.appStateStorage.putBestBlockNumber(targetBlockHeader.number)

    syncController ! SyncController.StartSync

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer4.id))))

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Right(ByteString("peer1_bestHash")), 1, 0, reverse = false),
      peer1.id))
    syncController ! MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000))), peer1.id)
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Right(ByteString("peer4_bestHash")), 1, 0, reverse = false),
      peer4.id))
    etcPeerManager.expectNoMsg()

    syncController ! MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000))), peer4.id)

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer4.id))),
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
  }

  it should "broadcast all blocks if they were all valid" in new TestSetup() {
    val peer1TestProbe: TestProbe = TestProbe()(system)

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref, false)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer1 -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0)
    )))

    val expectedMaxBlock = 399500
    val newBlockDifficulty = 23
    val maxBlocTotalDifficulty = 12340
    val maxBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedMaxBlock)
    val newBlockHeader: BlockHeader = baseBlockHeader
      .copy(number = expectedMaxBlock + 1, parentHash = maxBlockHeader.hash, difficulty = newBlockDifficulty,
        stateRoot = ByteString(Hex.decode("d0aedc3838a3d7f9a526bdd642b55fb1b6292596985cfab2eedb751da19b8bb4")))
    val nextNewBlockHeader: BlockHeader = baseBlockHeader
      .copy(number = expectedMaxBlock + 2, parentHash = newBlockHeader.hash, difficulty = newBlockDifficulty,
        stateRoot = ByteString(Hex.decode("36c8b1c29ea8aeee08516f182721a9e0af77f924f7fc8d7db60a11e3223d11ee")))

    storagesInstance.storages.appStateStorage.putBestBlockNumber(maxBlockHeader.number)
    storagesInstance.storages.blockHeadersStorage.put(maxBlockHeader.hash, maxBlockHeader)
    storagesInstance.storages.blockNumberMappingStorage.put(maxBlockHeader.number, maxBlockHeader.hash)
    storagesInstance.storages.totalDifficultyStorage.put(maxBlockHeader.hash, maxBlocTotalDifficulty)

    storagesInstance.storages.appStateStorage.fastSyncDone()

    syncController ! SyncController.StartSync

    //Turn broadcasting on the RegularSync on by sending an empty BlockHeaders message:
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(expectedMaxBlock + 1), Config.Sync.blockHeadersPerRequest, 0, reverse = false),
      peer1.id))

    syncController.children.last ! MessageFromPeer(BlockHeaders(Seq()), peer1.id)

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))

    time.advance(Config.Sync.checkForNewBlockInterval)

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(expectedMaxBlock + 1), Config.Sync.blockHeadersPerRequest, 0, reverse = false),
      peer1.id))

    syncController.children.last ! MessageFromPeer(BlockHeaders(Seq(newBlockHeader, nextNewBlockHeader)), peer1.id)

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer1.id))))

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockBodies(Seq(newBlockHeader.hash, nextNewBlockHeader.hash)),
      peer1.id))

    syncController.children.last ! MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil), BlockBody(Nil, Nil))), peer1.id)

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
        NewBlock(Block(newBlockHeader, BlockBody(Nil, Nil)), maxBlocTotalDifficulty + newBlockDifficulty), peer1.id))
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
        NewBlock(Block(nextNewBlockHeader, BlockBody(Nil, Nil)), maxBlocTotalDifficulty + 2 * newBlockDifficulty), peer1.id))

    ommersPool.expectMsg(RemoveOmmers(newBlockHeader))
    ommersPool.expectMsg(RemoveOmmers(nextNewBlockHeader))

    pendingTransactionsManager.expectMsg(AddTransactions(Nil))
    pendingTransactionsManager.expectMsg(RemoveTransactions(Nil))
    pendingTransactionsManager.expectMsg(RemoveTransactions(Nil))

    ommersPool.expectNoMsg()
    pendingTransactionsManager.expectNoMsg()
  }

  val invalidBlockNumber = 399502

  it should "only broadcast blocks that it was able to successfully execute" in new TestSetup(Seq(invalidBlockNumber)) {

    val peer1TestProbe: TestProbe = TestProbe()(system)
    val peer2TestProbe: TestProbe = TestProbe()(system)

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref, false)
    val peer2 = Peer(new InetSocketAddress("127.0.0.2", 0), peer2TestProbe.ref, false)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status= Status(1, 1, totalDifficulty = 0, ByteString("peer2_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer1 -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0),
      peer2 -> PeerInfo(peer2Status, forkAccepted = true, totalDifficulty = peer2Status.totalDifficulty, maxBlockNumber = 0)
    )))

    val expectedMaxBlock = 399500
    val newBlockDifficulty = 23
    val maxBlocTotalDifficulty = 12340
    val maxBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedMaxBlock)
    val newBlockHeader: BlockHeader = baseBlockHeader
      .copy(number = expectedMaxBlock + 1, parentHash = maxBlockHeader.hash, difficulty = newBlockDifficulty,
        stateRoot = ByteString(Hex.decode("d0aedc3838a3d7f9a526bdd642b55fb1b6292596985cfab2eedb751da19b8bb4")))
    val invalidNextNewBlockHeader: BlockHeader = baseBlockHeader
      .copy(number = invalidBlockNumber, parentHash = newBlockHeader.hash, difficulty = newBlockDifficulty) //Wrong state root hash

    storagesInstance.storages.appStateStorage.putBestBlockNumber(maxBlockHeader.number)
    storagesInstance.storages.blockHeadersStorage.put(maxBlockHeader.hash, maxBlockHeader)
    storagesInstance.storages.blockNumberMappingStorage.put(maxBlockHeader.number, maxBlockHeader.hash)
    storagesInstance.storages.totalDifficultyStorage.put(maxBlockHeader.hash, maxBlocTotalDifficulty)

    storagesInstance.storages.appStateStorage.fastSyncDone()

    syncController ! SyncController.StartSync

    //Turn broadcasting on the RegularSync on by sending an empty BlockHeaders message:
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(expectedMaxBlock + 1), Config.Sync.blockHeadersPerRequest, 0, reverse = false),
      peer1.id))

    syncController.children.last ! MessageFromPeer(BlockHeaders(Seq()), peer1.id)

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))

    time.advance(Config.Sync.checkForNewBlockInterval)

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(expectedMaxBlock + 1), Config.Sync.blockHeadersPerRequest, 0, reverse = false),
      peer1.id))
    syncController.children.last ! MessageFromPeer(BlockHeaders(Seq(newBlockHeader, invalidNextNewBlockHeader)), peer1.id)
    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer1.id))))

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockBodies(Seq(newBlockHeader.hash, invalidNextNewBlockHeader.hash)), peer1.id))
    syncController.children.last ! MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil), BlockBody(Nil, Nil))), peer1.id)

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      NewBlock(Block(newBlockHeader, BlockBody(Nil, Nil)), maxBlocTotalDifficulty + newBlockDifficulty), peer1.id))
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      NewBlock(Block(newBlockHeader, BlockBody(Nil, Nil)), maxBlocTotalDifficulty + newBlockDifficulty), peer2.id))

    ommersPool.expectMsg(RemoveOmmers(newBlockHeader))
    pendingTransactionsManager.expectMsg(AddTransactions(Nil))
    pendingTransactionsManager.expectMsg(RemoveTransactions(Nil))

    ommersPool.expectNoMsg()
    pendingTransactionsManager.expectNoMsg()

  }

  val invalidBlock = 399501
  it should "only ask a peer once for BlockHeaders in case execution of a block were to fail" in new TestSetup(Seq(invalidBlock)) {
    val peer1TestProbe: TestProbe = TestProbe()(system)
    val peer2TestProbe: TestProbe = TestProbe()(system)

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref, false)
    val peer2 = Peer(new InetSocketAddress("127.0.0.2", 0), peer2TestProbe.ref, false)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status= Status(1, 1, totalDifficulty = 0, ByteString("peer2_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer1 -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0),
      peer2 -> PeerInfo(peer2Status, forkAccepted = true, totalDifficulty = peer2Status.totalDifficulty, maxBlockNumber = 0)
    )))

    val expectedMaxBlock = 399500
    val newBlockDifficulty = 23
    val maxBlocTotalDifficulty = 12340
    val maxBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedMaxBlock)
    val newBlockHeader: BlockHeader = baseBlockHeader
      .copy(number = invalidBlock, parentHash = maxBlockHeader.hash, difficulty = newBlockDifficulty)

    storagesInstance.storages.appStateStorage.putBestBlockNumber(maxBlockHeader.number)
    storagesInstance.storages.blockHeadersStorage.put(maxBlockHeader.hash, maxBlockHeader)
    storagesInstance.storages.blockNumberMappingStorage.put(maxBlockHeader.number, maxBlockHeader.hash)
    storagesInstance.storages.totalDifficultyStorage.put(maxBlockHeader.hash, maxBlocTotalDifficulty)

    storagesInstance.storages.appStateStorage.fastSyncDone()

    syncController ! SyncController.StartSync

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(expectedMaxBlock + 1), Config.Sync.blockHeadersPerRequest, 0, reverse = false), peer1.id))
    syncController.children.last ! MessageFromPeer(BlockHeaders(Seq(newBlockHeader)), peer1.id)

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetBlockBodies(Seq(newBlockHeader.hash)), peer1.id))
    syncController.children.last ! MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil))), peer1.id)

    peerMessageBus.expectMsgAllOf(Timeouts.normalTimeout,
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))),
      Unsubscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer1.id))))

    //As block execution failed for a block received from peer1, the same block is asked to peer2
    etcPeerManager.expectMsg(Timeouts.longTimeout, EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(expectedMaxBlock + 1), Config.Sync.blockHeadersPerRequest, 0, reverse = false),
      peer2.id))

    //No other message should be received as no response was sent to peer2
    peerMessageBus.expectNoMsg()
    etcPeerManager.expectNoMsg()

    pendingTransactionsManager.expectMsg(AddTransactions(Nil))

    ommersPool.expectNoMsg()
    pendingTransactionsManager.expectNoMsg()
  }

  it should "accept mined blocks" in new TestSetup() {
    val peerTestProbe: TestProbe = TestProbe()(system)

    val peer = Peer(new InetSocketAddress("127.0.0.1", 0), peerTestProbe.ref, false)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0))))

    val expectedMaxBlock = 399500
    val newBlockDifficulty = 23
    val maxBlocTotalDifficulty = 12340
    val maxBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedMaxBlock)
    val minedBlockHeader: BlockHeader = baseBlockHeader
      .copy(number = expectedMaxBlock + 1, parentHash = maxBlockHeader.hash, difficulty = newBlockDifficulty,
        stateRoot = ByteString(Hex.decode("d0aedc3838a3d7f9a526bdd642b55fb1b6292596985cfab2eedb751da19b8bb4")))

    storagesInstance.storages.appStateStorage.putBestBlockNumber(maxBlockHeader.number)
    storagesInstance.storages.blockHeadersStorage.put(maxBlockHeader.hash, maxBlockHeader)
    storagesInstance.storages.blockNumberMappingStorage.put(maxBlockHeader.number, maxBlockHeader.hash)
    storagesInstance.storages.totalDifficultyStorage.put(maxBlockHeader.hash, maxBlocTotalDifficulty)

    storagesInstance.storages.appStateStorage.fastSyncDone()

    syncController ! SyncController.StartSync

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(expectedMaxBlock + 1), Config.Sync.blockHeadersPerRequest, 0, reverse = false),
      peer.id))
    syncController.children.last ! MessageFromPeer(BlockHeaders(Seq()), peer.id)

    //wait for empty headers processing
    Thread.sleep(Timeouts.shortTimeout.toMillis)
    syncController ! MinedBlock(Block(minedBlockHeader,BlockBody(Nil,Nil)))

    //wait for actor to insert data
    Thread.sleep(Timeouts.normalTimeout.toMillis)
    blockchain.getBlockByNumber(expectedMaxBlock + 1) shouldBe Some(Block(minedBlockHeader, BlockBody(Nil, Nil)))
    blockchain.getTotalDifficultyByHash(minedBlockHeader.hash) shouldBe Some(maxBlocTotalDifficulty + minedBlockHeader.difficulty)

    ommersPool.expectMsg(RemoveOmmers(minedBlockHeader))
    ommersPool.expectNoMsg()
    pendingTransactionsManager.expectMsg(RemoveTransactions(Nil))
    pendingTransactionsManager.expectNoMsg()
  }

  it should "accept mined blocks after request timed-out" in new TestSetup() {
    val peerTestProbe: TestProbe = TestProbe()(system)

    val peer = Peer(new InetSocketAddress("127.0.0.1", 0), peerTestProbe.ref, false)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0))))

    val expectedMaxBlock = 399500
    val newBlockDifficulty = 23
    val maxBlocTotalDifficulty = 12340
    val maxBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedMaxBlock)
    val minedBlockHeader: BlockHeader = baseBlockHeader
      .copy(number = expectedMaxBlock + 1, parentHash = maxBlockHeader.hash, difficulty = newBlockDifficulty,
        stateRoot = ByteString(Hex.decode("d0aedc3838a3d7f9a526bdd642b55fb1b6292596985cfab2eedb751da19b8bb4")))

    storagesInstance.storages.appStateStorage.putBestBlockNumber(maxBlockHeader.number)
    storagesInstance.storages.blockHeadersStorage.put(maxBlockHeader.hash, maxBlockHeader)
    storagesInstance.storages.blockNumberMappingStorage.put(maxBlockHeader.number, maxBlockHeader.hash)
    storagesInstance.storages.totalDifficultyStorage.put(maxBlockHeader.hash, maxBlocTotalDifficulty)

    storagesInstance.storages.appStateStorage.fastSyncDone()

    peerMessageBus.ignoreNoMsg()

    syncController ! SyncController.StartSync

    //Send block headers request
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(expectedMaxBlock + 1), Config.Sync.blockHeadersPerRequest, 0, reverse = false),
      peer.id))
    peerMessageBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))

    //wait for timeout
    time.advance(2 * Config.Sync.peerResponseTimeout)
    peerMessageBus.expectMsg(Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id))))

    //wait for Done msg processing
    Thread.sleep(1.seconds.toMillis)
    syncController ! MinedBlock(Block(minedBlockHeader,BlockBody(Nil,Nil)))

    //wait for actor to insert data
    Thread.sleep(3.seconds.toMillis)

    //Check that the mined block was inserted into the blockchain
    blockchain.getBlockByNumber(expectedMaxBlock + 1) shouldBe Some(Block(minedBlockHeader, BlockBody(Nil, Nil)))
    blockchain.getTotalDifficultyByHash(minedBlockHeader.hash) shouldBe Some(maxBlocTotalDifficulty + minedBlockHeader.difficulty)

    ommersPool.expectMsg(RemoveOmmers(minedBlockHeader))
    ommersPool.expectNoMsg()
    pendingTransactionsManager.expectMsg(RemoveTransactions(Nil))
    pendingTransactionsManager.expectNoMsg()
  }

  it should "accept add mined blocks as ommers when doing sync" in new TestSetup() {
    val peerTestProbe: TestProbe = TestProbe()(system)

    val peer = Peer(new InetSocketAddress("127.0.0.1", 0), peerTestProbe.ref, false)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    etcPeerManager.send(syncController, HandshakedPeers(Map(
      peer -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0))))

    val expectedMaxBlock = 399500
    val newBlockDifficulty = 23
    val maxBlocTotalDifficulty = 12340
    val maxBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedMaxBlock)
    val minedBlockHeader: BlockHeader = baseBlockHeader
      .copy(number = expectedMaxBlock + 1, parentHash = maxBlockHeader.hash, difficulty = newBlockDifficulty,
        stateRoot = ByteString(Hex.decode("d0aedc3838a3d7f9a526bdd642b55fb1b6292596985cfab2eedb751da19b8bb4")))

    storagesInstance.storages.appStateStorage.putBestBlockNumber(maxBlockHeader.number)
    storagesInstance.storages.blockHeadersStorage.put(maxBlockHeader.hash, maxBlockHeader)
    storagesInstance.storages.blockNumberMappingStorage.put(maxBlockHeader.number, maxBlockHeader.hash)
    storagesInstance.storages.totalDifficultyStorage.put(maxBlockHeader.hash, maxBlocTotalDifficulty)

    storagesInstance.storages.appStateStorage.fastSyncDone()

    syncController ! SyncController.StartSync

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(expectedMaxBlock + 1), Config.Sync.blockHeadersPerRequest, 0, reverse = false), peer.id))

    syncController ! MinedBlock(Block(minedBlockHeader,BlockBody(Nil,Nil)))
    blockchain.getBlockByHash(minedBlockHeader.hash) shouldBe None
    blockchain.getTotalDifficultyByHash(minedBlockHeader.hash) shouldBe None

    ommersPool.expectMsg(AddOmmers(minedBlockHeader))
    ommersPool.expectNoMsg()
    pendingTransactionsManager.expectNoMsg()
  }

  it should "start fast sync after restart, if fast sync was partially ran and then regular sync started" in new TestSetup with MockFactory {
    val peerTestProbe: TestProbe = TestProbe()(system)
    val peer = Peer(new InetSocketAddress("127.0.0.1", 0), peerTestProbe.ref, false)
    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    //Save previous incomplete attempt to fast sync
    val syncState = SyncState(targetBlock = Fixtures.Blocks.Block3125369.header, mptNodesQueue = Seq(StateMptNodeHash(ByteString("node_hash"))))
    storagesInstance.storages.fastSyncStateStorage.putSyncState(syncState)

    //Attempt to start regular sync
    val syncConfigWithRegularSync = syncConfig(enableFastSync = false)
    val syncControllerWithRegularSync = TestActorRef(Props(new SyncController(
      storagesInstance.storages.appStateStorage,
      blockchain,
      storagesInstance.storages,
      storagesInstance.storages.fastSyncStateStorage,
      ledger,
      new Mocks.MockValidatorsAlwaysSucceed,
      peerMessageBus.ref, pendingTransactionsManager.ref, ommersPool.ref, etcPeerManager.ref,
      syncConfigWithRegularSync,
      externalSchedulerOpt = Some(time.scheduler))))

    etcPeerManager.send(syncControllerWithRegularSync, HandshakedPeers(Map(
      peer -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0))))

    syncControllerWithRegularSync ! SyncController.StartSync

    //Fast sync node request should be received
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetNodeData(Seq(ByteString("node_hash"))), peer.id))
  }

  class TestSetup(blocksForWhichLedgerFails: Seq[BigInt] = Nil) extends EphemBlockchainTestSetup {
    implicit val system = ActorSystem("FastSyncControllerSpec_System")

    val time = new VirtualTime
    val etcPeerManager = TestProbe()
    etcPeerManager.ignoreMsg{ case GetHandshakedPeers => true }

    val ledger: Ledger = new Mocks.MockLedger((block, _, _) => !blocksForWhichLedgerFails.contains(block.header.number))

    val peerMessageBus = TestProbe()
    peerMessageBus.ignoreMsg{
      case Subscribe(PeerDisconnectedClassifier(_)) => true
      case Unsubscribe(Some(PeerDisconnectedClassifier(_))) => true
    }
    val pendingTransactionsManager = TestProbe()
    val ommersPool = TestProbe()

    def syncConfig(enableFastSync: Boolean): SyncConfig = new SyncConfig {
      override val doFastSync: Boolean = enableFastSync

      //unchanged
      override val blockBodiesPerRequest: Int = Config.Sync.blockBodiesPerRequest
      override val blacklistDuration: FiniteDuration = Config.Sync.blacklistDuration
      override val peersScanInterval: FiniteDuration = Config.Sync.peersScanInterval
      override val blockResolveDepth: Int = Config.Sync.blockResolveDepth
      override val printStatusInterval: FiniteDuration = Config.Sync.printStatusInterval
      override val targetBlockOffset: Int = Config.Sync.targetBlockOffset
      override val syncRetryInterval: FiniteDuration = Config.Sync.syncRetryInterval
      override val peerResponseTimeout: FiniteDuration = Config.Sync.peerResponseTimeout
      override val maxConcurrentRequests: Int = Config.Sync.maxConcurrentRequests
      override val startRetryInterval: FiniteDuration = Config.Sync.startRetryInterval
      override val receiptsPerRequest: Int = Config.Sync.receiptsPerRequest
      override val blockHeadersPerRequest: Int = Config.Sync.blockHeadersPerRequest
      override val minPeersToChooseTargetBlock: Int = Config.Sync.minPeersToChooseTargetBlock
      override val checkForNewBlockInterval: FiniteDuration = Config.Sync.checkForNewBlockInterval
      override val blockChainOnlyPeersPoolSize: Int = Config.Sync.blockChainOnlyPeersPoolSize
      override val persistStateSnapshotInterval: FiniteDuration = Config.Sync.persistStateSnapshotInterval
      override val nodesPerRequest: Int = Config.Sync.nodesPerRequest
    }

    val syncController = TestActorRef(Props(new SyncController(
      storagesInstance.storages.appStateStorage,
      blockchain,
      storagesInstance.storages,
      storagesInstance.storages.fastSyncStateStorage,
      ledger,
      new Mocks.MockValidatorsAlwaysSucceed,
      peerMessageBus.ref, pendingTransactionsManager.ref, ommersPool.ref, etcPeerManager.ref,
      syncConfig(Config.Sync.doFastSync),
      externalSchedulerOpt = Some(time.scheduler))))

    val EmptyTrieRootHash: ByteString = Account.EmptyStorageRootHash
    val baseBlockHeader = BlockHeader(
      parentHash = ByteString("unused"),
      ommersHash = ByteString("unused"),
      beneficiary = ByteString("unused"),
      stateRoot = EmptyTrieRootHash,
      transactionsRoot = EmptyTrieRootHash,
      receiptsRoot = EmptyTrieRootHash,
      logsBloom = BloomFilter.EmptyBloomFilter,
      difficulty = 0,
      number = 0,
      gasLimit = 0,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = ByteString("unused"),
      mixHash = ByteString("unused"),
      nonce = ByteString("unused"))

    blockchain.save(baseBlockHeader.parentHash, BigInt(0))
  }

}
