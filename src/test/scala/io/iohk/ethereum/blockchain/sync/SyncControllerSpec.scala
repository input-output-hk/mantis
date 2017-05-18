package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.{Fixtures, Mocks}
import io.iohk.ethereum.blockchain.sync.FastSync.{StateMptNodeHash, SyncState}
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.domain.{Account, Block, BlockHeader, BlockchainStorages}
import io.iohk.ethereum.ledger.BlockExecutionError.{TxsExecutionError, ValidationBeforeExecError}
import io.iohk.ethereum.ledger.{BlockExecutionError, BloomFilter, Ledger}
import io.iohk.ethereum.network.{Peer, PeerActor, PeerMessageBusActor}
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peers}
import io.iohk.ethereum.network.PeerMessageBusActor._
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, _}
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, GetReceipts, NodeData, Receipts}
import io.iohk.ethereum.validators._
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

// scalastyle:off
class SyncControllerSpec extends FlatSpec with Matchers {

  "FastSyncController" should "download target block and request state nodes" in new TestSetup() {

    val peer1TestProbe: TestProbe = TestProbe("peer1")(system)
    val peer2TestProbe: TestProbe = TestProbe("peer2")(system)

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref)
    val peer2 = Peer(new InetSocketAddress("127.0.0.2", 0), peer2TestProbe.ref)

    time.advance(1.seconds)

    val peer1Status = Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status = Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      peer1 -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty),
      peer2 -> PeerActor.Status.Handshaked(peer2Status, true, peer1Status.totalDifficulty))))

    fastSyncController ! SyncController.StartSync

    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    peer1TestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(ByteString("peer1_bestHash")), 1, 0, reverse = false)))
    fastSyncController ! MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000))), peer1.id)

    peer2TestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(ByteString("peer2_bestHash")), 1, 0, reverse = false)))
    fastSyncController ! MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 400000))), peer2.id)

    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    val expectedTargetBlock = 399500

    peer1TestProbe.expectNoMsg()

    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))
    peer2TestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedTargetBlock), 1, 0, reverse = false)))
    fastSyncController ! MessageFromPeer(BlockHeaders(Seq(targetBlockHeader)), peer2.id)

    storagesInstance.storages.appStateStorage.putBestBlockNumber(targetBlockHeader.number)

    peer1.ref ! PoisonPill

    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    peer1TestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(1), 10, 0, false)))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))

    peer2TestProbe.expectMsg(PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))
  }

  it should "download target block, request state, blocks and finish when downloaded" in new TestSetup() {
    val peer2TestProbe: TestProbe = TestProbe()(system)
    val peer2 = Peer(new InetSocketAddress("127.0.0.1", 0), peer2TestProbe.ref)

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

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      peer2 -> PeerActor.Status.Handshaked(peer2Status, true, peer2Status.totalDifficulty))))

    fastSyncController ! SyncController.StartSync

    peer2TestProbe.expectMsg(
      PeerActor.SendMessage(GetBlockHeaders(Left(targetBlockHeader.number), expectedTargetBlock - bestBlockHeaderNumber, 0, reverse = false)))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq(targetBlockHeader)), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    peer2TestProbe.expectMsg(
      PeerActor.SendMessage(GetReceipts(Seq(targetBlockHeader.hash))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(Receipts(Seq(Nil)), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer2.id))))

    peer2TestProbe.expectMsg(
      PeerActor.SendMessage(GetBlockBodies(Seq(targetBlockHeader.hash))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil))), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer2.id))))

    val stateMptLeafWithAccount =
      ByteString(Hex.decode("f86d9e328415c225a782bb339b22acad1c739e42277bc7ef34de3623114997ce78b84cf84a0186cb7d8738d800a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

    val watcher = TestProbe()
    watcher.watch(fastSyncController)

    peer2TestProbe.expectMsg(
      PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(NodeData(Seq(stateMptLeafWithAccount)), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))

    //switch to regular download
    peer2TestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(targetBlockHeader.number + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))
  }

  it should "not use (blacklist) a peer that fails to respond within time limit" in new TestSetup() {
    val peer2TestProbe: TestProbe = TestProbe()(system)

    val peer2 = Peer(new InetSocketAddress("127.0.0.1", 0), peer2TestProbe.ref)

    time.advance(1.seconds)

    val peer2Status = Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      peer2 -> PeerActor.Status.Handshaked(peer2Status, true, peer2Status.totalDifficulty))))

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)

    storagesInstance.storages.fastSyncStateStorage.putSyncState(SyncState(targetBlockHeader)
      .copy(bestBlockHeaderNumber = targetBlockHeader.number,
        mptNodesQueue = Seq(StateMptNodeHash(targetBlockHeader.stateRoot))))

    fastSyncController ! SyncController.StartSync

    peer2TestProbe.expectMsg(PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))

    // response timeout
    time.advance(2.seconds)
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))
    peer2TestProbe.expectNoMsg()

    // wait for blacklist timeout
    time.advance(6.seconds)
    peer2TestProbe.expectNoMsg()

    // wait for next sync retry
    time.advance(3.seconds)

    // peer should not be blacklisted anymore
    peer2TestProbe.expectMsg(PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))
  }

  it should "start regular download " in new TestSetup() {
    val peerTestProbe: TestProbe = TestProbe()(system)

    val peer = Peer(new InetSocketAddress("127.0.0.1", 0), peerTestProbe.ref)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      peer -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty))))

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

    fastSyncController ! SyncController.StartSync

    peerTestProbe.ignoreMsg { case u => u == Unsubscribe }

    peerTestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq(newBlockHeader)), peer.id))

    peerTestProbe.expectMsg(PeerActor.SendMessage(GetBlockBodies(Seq(newBlockHeader.hash))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))))
    peerMessageBus.reply(MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil))), peer.id))

    peerTestProbe.expectMsg(10.seconds,
      PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 2), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))),
      Unsubscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id)))
    )

    blockchain.getBlockByNumber(expectedMaxBlock + 1) shouldBe Some(Block(newBlockHeader, BlockBody(Nil, Nil)))
    blockchain.getTotalDifficultyByHash(newBlockHeader.hash) shouldBe Some(maxBlocTotalDifficulty + newBlockHeader.difficulty)
  }

  it should "resolve branch conflict" in new TestSetup() {
    val peerTestProbe: TestProbe = TestProbe()(system)
    val peer = Peer(new InetSocketAddress("127.0.0.1", 0), peerTestProbe.ref)

    time.advance(1.seconds)

    val peer1Status = Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      peer -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty))))

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

    fastSyncController ! SyncController.StartSync

    peerTestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq(newBlockHeader)), peer.id))

    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq(newBlockHeaderParent)), peer.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))

    peerTestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(newBlockHeader.parentHash), Config.FastSync.blockResolveDepth, 0, reverse = true)))

    peerTestProbe.expectMsg(PeerActor.SendMessage(GetBlockBodies(Seq(newBlockHeaderParent.hash, newBlockHeader.hash))))
    peerMessageBus.expectMsgAllOf(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))))
    peerMessageBus.reply(MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil), BlockBody(Nil, Nil))), peer.id))

    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq(nextNewBlockHeader)), peer.id))

    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))))

    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer.id))))
    peerMessageBus.reply(MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil))), peer.id))

    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))

    //start next download cycle

    peerTestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 2), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peerTestProbe.expectMsg(PeerActor.SendMessage(GetBlockBodies(Seq(nextNewBlockHeader.hash))))

    //wait for actor to insert data
    Thread.sleep(3.seconds.toMillis)

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
  }

  it should "only use ETC peer to choose target block" in new TestSetup() {
    val peer1TestProbe: TestProbe = TestProbe()(system)
    val peer2TestProbe: TestProbe = TestProbe()(system)
    val peer3TestProbe: TestProbe = TestProbe()(system)
    val peer4TestProbe: TestProbe = TestProbe()(system)

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref)
    val peer2 = Peer(new InetSocketAddress("127.0.0.2", 0), peer2TestProbe.ref)
    val peer3 = Peer(new InetSocketAddress("127.0.0.3", 0), peer3TestProbe.ref)
    val peer4 = Peer(new InetSocketAddress("127.0.0.4", 0), peer4TestProbe.ref)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status= Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))
    val peer3Status= Status(1, 1, 1, ByteString("peer3_bestHash"), ByteString("unused"))
    val peer4Status= Status(1, 1, 1, ByteString("peer4_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      peer1 -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty),
      peer2 -> PeerActor.Status.Handshaked(peer2Status, false, peer1Status.totalDifficulty),
      peer3 -> PeerActor.Status.Handshaked(peer3Status, false, peer1Status.totalDifficulty),
      peer4 -> PeerActor.Status.Handshaked(peer4Status, true, peer1Status.totalDifficulty))))

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)
    storagesInstance.storages.appStateStorage.putBestBlockNumber(targetBlockHeader.number)

    fastSyncController ! SyncController.StartSync

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer4.id))))

    peer1TestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(ByteString("peer1_bestHash")), 1, 0, reverse = false)))
    fastSyncController ! MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000))), peer1.id)

    peer2TestProbe.expectNoMsg()
    peer3TestProbe.expectNoMsg()

    peer4TestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(ByteString("peer4_bestHash")), 1, 0, reverse = false)))
    fastSyncController ! MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000))), peer4.id)

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer4.id))),
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
  }

  it should "broadcast all blocks if they were all valid" in new TestSetup() {
    val peer1TestProbe: TestProbe = TestProbe()(system)

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      peer1 -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty)
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

    fastSyncController ! SyncController.StartSync

    //Turn broadcasting on the RegularSync on by sending an empty BlockHeaders message:
    peer1TestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))

    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq()), peer1.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))

    time.advance(Config.FastSync.checkForNewBlockInterval)

    peer1TestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))

    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq(newBlockHeader, nextNewBlockHeader)), peer1.id))

    peer1TestProbe.expectMsg(PeerActor.SendMessage(GetBlockBodies(Seq(newBlockHeader.hash, nextNewBlockHeader.hash))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer1.id))))

    peerMessageBus.reply(MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil), BlockBody(Nil, Nil))), peer1.id))

    //TODO: investigate why such a long timeout is required
    peer1TestProbe.expectMsgAllOf(20.seconds,
      PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 3), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)),
      PeerActor.BroadcastBlocks(Seq(
        NewBlock(Block(newBlockHeader, BlockBody(Nil, Nil)), maxBlocTotalDifficulty + newBlockDifficulty),
        NewBlock(Block(nextNewBlockHeader, BlockBody(Nil, Nil)), maxBlocTotalDifficulty + 2 * newBlockDifficulty)
      ))
    )
  }

  val invalidBlockNumber = 399502

  it should "only broadcast blocks that it was able to successfully execute" in new TestSetup(Seq(invalidBlockNumber)) {

    val peer1TestProbe: TestProbe = TestProbe()(system)
    val peer2TestProbe: TestProbe = TestProbe()(system)

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref)
    val peer2 = Peer(new InetSocketAddress("127.0.0.2", 0), peer2TestProbe.ref)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status= Status(1, 1, totalDifficulty = 0, ByteString("peer2_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      peer1 -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty),
      peer2 -> PeerActor.Status.Handshaked(peer2Status, true, peer2Status.totalDifficulty)
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

    fastSyncController ! SyncController.StartSync

    //Turn broadcasting on the RegularSync on by sending an empty BlockHeaders message:
    peer1TestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq()), peer1.id))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
    time.advance(Config.FastSync.checkForNewBlockInterval)

    peer1TestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
    peer1TestProbe.reply(MessageFromPeer(BlockHeaders(Seq(newBlockHeader, invalidNextNewBlockHeader)), peer1.id))

    peer1TestProbe.expectMsg(PeerActor.SendMessage(GetBlockBodies(Seq(newBlockHeader.hash, invalidNextNewBlockHeader.hash))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer1.id))))
    peerMessageBus.reply(MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil), BlockBody(Nil, Nil))), peer1.id))

    //TODO: investigate why such a long timeout is required
    peer2TestProbe.expectMsgAllOf(20.seconds,
      PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 2), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)),
      PeerActor.BroadcastBlocks(Seq(NewBlock(Block(newBlockHeader, BlockBody(Nil, Nil)), maxBlocTotalDifficulty + newBlockDifficulty)))
    )
  }

  val invalidBlock = 399501
  it should "should only ask a peer once for BlockHeaders in case execution of a block were to fail" in new TestSetup(Seq(invalidBlock)) {
    val peer1TestProbe: TestProbe = TestProbe()(system)
    val peer2TestProbe: TestProbe = TestProbe()(system)

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref)
    val peer2 = Peer(new InetSocketAddress("127.0.0.2", 0), peer2TestProbe.ref)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status= Status(1, 1, totalDifficulty = 0, ByteString("peer2_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      peer1 -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty),
      peer2 -> PeerActor.Status.Handshaked(peer2Status, true, peer2Status.totalDifficulty)
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

    fastSyncController ! SyncController.StartSync

    peer1TestProbe.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq(newBlockHeader)), peer1.id))

    peer1TestProbe.expectMsg(PeerActor.SendMessage(GetBlockBodies(Seq(newBlockHeader.hash))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer1.id))))
    peerMessageBus.reply(MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil))), peer1.id))

    peerMessageBus.expectMsgAllOf(Unsubscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))

    //As block execution failed for a block received from peer1, the same block is asked to peer2
    peer2TestProbe.expectMsgAllOf(10.seconds,
      PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))

    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    //No other message should be received as no response was sent to peer2
    peer2TestProbe.expectNoMsg()
  }

  class TestSetup(blocksForWhichLedgerFails: Seq[BigInt] = Nil) extends EphemBlockchainTestSetup {
    implicit val system = ActorSystem("FastSyncControllerSpec_System")

    val time = new VirtualTime
    val peerManager = TestProbe()

    val dataSource = EphemDataSource()

    val ledger: Ledger = new Mocks.MockLedger((block, _, _) => !blocksForWhichLedgerFails.contains(block.header.number))

    val peerMessageBus = TestProbe()(system)

    val fastSyncController = TestActorRef(Props(new SyncController(peerManager.ref,
      storagesInstance.storages.appStateStorage,
      blockchain,
      storagesInstance.storages,
      storagesInstance.storages.fastSyncStateStorage,
      ledger,
      new Mocks.MockValidatorsAlwaysSucceed,
      peerMessageBus.ref,
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
