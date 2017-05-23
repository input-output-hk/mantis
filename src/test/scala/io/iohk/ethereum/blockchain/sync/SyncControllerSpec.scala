package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.actor.{ActorSystem, PoisonPill, Props}
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.blockchain.sync.FastSync.{StateMptNodeHash, SyncState}
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.domain.{Account, Block, BlockHeader}
import io.iohk.ethereum.ledger.{BloomFilter, Ledger}
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.network.PeerActor.Unsubscribe
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peer, Peers}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, _}
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, GetReceipts, NodeData, Receipts}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.duration._

class SyncControllerSpec extends FlatSpec with Matchers {

  "FastSyncController" should "download target block and request state nodes" in new TestSetup() {

    val peer1: TestProbe = TestProbe()(system)
    val peer2: TestProbe = TestProbe()(system)

    time.advance(1.seconds)

    val peer1Status = Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status = Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      Peer(new InetSocketAddress("127.0.0.1", 0), peer1.ref) -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty),
      Peer(new InetSocketAddress("127.0.0.1", 0), peer2.ref) -> PeerActor.Status.Handshaked(peer2Status, true, peer1Status.totalDifficulty))))

    syncController ! SyncController.StartSync

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

    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)
    peer2.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer2.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedTargetBlock), 1, 0, reverse = false)))
    peer2.reply(PeerActor.MessageReceived(BlockHeaders(Seq(targetBlockHeader))))

    storagesInstance.storages.appStateStorage.putBestBlockNumber(targetBlockHeader.number)

    peer1.ref ! PoisonPill

    peer2.expectMsg(PeerActor.Unsubscribe)

    peer2.expectMsg(PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peer2.expectMsg(PeerActor.Subscribe(Set(NodeData.code)))
  }

  it should "download target block, request state, blocks and finish when downloaded" in new TestSetup() {
    val peer2: TestProbe = TestProbe()(system)

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
      Peer(new InetSocketAddress("127.0.0.1", 0), peer2.ref) -> PeerActor.Status.Handshaked(peer2Status, true, peer2Status.totalDifficulty))))

    syncController ! SyncController.StartSync

    peer2.expectMsgAllOf(
      PeerActor.SendMessage(GetBlockHeaders(Left(targetBlockHeader.number), expectedTargetBlock - bestBlockHeaderNumber, 0, reverse = false)),
      PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer2.reply(PeerActor.MessageReceived(BlockHeaders(Seq(targetBlockHeader))))
    peer2.expectMsg(PeerActor.Unsubscribe)

    peer2.expectMsgAllOf(
      PeerActor.SendMessage(GetReceipts(Seq(targetBlockHeader.hash))),
      PeerActor.Subscribe(Set(Receipts.code)))
    peer2.reply(PeerActor.MessageReceived(Receipts(Seq(Nil))))
    peer2.expectMsg(PeerActor.Unsubscribe)

    peer2.expectMsgAllOf(
      PeerActor.SendMessage(GetBlockBodies(Seq(targetBlockHeader.hash))),
      PeerActor.Subscribe(Set(BlockBodies.code)))
    peer2.reply(PeerActor.MessageReceived(BlockBodies(Seq(BlockBody(Nil, Nil)))))
    peer2.expectMsg(PeerActor.Unsubscribe)

    val stateMptLeafWithAccount =
      ByteString(Hex.decode("f86d9e328415c225a782bb339b22acad1c739e42277bc7ef34de3623114997ce78b84cf84a0186cb7d8738d800a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

    val watcher = TestProbe()
    watcher.watch(syncController)

    peer2.expectMsgAllOf(
      PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))),
      PeerActor.Subscribe(Set(NodeData.code)))
    peer2.reply(PeerActor.MessageReceived(NodeData(Seq(stateMptLeafWithAccount))))
    peer2.expectMsg(PeerActor.Unsubscribe)

    //switch to regular download
    peer2.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(targetBlockHeader.number + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peer2.expectMsgAllOf(PeerActor.Subscribe(Set(BlockHeaders.code)))
  }

  it should "not use (blacklist) a peer that fails to respond within time limit" in new TestSetup() {
    val peer2: TestProbe = TestProbe()(system)

    time.advance(1.seconds)

    val peer2Status = Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      Peer(new InetSocketAddress("127.0.0.1", 0), peer2.ref) -> PeerActor.Status.Handshaked(peer2Status, true, peer2Status.totalDifficulty))))

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)

    storagesInstance.storages.fastSyncStateStorage.putSyncState(SyncState(targetBlockHeader)
      .copy(bestBlockHeaderNumber = targetBlockHeader.number,
        mptNodesQueue = Seq(StateMptNodeHash(targetBlockHeader.stateRoot))))

    syncController ! SyncController.StartSync

    peer2.expectMsg(PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peer2.expectMsg(PeerActor.Subscribe(Set(NodeData.code)))

    // response timeout
    time.advance(2.seconds)
    peer2.expectMsg(PeerActor.Unsubscribe)
    peer2.expectNoMsg()

    // wait for blacklist timeout
    time.advance(6.seconds)
    peer2.expectNoMsg()

    // wait for next sync retry
    time.advance(3.seconds)

    // peer should not be blacklisted anymore
    peer2.expectMsg(PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peer2.expectMsg(PeerActor.Subscribe(Set(NodeData.code)))
  }

  it should "start regular download " in new TestSetup() {
    val peer: TestProbe = TestProbe()(system)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      Peer(new InetSocketAddress("127.0.0.1", 0), peer.ref) -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty))))

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

    peer.ignoreMsg { case u => u == Unsubscribe }

    peer.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer.reply(PeerActor.MessageReceived(BlockHeaders(Seq(newBlockHeader))))

    peer.expectMsg(PeerActor.SendMessage(GetBlockBodies(Seq(newBlockHeader.hash))))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockBodies.code)))
    peer.reply(PeerActor.MessageReceived(BlockBodies(Seq(BlockBody(Nil, Nil)))))

    peer.expectMsgAllOf(10.seconds,
      PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 2), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)),
      PeerActor.Subscribe(Set(BlockHeaders.code)),
      PeerActor.BroadcastBlocks(Seq(NewBlock(Block(newBlockHeader, BlockBody(Nil, Nil)), maxBlocTotalDifficulty + newBlockDifficulty))))

    blockchain.getBlockByNumber(expectedMaxBlock + 1) shouldBe Some(Block(newBlockHeader, BlockBody(Nil, Nil)))
    blockchain.getTotalDifficultyByHash(newBlockHeader.hash) shouldBe Some(maxBlocTotalDifficulty + newBlockHeader.difficulty)
  }

  it should "resolve branch conflict" in new TestSetup() {
    val peer: TestProbe = TestProbe()(system)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      Peer(new InetSocketAddress("127.0.0.1", 0), peer.ref) -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty))))

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

    peer.ignoreMsg { case u => u == Unsubscribe }

    peer.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer.reply(PeerActor.MessageReceived(BlockHeaders(Seq(newBlockHeader))))

    peer.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(newBlockHeader.parentHash), Config.FastSync.blockResolveDepth, 0, reverse = true)))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer.reply(PeerActor.MessageReceived(BlockHeaders(Seq(newBlockHeaderParent))))

    peer.expectMsg(PeerActor.SendMessage(GetBlockBodies(Seq(newBlockHeaderParent.hash, newBlockHeader.hash))))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockBodies.code)))
    peer.reply(PeerActor.MessageReceived(BlockBodies(Seq(BlockBody(Nil, Nil), BlockBody(Nil, Nil)))))

    //start next download cycle

    peer.expectMsgAllOf(
      PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 2), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)),
      PeerActor.BroadcastBlocks(Seq(
        NewBlock(Block(newBlockHeaderParent, BlockBody(Nil, Nil)), commonRootTotalDifficulty + newBlockDifficulty),
        NewBlock(Block(newBlockHeader, BlockBody(Nil, Nil)), commonRootTotalDifficulty + 2 * newBlockDifficulty)
      )))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer.reply(PeerActor.MessageReceived(BlockHeaders(Seq(nextNewBlockHeader))))
    peer.expectMsg(PeerActor.SendMessage(GetBlockBodies(Seq(nextNewBlockHeader.hash))))
    peer.expectMsg(PeerActor.Subscribe(Set(BlockBodies.code)))
    peer.reply(PeerActor.MessageReceived(BlockBodies(Seq(BlockBody(Nil, Nil)))))

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
    val peer1: TestProbe = TestProbe()(system)
    val peer2: TestProbe = TestProbe()(system)
    val peer3: TestProbe = TestProbe()(system)
    val peer4: TestProbe = TestProbe()(system)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status= Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))
    val peer3Status= Status(1, 1, 1, ByteString("peer3_bestHash"), ByteString("unused"))
    val peer4Status= Status(1, 1, 1, ByteString("peer4_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      Peer(new InetSocketAddress("127.0.0.1", 0), peer1.ref) -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty),
      Peer(new InetSocketAddress("127.0.0.1", 0), peer2.ref) -> PeerActor.Status.Handshaked(peer2Status, false, peer1Status.totalDifficulty),
      Peer(new InetSocketAddress("127.0.0.1", 0), peer3.ref) -> PeerActor.Status.Handshaked(peer3Status, false, peer1Status.totalDifficulty),
      Peer(new InetSocketAddress("127.0.0.1", 0), peer4.ref) -> PeerActor.Status.Handshaked(peer4Status, true, peer1Status.totalDifficulty))))

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)
    storagesInstance.storages.appStateStorage.putBestBlockNumber(targetBlockHeader.number)

    syncController ! SyncController.StartSync

    peer1.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer1.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(ByteString("peer1_bestHash")), 1, 0, reverse = false)))
    peer1.reply(PeerActor.MessageReceived(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000)))))
    peer1.expectMsg(PeerActor.Unsubscribe)

    peer2.expectNoMsg()
    peer3.expectNoMsg()

    peer4.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer4.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Right(ByteString("peer4_bestHash")), 1, 0, reverse = false)))
    peer4.reply(PeerActor.MessageReceived(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000)))))
    peer4.expectMsg(PeerActor.Unsubscribe)
  }

  it should "broadcast all blocks if they were all valid" in new TestSetup() {
    val peer1: TestProbe = TestProbe()(system)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      Peer(new InetSocketAddress("127.0.0.1", 0), peer1.ref) -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty)
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
    peer1.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peer1.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer1.reply(PeerActor.MessageReceived(BlockHeaders(Seq())))
    peer1.expectMsg(Unsubscribe)
    time.advance(Config.FastSync.checkForNewBlockInterval)

    peer1.ignoreMsg { case u => u == Unsubscribe }

    peer1.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peer1.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer1.reply(PeerActor.MessageReceived(BlockHeaders(Seq(newBlockHeader, nextNewBlockHeader))))

    peer1.expectMsg(PeerActor.SendMessage(GetBlockBodies(Seq(newBlockHeader.hash, nextNewBlockHeader.hash))))
    peer1.expectMsg(PeerActor.Subscribe(Set(BlockBodies.code)))
    peer1.reply(PeerActor.MessageReceived(BlockBodies(Seq(BlockBody(Nil, Nil), BlockBody(Nil, Nil)))))

    //TODO: investigate why such a long timeout is required
    peer1.expectMsgAllOf(20.seconds,
      PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 3), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)),
      PeerActor.Subscribe(Set(BlockHeaders.code)),
      PeerActor.BroadcastBlocks(Seq(
        NewBlock(Block(newBlockHeader, BlockBody(Nil, Nil)), maxBlocTotalDifficulty + newBlockDifficulty),
        NewBlock(Block(nextNewBlockHeader, BlockBody(Nil, Nil)), maxBlocTotalDifficulty + 2 * newBlockDifficulty)
      ))
    )
  }

  val invalidBlockNumber = 399502

  it should "only broadcast blocks that it was able to successfully execute" in new TestSetup(Seq(invalidBlockNumber)) {

    val peer1: TestProbe = TestProbe()(system)
    val peer2: TestProbe = TestProbe()(system)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status= Status(1, 1, totalDifficulty = 0, ByteString("peer2_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      Peer(new InetSocketAddress("127.0.0.1", 0), peer1.ref) -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty),
      Peer(new InetSocketAddress("127.0.0.2", 0), peer2.ref) -> PeerActor.Status.Handshaked(peer2Status, true, peer2Status.totalDifficulty)
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
    peer1.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peer1.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer1.reply(PeerActor.MessageReceived(BlockHeaders(Seq())))
    peer1.expectMsg(Unsubscribe)
    time.advance(Config.FastSync.checkForNewBlockInterval)

    peer1.ignoreMsg { case u => u == Unsubscribe }

    peer1.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peer1.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer1.reply(PeerActor.MessageReceived(BlockHeaders(Seq(newBlockHeader, invalidNextNewBlockHeader))))

    peer1.expectMsg(PeerActor.SendMessage(GetBlockBodies(Seq(newBlockHeader.hash, invalidNextNewBlockHeader.hash))))
    peer1.expectMsg(PeerActor.Subscribe(Set(BlockBodies.code)))
    peer1.reply(PeerActor.MessageReceived(BlockBodies(Seq(BlockBody(Nil, Nil), BlockBody(Nil, Nil)))))

    //TODO: investigate why such a long timeout is required
    peer2.expectMsgAllOf(20.seconds,
      PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 2), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)),
      PeerActor.Subscribe(Set(BlockHeaders.code)),
      PeerActor.BroadcastBlocks(Seq(NewBlock(Block(newBlockHeader, BlockBody(Nil, Nil)), maxBlocTotalDifficulty + newBlockDifficulty)))
    )
  }

  val invalidBlock = 399501
  it should "should only ask a peer once for BlockHeaders in case execution of a block were to fail" in new TestSetup(Seq(invalidBlock)) {

    val peer1: TestProbe = TestProbe()(system)
    val peer2: TestProbe = TestProbe()(system)

    time.advance(1.seconds)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status= Status(1, 1, totalDifficulty = 0, ByteString("peer2_bestHash"), ByteString("unused"))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(Peers(Map(
      Peer(new InetSocketAddress("127.0.0.1", 0), peer1.ref) -> PeerActor.Status.Handshaked(peer1Status, true, peer1Status.totalDifficulty),
      Peer(new InetSocketAddress("127.0.0.2", 0), peer2.ref) -> PeerActor.Status.Handshaked(peer2Status, true, peer2Status.totalDifficulty)
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

    peer1.ignoreMsg { case u => u == Unsubscribe }

    peer1.expectMsg(PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)))
    peer1.expectMsg(PeerActor.Subscribe(Set(BlockHeaders.code)))
    peer1.reply(PeerActor.MessageReceived(BlockHeaders(Seq(newBlockHeader))))

    peer1.expectMsg(PeerActor.SendMessage(GetBlockBodies(Seq(newBlockHeader.hash))))
    peer1.expectMsg(PeerActor.Subscribe(Set(BlockBodies.code)))
    peer1.reply(PeerActor.MessageReceived(BlockBodies(Seq(BlockBody(Nil, Nil)))))

    //As block execution failed for a block received from peer1, the same block is asked to peer2
    peer2.expectMsgAllOf(10.seconds,
      PeerActor.SendMessage(GetBlockHeaders(Left(expectedMaxBlock + 1), Config.FastSync.blockHeadersPerRequest, 0, reverse = false)),
      PeerActor.Subscribe(Set(BlockHeaders.code)))

    //No other message should be received as no response was sent to peer2
    peer2.expectNoMsg()
  }

  class TestSetup(blocksForWhichLedgerFails: Seq[BigInt] = Nil) extends EphemBlockchainTestSetup {
    implicit val system = ActorSystem("FastSyncControllerSpec_System")

    val time = new VirtualTime
    val peerManager = TestProbe()

    val dataSource = EphemDataSource()

    val ledger: Ledger = new Mocks.MockLedger((block, _, _) => !blocksForWhichLedgerFails.contains(block.header.number))

    val syncController = TestActorRef(Props(new SyncController(peerManager.ref,
      storagesInstance.storages.appStateStorage,
      blockchain,
      storagesInstance.storages,
      storagesInstance.storages.fastSyncStateStorage,
      ledger,
      new Mocks.MockValidatorsAlwaysSucceed,
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
