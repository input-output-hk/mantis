package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSync.{StateMptNodeHash, SyncState}
import io.iohk.ethereum.consensus.TestConsensus
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderPoWError
import io.iohk.ethereum.consensus.validators.{BlockHeaderValidator, Validators}
import io.iohk.ethereum.domain.{Account, BlockHeader}
import io.iohk.ethereum.ledger.{BloomFilter, Ledger}
import io.iohk.ethereum.network.EtcPeerManagerActor.{HandshakedPeers, PeerInfo}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerDisconnectedClassifier}
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, _}
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, GetReceipts, NodeData, Receipts}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.vm.VM
import io.iohk.ethereum.{Fixtures, Mocks}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

import scala.collection.immutable.Set
import scala.concurrent.Await
import scala.concurrent.duration._

// scalastyle:off file.size.limit
class SyncControllerSpec extends FlatSpec with Matchers with BeforeAndAfter with MockFactory {

  implicit var system: ActorSystem = _

  before {
    system = ActorSystem("SyncControllerSpec_System")
  }

  after {
    Await.result(system.terminate(), 1.seconds)
  }

  "SyncController" should "download target block and request state nodes" in new TestSetup() {

    val peer1TestProbe: TestProbe = TestProbe("peer1")(system)
    val peer2TestProbe: TestProbe = TestProbe("peer2")(system)

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref, false)
    val peer2 = Peer(new InetSocketAddress("127.0.0.2", 0), peer2TestProbe.ref, false)

    syncController ! SyncController.Start

    Thread.sleep(startDelayMillis)

    val peer1Status = Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status = Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))

    val handshakedPeers = HandshakedPeers(Map(
      peer1 -> PeerInfo(peer1Status, peer1Status.totalDifficulty, forkAccepted = true, maxBlockNumber = 0),
      peer2 -> PeerInfo(peer2Status, peer1Status.totalDifficulty, forkAccepted = true, maxBlockNumber = 0)))

    etcPeerManager.send(syncController.getSingleChild("fast-sync").getChild(Seq("target-block-selector").toIterator), handshakedPeers)
    etcPeerManager.send(syncController.getSingleChild("fast-sync"), handshakedPeers)

    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Right(ByteString("peer1_bestHash")), 1, 0, reverse = false), peer1.id))
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000))), peer1.id))

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Right(ByteString("peer2_bestHash")), 1, 0, reverse = false), peer2.id))
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 400000))), peer2.id))

    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedTargetBlock), 1, 0, reverse = false), peer2.id))

    storagesInstance.storages.appStateStorage.putBestBlockNumber(targetBlockHeader.number)
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(targetBlockHeader)), peer2.id))

    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot)), peer1.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(1), 10, 0, reverse = false), peer2.id)
    )
    etcPeerManager.expectNoMsg()

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(),
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
        pendingMptNodes = Seq(StateMptNodeHash(targetBlockHeader.stateRoot))))

    Thread.sleep(1.seconds.toMillis)

    val peer2Status = Status(1, 1, 20, ByteString("peer2_bestHash"), ByteString("unused"))

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(Map(
      peer2 -> PeerInfo(peer2Status, forkAccepted = true, totalDifficulty = peer2Status.totalDifficulty, maxBlockNumber = 0)))

    etcPeerManager.send(syncController.getSingleChild("fast-sync").getChild(Seq("target-block-selector").toIterator), handshakedPeers)
    etcPeerManager.send(syncController.getSingleChild("fast-sync"), handshakedPeers)

    val stateMptLeafWithAccount =
      ByteString(Hex.decode("f86d9e328415c225a782bb339b22acad1c739e42277bc7ef34de3623114997ce78b84cf84a0186cb7d8738d800a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

    val watcher = TestProbe()
    watcher.watch(syncController)

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(NodeData(Seq(stateMptLeafWithAccount)), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe())

    //wait for peers throttle
    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    //trigger scheduling
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(targetBlockHeader.number), expectedTargetBlock - bestBlockHeaderNumber, 0, reverse = false),
      peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq(targetBlockHeader)), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe())

    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    Thread.sleep(2.second.toMillis)
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetReceipts(Seq(targetBlockHeader.hash)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(Receipts(Seq(Nil)), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe())

    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockBodies(Seq(targetBlockHeader.hash)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(BlockBodies(Seq(BlockBody(Nil, Nil))), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe())

    Thread.sleep(startDelayMillis)
    etcPeerManager.send(syncController.getSingleChild("regular-sync"), handshakedPeers)

    //switch to regular download
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(targetBlockHeader.number + 1), syncConfig.blockHeadersPerRequest, 0, reverse = false),
      peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))
  }

  it should "handle blocks that fail validation" in new TestSetup(_validators = new Mocks.MockValidatorsAlwaysSucceed {
    override val blockHeaderValidator: BlockHeaderValidator = { (blockHeader, getBlockHeaderByHash) => Left(HeaderPoWError) }
  }) {
    val peer2TestProbe: TestProbe = TestProbe()(system)
    val peer2 = Peer(new InetSocketAddress("127.0.0.1", 0), peer2TestProbe.ref, false)


    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(
      number = expectedTargetBlock,
      stateRoot = ByteString(Hex.decode("deae1dfad5ec8dcef15915811e1f044d2543674fd648f94345231da9fc2646cc")))
    val bestBlockHeaderNumber: BigInt = targetBlockHeader.number - 2
    storagesInstance.storages.fastSyncStateStorage.putSyncState(SyncState(targetBlockHeader)
      .copy(bestBlockHeaderNumber = bestBlockHeaderNumber,
        pendingMptNodes = Seq(StateMptNodeHash(targetBlockHeader.stateRoot))))

    Thread.sleep(1.seconds.toMillis)

    val peer2Status = Status(1, 1, 20, ByteString("peer2_bestHash"), ByteString("unused"))

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(Map(
      peer2 -> PeerInfo(peer2Status, forkAccepted = true, totalDifficulty = peer2Status.totalDifficulty, maxBlockNumber = 0)))

    etcPeerManager.send(syncController.getSingleChild("fast-sync").getChild(Seq("target-block-selector").toIterator), handshakedPeers)
    etcPeerManager.send(syncController.getSingleChild("fast-sync"), handshakedPeers)

    val stateMptLeafWithAccount =
      ByteString(Hex.decode("f86d9e328415c225a782bb339b22acad1c739e42277bc7ef34de3623114997ce78b84cf84a0186cb7d8738d800a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

    val watcher = TestProbe()
    watcher.watch(syncController)

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(NodeData(Seq(stateMptLeafWithAccount)), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe())

    //wait for peers throttle
    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    //trigger scheduling

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(targetBlockHeader.number - 1), expectedTargetBlock - bestBlockHeaderNumber, 0, reverse = false),
      peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq(targetBlockHeader.copy(number = targetBlockHeader.number - 1))), peer2.id))
    peerMessageBus.expectMsg(Unsubscribe())

    syncController.getSingleChild("fast-sync") ! FastSync.PersistSyncState
    Thread.sleep(200)
    val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get
    syncState.bestBlockHeaderNumber shouldBe (bestBlockHeaderNumber - syncConfig.fastSyncBlockValidationN)
    syncState.blockBodiesQueue.isEmpty shouldBe true
    syncState.receiptsQueue.isEmpty shouldBe true
    syncState.nextBlockToFullyValidate shouldBe (bestBlockHeaderNumber - syncConfig.fastSyncBlockValidationN + 1)
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
        pendingMptNodes = Seq(StateMptNodeHash(targetBlockHeader.stateRoot))))

    Thread.sleep(1.seconds.toMillis)

    val peerStatus = Status(1, 1, 20, ByteString("peer2_bestHash"), ByteString("unused"))

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(Map(
      peer -> PeerInfo(peerStatus, forkAccepted = true, totalDifficulty = peerStatus.totalDifficulty, maxBlockNumber = 0)))

    etcPeerManager.send(syncController.getSingleChild("fast-sync").getChild(Seq("target-block-selector").toIterator), handshakedPeers)
    etcPeerManager.send(syncController.getSingleChild("fast-sync"), handshakedPeers)

    val stateMptLeafWithAccount =
      ByteString(Hex.decode("f86d9e328415c225a782bb339b22acad1c739e42277bc7ef34de3623114997ce78b84cf84a0186cb7d8738d800a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

    val watcher = TestProbe()
    watcher.watch(syncController)

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot)), peer.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer.id))))
    peerMessageBus.reply(MessageFromPeer(NodeData(Seq(stateMptLeafWithAccount)), peer.id))
    peerMessageBus.expectMsg(Unsubscribe())

    //trigger scheduling
    etcPeerManager.expectNoMsg(80.millis)

    //wait for peers throttle
    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)

    //trigger scheduling again
    Thread.sleep(2.second.toMillis)
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(targetBlockHeader.number), expectedTargetBlock - bestBlockHeaderNumber, 0, reverse = false),
      peer.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
    peerMessageBus.reply(MessageFromPeer(BlockHeaders(Seq(targetBlockHeader)), peer.id))
    peerMessageBus.expectMsg(Unsubscribe())
  }

  it should "not use (blacklist) a peer that fails to respond within time limit" in new TestSetup() {
    val peer2TestProbe: TestProbe = TestProbe()(system)

    val peer2 = Peer(new InetSocketAddress("127.0.0.1", 0), peer2TestProbe.ref, false)

    Thread.sleep(1.seconds.toMillis)

    val peer2Status = Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)

    storagesInstance.storages.fastSyncStateStorage.putSyncState(SyncState(targetBlockHeader)
      .copy(bestBlockHeaderNumber = targetBlockHeader.number,
        pendingMptNodes = Seq(StateMptNodeHash(targetBlockHeader.stateRoot))))

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(Map(
      peer2 -> PeerInfo(peer2Status, forkAccepted = true, totalDifficulty = peer2Status.totalDifficulty, maxBlockNumber = 0)))

    etcPeerManager.send(syncController.getSingleChild("fast-sync"), handshakedPeers)

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))
    peerMessageBus.expectMsg(Unsubscribe())

    // response timeout
    Thread.sleep(2.seconds.toMillis)
    etcPeerManager.expectNoMsg()

    // wait for blacklist timeout
    Thread.sleep(6.seconds.toMillis)

    // peer should not be blacklisted anymore
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot)), peer2.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer2.id))))
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

    Thread.sleep(1.seconds.toMillis)

    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status= Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))
    val peer3Status= Status(1, 1, 1, ByteString("peer3_bestHash"), ByteString("unused"))
    val peer4Status= Status(1, 1, 1, ByteString("peer4_bestHash"), ByteString("unused"))

    val handshakedPeers = HandshakedPeers(Map(
      peer1 -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0),
      peer2 -> PeerInfo(peer2Status, forkAccepted = false, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0),
      peer3 -> PeerInfo(peer3Status, forkAccepted = false, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0),
      peer4 -> PeerInfo(peer4Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0)))

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)
    storagesInstance.storages.appStateStorage.putBestBlockNumber(targetBlockHeader.number)

    syncController ! SyncController.Start

    Thread.sleep(startDelayMillis)

    etcPeerManager.send(syncController.getSingleChild("fast-sync").getChild(Seq("target-block-selector").toIterator), handshakedPeers)
    etcPeerManager.send(syncController.getSingleChild("fast-sync"), handshakedPeers)

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer4.id))))

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Right(ByteString("peer1_bestHash")), 1, 0, reverse = false),
      peer1.id))
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000))), peer1.id))
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Right(ByteString("peer4_bestHash")), 1, 0, reverse = false),
      peer4.id))
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000))), peer4.id))

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer4.id))),
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
  }

  it should "re-enqueue block bodies when empty response is received" in new TestSetup {
    // there are 2 blocks queued
    val syncState = SyncState(targetBlock = Fixtures.Blocks.Block3125369.header, blockBodiesQueue = Seq(ByteString("1"), ByteString("asd")))
    storagesInstance.storages.fastSyncStateStorage.putSyncState(syncState)

    // start fast sync
    val fastSync = TestActorRef(Props(new FastSync(
      storagesInstance.storages.fastSyncStateStorage,
      storagesInstance.storages.appStateStorage,
      blockchain,
      new Mocks.MockValidatorsAlwaysSucceed,
      peerMessageBus.ref, etcPeerManager.ref,
      syncConfig,
      scheduler = system.scheduler)))

    val peer1TestProbe: TestProbe = TestProbe()(system)
    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref, false)
    val peer1Status = Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val handshakedPeers = HandshakedPeers(Map(
      peer1 -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0)))

    fastSync ! FastSync.Start

    etcPeerManager.send(fastSync, handshakedPeers)

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockBodies(Seq(ByteString("1"), ByteString("asd"))), peer1.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(peer1.id))))
    peerMessageBus.reply(MessageFromPeer(BlockBodies(Nil), peer1.id))
    peerMessageBus.expectMsg(Unsubscribe())

    Thread.sleep(500)
    fastSync ! FastSync.PersistSyncState
    Thread.sleep(500)

    storagesInstance.storages.fastSyncStateStorage.getSyncState().get.blockBodiesQueue shouldBe Seq(ByteString("1"), ByteString("asd"))
  }

  it should "start fast sync after restart, if fast sync was partially ran and then regular sync started" in new TestSetup() with MockFactory {
    val peerTestProbe: TestProbe = TestProbe()(system)
    val peer = Peer(new InetSocketAddress("127.0.0.1", 0), peerTestProbe.ref, false)
    val peer1Status= Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))

    //Save previous incomplete attempt to fast sync
    val syncState = SyncState(targetBlock = Fixtures.Blocks.Block3125369.header, pendingMptNodes = Seq(StateMptNodeHash(ByteString("node_hash"))))
    storagesInstance.storages.fastSyncStateStorage.putSyncState(syncState)

    //Attempt to start regular sync

    override lazy val syncConfig = defaultSyncConfig.copy(doFastSync = false)

    val syncControllerWithRegularSync = TestActorRef(Props(new SyncController(
      storagesInstance.storages.appStateStorage,
      blockchain,
      storagesInstance.storages.fastSyncStateStorage,
      ledger,
      new Mocks.MockValidatorsAlwaysSucceed,
      peerMessageBus.ref, pendingTransactionsManager.ref, ommersPool.ref, etcPeerManager.ref,
      syncConfig,
      () => (),
      externalSchedulerOpt = None)))

    syncControllerWithRegularSync ! SyncController.Start

    syncControllerWithRegularSync.getSingleChild("fast-sync") ! HandshakedPeers(Map(
      peer -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0)))

    //Fast sync node request should be received
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetNodeData(Seq(ByteString("node_hash"))), peer.id))
  }

  class TestSetup(
    blocksForWhichLedgerFails: Seq[BigInt] = Nil,
    _validators: Validators = new Mocks.MockValidatorsAlwaysSucceed
  ) extends EphemBlockchainTestSetup {

    //+ cake overrides
    // FIXME ! overrides the same impl.
    override lazy val vm: VM = VM

    override lazy val validators: Validators = _validators

    override lazy val consensus: TestConsensus = loadConsensus().withValidators(validators)

    override lazy val ledger: Ledger = mock[Ledger]
    //+ cake overrides

    private def isNewBlock(msg: Message): Boolean = msg match {
      case _: NewBlock => true
      case _ => false
    }

    val etcPeerManager = TestProbe()
    etcPeerManager.ignoreMsg{
      case EtcPeerManagerActor.SendMessage(msg, _) if isNewBlock(msg.underlyingMsg) => true
      case EtcPeerManagerActor.GetHandshakedPeers => true
    }

    val peerMessageBus = TestProbe()
    peerMessageBus.ignoreMsg{
      case Subscribe(MessageClassifier(codes, PeerSelector.AllPeers)) if codes == Set(NewBlock.code, NewBlockHashes.code) => true
      case Subscribe(PeerDisconnectedClassifier(_)) => true
      case Unsubscribe(Some(PeerDisconnectedClassifier(_))) => true
    }
    val pendingTransactionsManager = TestProbe()
    val ommersPool = TestProbe()

    lazy val defaultSyncConfig = SyncConfig(
      doFastSync = true,

      printStatusInterval = 1.hour,
      persistStateSnapshotInterval = 20.seconds,
      targetBlockOffset = 500,
      branchResolutionRequestSize = 20,
      blacklistDuration = 5.seconds,
      syncRetryInterval = 1.second,
      checkForNewBlockInterval = 1.second,
      startRetryInterval = 500.milliseconds,
      blockChainOnlyPeersPoolSize = 100,
      maxConcurrentRequests = 10,
      blockHeadersPerRequest = 10,
      blockBodiesPerRequest = 10,
      nodesPerRequest = 10,
      receiptsPerRequest = 10,
      minPeersToChooseTargetBlock = 2,
      peerResponseTimeout = 1.second,
      peersScanInterval = 500.milliseconds,
      fastSyncThrottle = 100.milliseconds,
      maxQueuedBlockNumberAhead = 10,
      maxQueuedBlockNumberBehind = 10,
      maxNewBlockHashAge = 20,
      maxNewHashes = 64,
      redownloadMissingStateNodes = false,
      fastSyncBlockValidationK = 100,
      fastSyncBlockValidationN = 2048,
      fastSyncBlockValidationX = 50
    )

    override lazy val syncConfig = defaultSyncConfig

    lazy val syncController = TestActorRef(Props(new SyncController(
      storagesInstance.storages.appStateStorage,
      blockchain,
      storagesInstance.storages.fastSyncStateStorage,
      ledger,
      validators,
      peerMessageBus.ref, pendingTransactionsManager.ref, ommersPool.ref, etcPeerManager.ref,
      syncConfig,
      () => (),
      externalSchedulerOpt = None)))

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

    val startDelayMillis = 200
  }

}
