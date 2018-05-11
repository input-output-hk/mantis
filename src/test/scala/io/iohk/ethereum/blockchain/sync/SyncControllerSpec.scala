package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSync.SyncState
import io.iohk.ethereum.blockchain.sync.FastSyncStateHandler.StateMptNodeHash
import io.iohk.ethereum.consensus.TestConsensus
import io.iohk.ethereum.consensus.validators.BlockHeaderError.{HeaderParentNotFoundError, HeaderPoWError}
import io.iohk.ethereum.consensus.validators.{BlockHeaderValid, BlockHeaderValidator, Validators}
import io.iohk.ethereum.domain.{Account, BlockHeader, Receipt}
import io.iohk.ethereum.ledger.Ledger.VMImpl
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
import io.iohk.ethereum.{Fixtures, Mocks}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import org.bouncycastle.util.encoders.Hex

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

  "SyncController" should "download target block and request blockheaders" in new TestSetup() {
    syncController ! SyncController.Start

    Thread.sleep(startDelayMillis)

    updateHandshakedPeers(HandshakedPeers(twoAcceptedPeers))

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

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(1), 10, 0, reverse = false), peer1.id)
    )

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id)))
    )
  }

  it should "download target block, request state, blocks and finish when downloaded" in new TestSetup() {

    val newSafeTarget   = defaultExpectedTargetBlock + syncConfig.fastSyncBlockValidationX
    val bestBlockNumber = defaultExpectedTargetBlock
    val firstNewBlock = bestBlockNumber + 1

    startWithState(defaultState.copy(
      bestBlockHeaderNumber = bestBlockNumber,
      safeDownloadTarget = newSafeTarget)
    )

    Thread.sleep(1.seconds.toMillis)

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(singlePeer)
    updateHandshakedPeers(handshakedPeers)

    val watcher = TestProbe()
    watcher.watch(syncController)

    val newBlocks = getHeaders(firstNewBlock, syncConfig.blockHeadersPerRequest)
    val newReceipts = newBlocks.map(_.hash).map(_ => Seq.empty[Receipt])
    val newBodies = newBlocks.map(_ => BlockBody(Nil, Nil))

    //wait for peers throttle
    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    sendBlockHeaders(firstNewBlock, newBlocks, peer1, newBlocks.size)

    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    sendNewTargetBlock(defaultTargetBlockHeader.copy(number = defaultTargetBlockHeader.number + 1), peer1, peer1Status, handshakedPeers)

    Thread.sleep(1.second.toMillis)
    sendReceipts(newBlocks.map(_.hash), newReceipts, peer1)

    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    sendBlockBodies(newBlocks.map(_.hash), newBodies, peer1)

    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    sendNodes(Seq(defaultTargetBlockHeader.stateRoot), Seq(defaultStateMptLeafWithAccount), peer1)

    Thread.sleep(startDelayMillis)
    etcPeerManager.send(syncController.getSingleChild("regular-sync"), handshakedPeers)

    //switch to regular download
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(defaultTargetBlockHeader.number + 1), syncConfig.blockHeadersPerRequest, 0, reverse = false),
      peer1.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
  }

  it should "handle blocks that fail validation" in new TestSetup(_validators = new Mocks.MockValidatorsAlwaysSucceed {
    override val blockHeaderValidator: BlockHeaderValidator = { (blockHeader, getBlockHeaderByHash) => Left(HeaderPoWError) }
  }) {

    val bestBlockNumber = defaultExpectedTargetBlock - 1

    startWithState(defaultState.copy(bestBlockHeaderNumber = bestBlockNumber))

    Thread.sleep(1.seconds.toMillis)

    syncController ! SyncController.Start

    updateHandshakedPeers(HandshakedPeers(singlePeer))

    sendBlockHeaders(
      defaultTargetBlockHeader.number,
      Seq(defaultTargetBlockHeader.copy(number = defaultExpectedTargetBlock)),
      peer1,
      defaultExpectedTargetBlock - bestBlockNumber)

    persistState()

    val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    syncState.bestBlockHeaderNumber shouldBe (bestBlockNumber - syncConfig.fastSyncBlockValidationN)
    syncState.nextBlockToFullyValidate shouldBe (bestBlockNumber - syncConfig.fastSyncBlockValidationN + 1)
    syncState.blockBodiesQueue.isEmpty shouldBe true
    syncState.receiptsQueue.isEmpty shouldBe true
  }

  it should "not change best block after receiving faraway block" in new TestSetup(_validators = new Mocks.MockValidatorsAlwaysSucceed {
    override val blockHeaderValidator: BlockHeaderValidator = { (blockHeader, getBlockHeaderByHash) => Left(HeaderParentNotFoundError) }
  }) {

    val targetNumber = 200000
    val bestNumber = targetNumber - 10
    val startState = defaultState.copy(
      targetBlock = baseBlockHeader.copy(number = targetNumber ),
      bestBlockHeaderNumber = bestNumber,
      safeDownloadTarget = targetNumber + 10
    )

    startWithState(startState)
    Thread.sleep(1.seconds.toMillis)

    syncController ! SyncController.Start
    updateHandshakedPeers(HandshakedPeers(singlePeer))
    val fast = syncController.getSingleChild("fast-sync")
    Thread.sleep(2.seconds.toMillis)

    // Send block that is way forward, we should ignore that block and blacklist that peer
    val futureHeaders = getHeaders(BigInt(bestNumber + 5),BigInt(1))
    fast ! PeerRequestHandler.ResponseReceived(peer1, BlockHeaders(futureHeaders), 2L)

    // Persist current State
    persistState()

    val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    // State should not change after this rogue block
    syncState.bestBlockHeaderNumber shouldBe bestNumber
    syncState.nextBlockToFullyValidate shouldBe defaultState.nextBlockToFullyValidate
    syncState.blockBodiesQueue.isEmpty shouldBe true
    syncState.receiptsQueue.isEmpty shouldBe true
  }

  it should "update target block if target fail" in new TestSetup(_validators = new Mocks.MockValidatorsAlwaysSucceed {
    override val blockHeaderValidator: BlockHeaderValidator = { (blockHeader, getBlockHeaderByHash) => {
      if (blockHeader.number != 399500 + 10 ){
        Right(BlockHeaderValid)
      } else {
        Left(HeaderPoWError)
      }
    } }
  }) {

    val newSafeTarget   = defaultExpectedTargetBlock + syncConfig.fastSyncBlockValidationX
    val bestBlockNumber = defaultExpectedTargetBlock
    val firstNewBlock = bestBlockNumber + 1

    startWithState(defaultState.copy(bestBlockHeaderNumber = bestBlockNumber, safeDownloadTarget = newSafeTarget))

    Thread.sleep(1.seconds.toMillis)

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(singlePeer)
    updateHandshakedPeers(handshakedPeers)


    val newBlocks = getHeaders(firstNewBlock, syncConfig.blockHeadersPerRequest)
    sendBlockHeaders(
      firstNewBlock,
      newBlocks,
      peer1,
      syncConfig.blockHeadersPerRequest
    )

    val numberOfBLocksThatPassedValidation = 9
    val newBestBlock = bestBlockNumber + numberOfBLocksThatPassedValidation

    Thread.sleep(1.second.toMillis)

    val newBestBlockHeader = defaultTargetBlockHeader.copy(number = defaultExpectedTargetBlock + syncConfig.targetBlockOffset + syncConfig.blockHeadersPerRequest)

    sendNewTargetBlock(
      newBestBlockHeader,
      peer1,
      peer1Status,
      handshakedPeers
    )

    persistState()

    val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    syncState.targetBlock shouldEqual  newBestBlockHeader
    syncState.safeDownloadTarget shouldEqual  newBestBlockHeader.number + syncConfig.fastSyncBlockValidationX
    syncState.bestBlockHeaderNumber shouldBe (newBestBlock - syncConfig.fastSyncBlockValidationN)
    syncState.blockBodiesQueue.isEmpty shouldBe true
    syncState.receiptsQueue.isEmpty shouldBe true
    syncState.nextBlockToFullyValidate shouldBe (newBestBlock - syncConfig.fastSyncBlockValidationN + 1)
    syncState.targetBlockUpdateFailures shouldEqual 1
  }

  it should "not process, out of date new target block" in new TestSetup() {

    val newSafeTarget   = defaultExpectedTargetBlock + syncConfig.fastSyncBlockValidationX
    val bestBlockNumber = defaultExpectedTargetBlock
    val bestValid = bestBlockNumber
    val firstNewBlock = bestBlockNumber + 1

    startWithState(defaultState.copy(
      bestBlockHeaderNumber = bestBlockNumber,
      safeDownloadTarget = newSafeTarget
    ))

    Thread.sleep(1.seconds.toMillis)

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(singlePeer)
    updateHandshakedPeers(handshakedPeers)

    val newBlocks = getHeaders(firstNewBlock, syncConfig.blockHeadersPerRequest)
    sendBlockHeaders(
      firstNewBlock,
      newBlocks,
      peer1,
      syncConfig.blockHeadersPerRequest
    )

    Thread.sleep(1.second.toMillis)

    val newTarget = defaultTargetBlockHeader.copy(number = defaultTargetBlockHeader.number - 1)

    sendNewTargetBlock(
      newTarget,
      peer1,
      peer1Status,
      handshakedPeers
    )

    persistState()

    val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    syncState.targetBlockUpdateFailures shouldEqual  1

    Thread.sleep(syncConfig.syncRetryInterval.toMillis)

    val goodTarget = newTarget.copy(number = newTarget.number + syncConfig.blockHeadersPerRequest)
    sendNewTargetBlock(
      goodTarget,
      peer1,
      peer1Status,
      handshakedPeers,
      "$b"
    )

    persistState()

    val newSyncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    newSyncState.safeDownloadTarget shouldEqual goodTarget.number + syncConfig.fastSyncBlockValidationX
    newSyncState.targetBlock shouldEqual goodTarget
    newSyncState.bestBlockHeaderNumber shouldEqual bestBlockNumber + syncConfig.blockHeadersPerRequest
    newSyncState.targetBlockUpdateFailures shouldEqual  1
  }

  it should "should start state download only when target block is fresh enough" in new TestSetup() {

    val newSafeTarget   = defaultExpectedTargetBlock + syncConfig.fastSyncBlockValidationX
    val bestBlockNumber = defaultExpectedTargetBlock
    val firstNewBlock = bestBlockNumber + 1

    startWithState(defaultState.copy(bestBlockHeaderNumber = bestBlockNumber, safeDownloadTarget = newSafeTarget))

    Thread.sleep(1.seconds.toMillis)

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(singlePeer)

    updateHandshakedPeers(handshakedPeers)

    val newBlocks = getHeaders(firstNewBlock, syncConfig.blockHeadersPerRequest)

    sendBlockHeaders(
      firstNewBlock,
      newBlocks,
      peer1,
      syncConfig.blockHeadersPerRequest
    )

    Thread.sleep(1.second.toMillis)

    val newTarget = defaultTargetBlockHeader.copy(number = defaultExpectedTargetBlock + syncConfig.maxTargetDifference)

    sendNewTargetBlock(
      newTarget,
      peer1,
      peer1Status,
      handshakedPeers
    )

    persistState()

    val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    // Target did not change as new target was close enough
    syncState.targetBlock shouldEqual defaultTargetBlockHeader
  }

  it should "throttle requests to peer" in  new TestSetup() {
    val newBestBlock = defaultExpectedTargetBlock - 2 * syncConfig.blockHeadersPerRequest
    val newBestValid = newBestBlock
    val firstNewBlockNumber = newBestBlock + 1
    startWithState(defaultState.copy(bestBlockHeaderNumber = newBestBlock))

    syncController ! SyncController.Start

    updateHandshakedPeers(HandshakedPeers(singlePeer))

    val newBlocks = getHeaders(firstNewBlockNumber, syncConfig.blockHeadersPerRequest)
    sendBlockHeaders(firstNewBlockNumber, newBlocks, peer1, 10)
    etcPeerManager.expectNoMsg(syncConfig.fastSyncThrottle)
    sendReceipts(newBlocks.map(_.hash), newBlocks.map(_ => Seq.empty[Receipt]), peer1)
  }

  it should "not use (blacklist) a peer that fails to respond within time limit" in new TestSetup() {

    Thread.sleep(1.seconds.toMillis)

    startWithState(
      defaultState.copy(
        bestBlockHeaderNumber = defaultExpectedTargetBlock,
        pendingMptNodes = Seq(StateMptNodeHash(defaultTargetBlockHeader.stateRoot))
      )
    )

    syncController ! SyncController.Start

    updateHandshakedPeers(HandshakedPeers(singlePeer))

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetNodeData(Seq(defaultTargetBlockHeader.stateRoot)), peer1.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer1.id))))
    peerMessageBus.expectMsg(Unsubscribe())

    // response timeout
    Thread.sleep(2.seconds.toMillis)
    etcPeerManager.expectNoMsg()

    // wait for blacklist timeout
    Thread.sleep(6.seconds.toMillis)

    // peer should not be blacklisted anymore
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetNodeData(Seq(defaultTargetBlockHeader.stateRoot)), peer1.id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer1.id))))
  }

  it should "only use ETC peer to choose target block" in new TestSetup() {
    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)
    storagesInstance.storages.appStateStorage.putBestBlockNumber(targetBlockHeader.number)

    syncController ! SyncController.Start

    Thread.sleep(startDelayMillis)

    updateHandshakedPeers(HandshakedPeers(allPeers))

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))))

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Right(ByteString("peer1_bestHash")), 1, 0, reverse = false),
      peer1.id))
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000))), peer1.id))
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Right(ByteString("peer2_bestHash")), 1, 0, reverse = false),
      peer2.id))
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000))), peer2.id))

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2.id))),
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

    fastSync ! FastSync.Start

    etcPeerManager.send(fastSync, HandshakedPeers(singlePeer))

    sendBlockBodies(Seq(ByteString("1"), ByteString("asd")), Nil, peer1)

    Thread.sleep(500)
    fastSync ! FastSync.PersistSyncState
    Thread.sleep(500)

    storagesInstance.storages.fastSyncStateStorage.getSyncState().get.blockBodiesQueue shouldBe Seq(ByteString("1"), ByteString("asd"))
  }

  it should "start fast sync after restart, if fast sync was partially ran and then regular sync started" in new TestSetup() with MockFactory {
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

    syncControllerWithRegularSync.getSingleChild("fast-sync") ! HandshakedPeers(singlePeer)

    //Fast sync node request should be received
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetNodeData(Seq(ByteString("node_hash"))), peer1.id))
  }

  class TestSetup(
    blocksForWhichLedgerFails: Seq[BigInt] = Nil,
    _validators: Validators = new Mocks.MockValidatorsAlwaysSucceed
  ) extends EphemBlockchainTestSetup {

    //+ cake overrides
    override implicit lazy val system: ActorSystem = SyncControllerSpec.this.system

    override lazy val vm: VMImpl = new VMImpl

    override lazy val validators: Validators = _validators

    override lazy val consensus: TestConsensus = buildTestConsensus().withValidators(validators)

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
      minPeersToChooseTargetBlock = 1,
      peerResponseTimeout = 1.second,
      peersScanInterval = 500.milliseconds,
      fastSyncThrottle = 100.milliseconds,
      maxQueuedBlockNumberAhead = 10,
      maxQueuedBlockNumberBehind = 10,
      maxNewBlockHashAge = 20,
      maxNewHashes = 64,
      broadcastNewBlockHashes = true,
      redownloadMissingStateNodes = false,
      fastSyncBlockValidationK = 100,
      fastSyncBlockValidationN = 2048,
      fastSyncBlockValidationX = 10,
      maxTargetDifference =  5,
      maximumTargetUpdateFailures = 1
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

    val peer1TestProbe: TestProbe = TestProbe("peer1")(system)
    val peer2TestProbe: TestProbe = TestProbe("peer2")(system)
    val peer3TestProbe: TestProbe = TestProbe("peer3")(system)
    val peer4TestProbe: TestProbe = TestProbe("peer4")(system)

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref, false)
    val peer2 = Peer(new InetSocketAddress("127.0.0.2", 0), peer2TestProbe.ref, false)
    val peer3 = Peer(new InetSocketAddress("127.0.0.3", 0), peer3TestProbe.ref, false)
    val peer4 = Peer(new InetSocketAddress("127.0.0.4", 0), peer4TestProbe.ref, false)

    val peer1Status= Status(1, 1, 20, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status= Status(1, 1, 20, ByteString("peer2_bestHash"), ByteString("unused"))
    val peer3Status= Status(1, 1, 20, ByteString("peer3_bestHash"), ByteString("unused"))
    val peer4Status= Status(1, 1, 20, ByteString("peer4_bestHash"), ByteString("unused"))

    val allPeers = Map(
      peer1 -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0),
      peer2 -> PeerInfo(peer2Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0),
      peer3 -> PeerInfo(peer3Status, forkAccepted = false, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0),
      peer4 -> PeerInfo(peer4Status, forkAccepted = false, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0)
    )

    val twoAcceptedPeers = Map(
      peer1 -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0),
      peer2 -> PeerInfo(peer2Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0)
    )

    val singlePeer = Map(peer1 -> PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0))

    def sendNewTargetBlock(targetBlockHeader: BlockHeader,
                           peer: Peer,
                           peerStatus: Status,
                           handshakedPeers: HandshakedPeers,
                           actorName: String = "$a"): Unit = {

      etcPeerManager.send(syncController.getSingleChild("fast-sync").getChild(Seq(actorName).toIterator), handshakedPeers)
      peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
      etcPeerManager.expectMsg(
        EtcPeerManagerActor.SendMessage(GetBlockHeaders(Right(peerStatus.bestHash), 1, 0, reverse = false), peer.id))
      etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(targetBlockHeader.copy(number = targetBlockHeader.number + syncConfig.targetBlockOffset))), peer.id))
      peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))


      peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
      etcPeerManager.expectMsg(
        EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(targetBlockHeader.number), 1, 0, reverse = false), peer.id))
      etcPeerManager.reply(
        MessageFromPeer(BlockHeaders(Seq(targetBlockHeader)), peer.id)
      )
      peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
      peerMessageBus.expectMsg(Unsubscribe())
    }

    val defaultExpectedTargetBlock = 399500
    val defaultSafeDownloadTarget = defaultExpectedTargetBlock
    val defaulBestValidated = defaultExpectedTargetBlock - 1
    val defaultBestBlock = defaultExpectedTargetBlock - 1
    val defaultStateRoot = "deae1dfad5ec8dcef15915811e1f044d2543674fd648f94345231da9fc2646cc"
    val defaultTargetBlockHeader = baseBlockHeader.copy(
      number = defaultExpectedTargetBlock,
      stateRoot = ByteString(Hex.decode(defaultStateRoot)))

    val defaultState = SyncState(
      defaultTargetBlockHeader,
      defaultSafeDownloadTarget,
      bestBlockHeaderNumber = defaultBestBlock)

    val defaultStateMptLeafWithAccount =
      ByteString(Hex.decode("f86d9e328415c225a782bb339b22acad1c739e42277bc7ef34de3623114997ce78b84cf84a0186cb7d8738d800a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

    def getHeaders(from: BigInt, number: BigInt): Seq[BlockHeader]= {
      val headers = (from until from + number).toSeq.map { nr =>
        defaultTargetBlockHeader.copy(number = nr)
      }

      def genChain(parenthash: ByteString, headers: Seq[BlockHeader], result: Seq[BlockHeader] = Seq.empty): Seq[BlockHeader] = {
        if (headers.isEmpty)
          result
        else {
          val header = headers.head
          val newHeader = header.copy(parentHash = parenthash)
          val newHash = newHeader.hash
          genChain(newHash, headers.tail, result :+ newHeader)
        }
      }

      val first = headers.head

      first +: genChain(first.hash, headers.tail)
    }

    def updateHandshakedPeers(handshakedPeers: HandshakedPeers): Unit = {
      etcPeerManager.send(syncController.getSingleChild("fast-sync").getChild(Seq("target-block-selector").toIterator), handshakedPeers)
      etcPeerManager.send(syncController.getSingleChild("fast-sync"), handshakedPeers)
    }

    def startWithState(state: SyncState): Unit = {
      storagesInstance.storages.fastSyncStateStorage.putSyncState(state)
    }

    def sendBlockHeaders(from: BigInt, response: Seq[BlockHeader], fromPeer: Peer, responseLength: BigInt): Unit = {
      etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(from), responseLength, 0, reverse = false), fromPeer.id))
      peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(fromPeer.id))))
      peerMessageBus.reply(MessageFromPeer(BlockHeaders(response), fromPeer.id))
      peerMessageBus.expectMsg(Unsubscribe())
    }

    def sendReceipts(forBlocks: Seq[ByteString], response: Seq[Seq[Receipt]], fromPeer: Peer): Unit = {
      etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetReceipts(forBlocks), fromPeer.id))
      peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(fromPeer.id))))
      peerMessageBus.reply(MessageFromPeer(Receipts(response), fromPeer.id))
      peerMessageBus.expectMsg(Unsubscribe())
    }


    def sendBlockBodies(forBlocks: Seq[ByteString], response: Seq[BlockBody], fromPeer: Peer): Unit = {
      etcPeerManager.expectMsg(
        EtcPeerManagerActor.SendMessage(GetBlockBodies(forBlocks), fromPeer.id))
      peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(fromPeer.id))))
      peerMessageBus.reply(MessageFromPeer(BlockBodies(response), fromPeer.id))
      peerMessageBus.expectMsg(Unsubscribe())
    }

    def sendNodes(forBlocks: Seq[ByteString], response: Seq[ByteString], fromPeer: Peer): Unit = {
      etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetNodeData(forBlocks), fromPeer.id))
      peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(fromPeer.id))))
      peerMessageBus.reply(MessageFromPeer(NodeData(response), fromPeer.id))
      peerMessageBus.expectMsg(Unsubscribe())
    }

    def persistState(): Unit = {
      Thread.sleep(300)
      syncController.getSingleChild("fast-sync") ! FastSync.PersistSyncState
      Thread.sleep(300)
    }

  }

}
