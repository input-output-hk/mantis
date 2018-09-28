package io.iohk.ethereum.blockchain.sync

import akka.actor.{ ActorRef, ActorSystem, Props }
import akka.testkit.{ TestActorRef, TestProbe }
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSync.{ StateMptNodeHash, SyncState }
import io.iohk.ethereum.consensus.TestConsensus
import io.iohk.ethereum.consensus.validators.BlockHeaderError.{ HeaderParentNotFoundError, HeaderPoWError }
import io.iohk.ethereum.consensus.validators.{ BlockHeaderValid, BlockHeaderValidator, Validators }
import io.iohk.ethereum.domain.{ BlockHeader, Receipt }
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.network.EtcPeerManagerActor.{ HandshakedPeers, PeerInfo }
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{ MessageClassifier, PeerDisconnectedClassifier }
import io.iohk.ethereum.network.PeerEventBusActor.{ PeerSelector, Subscribe, Unsubscribe }
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{ NewBlock, Status }
import io.iohk.ethereum.network.p2p.messages.PV62.{ BlockBody, _ }
import io.iohk.ethereum.network.p2p.messages.PV63.{ GetNodeData, GetReceipts, NodeData, Receipts }
import io.iohk.ethereum.network.{ EtcPeerManagerActor, Peer, PeerId }
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.{ Fixtures, Mocks }
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ BeforeAndAfter, FlatSpec, Matchers }

import scala.concurrent.Await
import scala.concurrent.duration._

// scalastyle:off magic.number
class SyncControllerSpec extends FlatSpec with Matchers with BeforeAndAfter with MockFactory {

  implicit var system: ActorSystem = _

  before { system = ActorSystem("SyncControllerSpec_System") }

  after { Await.result(system.terminate(), 1.second) }

  "SyncController" should "download target block and request blockHeaders" in new TestSetup() {
    syncController ! SyncController.Start

    Thread.sleep(StartDelayMillis)

    updateHandshakedPeers(HandshakedPeers(twoAcceptedPeers))

    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1Id))))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2Id))))

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Right(peer1Status.bestHash), 1, 0, reverse = false), peer1Id))
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 300000))), peer1Id))

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Right(peer2Status.bestHash), 1, 0, reverse = false), peer2Id))
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 400000))), peer2Id))

    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1Id))))
    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2Id))))

    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2Id))))
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedTargetBlock), 1, 0, reverse = false), peer2Id))

    storagesInstance.storages.appStateStorage.putBestBlockNumber(targetBlockHeader.number)
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(targetBlockHeader)), peer2Id))

    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2Id))))

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(1), 10, 0, reverse = false), peer1Id)
    )

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1Id)))
    )
  }

  it should "download target block, request state, blocks and finish when downloaded" in new TestSetup() {

    val newSafeTarget: Int = defaultExpectedTargetBlock + syncConfig.fastSyncBlockValidationX
    val bestBlockNumber: Int = defaultExpectedTargetBlock
    val firstNewBlock: Int = bestBlockNumber + 1

    startWithState(defaultState.copy(bestBlockHeaderNumber = bestBlockNumber, safeDownloadTarget = newSafeTarget))

    Thread.sleep(1.second.toMillis)

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(singlePeer)
    updateHandshakedPeers(handshakedPeers)

    val newBlocks: Seq[BlockHeader] = getHeaders(firstNewBlock, syncConfig.blockHeadersPerRequest)
    val newHashes: Seq[ByteString] = newBlocks.map(_.hash)
    val newReceipts: Seq[Seq[Receipt]] = newHashes.map(_ => Seq.empty[Receipt])
    val newBodies: Seq[BlockBody] = newBlocks.map(_ => BlockBody.empty)
    val blockNumber: BigInt = defaultTargetBlockHeader.number + 1

    // Wait for peers throttle
    Thread.sleep(Throttle)
    sendBlockHeaders(firstNewBlock, newBlocks, peer1Id, newBlocks.size)

    Thread.sleep(Throttle)
    sendNewTargetBlock(defaultTargetBlockHeader.copy(number = blockNumber), peer1Id, peer1Status, handshakedPeers)

    Thread.sleep(1.second.toMillis)
    sendReceipts(newHashes, newReceipts, peer1Id)

    Thread.sleep(Throttle)
    sendBlockBodies(newHashes, newBodies, peer1Id)

    Thread.sleep(Throttle)
    sendNodes(Seq(defaultTargetBlockHeader.stateRoot), Seq(defaultStateMptLeafWithAccount), peer1Id)

    Thread.sleep(StartDelayMillis)
    etcPeerManager.send(syncController.getSingleChild("regular-sync"), handshakedPeers)

    // Switch to regular download
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(blockNumber), syncConfig.blockHeadersPerRequest, 0, reverse = false),
      peer1Id
    ))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1Id))))
  }

  it should "handle blocks that fail validation" in new TestSetup(_validators = new Mocks.MockValidatorsAlwaysSucceed {
    override val blockHeaderValidator: BlockHeaderValidator = { (_, _) => Left(HeaderPoWError) }
  }) {

    val bestBlockNumber: Int = defaultExpectedTargetBlock - 1

    startWithState(defaultState.copy(bestBlockHeaderNumber = bestBlockNumber))

    Thread.sleep(1.second.toMillis)

    syncController ! SyncController.Start

    updateHandshakedPeers(HandshakedPeers(singlePeer))

    sendBlockHeaders(
      defaultTargetBlock.number,
      Seq(defaultTargetBlock.copy(number = defaultExpectedTargetBlock)),
      peer1Id,
      defaultExpectedTargetBlock - bestBlockNumber
    )

    persistFastSyncState()

    val syncState: SyncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    syncState.bestBlockHeaderNumber shouldBe (bestBlockNumber - syncConfig.fastSyncBlockValidationN)
    syncState.nextBlockToFullyValidate shouldBe (bestBlockNumber - syncConfig.fastSyncBlockValidationN + 1)
    syncState.blockBodiesQueue.isEmpty shouldBe true
    syncState.receiptsQueue.isEmpty shouldBe true
  }

  it should "not change best block after receiving faraway block" in new TestSetup(_validators = new Mocks.MockValidatorsAlwaysSucceed {
    override val blockHeaderValidator: BlockHeaderValidator = { (_, _) => Left(HeaderParentNotFoundError) }
  }) {

    val targetNumber = 200000
    val bestNumber: Int = targetNumber - 10
    val startState: SyncState = defaultState.copy(
      targetBlock = baseBlockHeader.copy(number = targetNumber ),
      bestBlockHeaderNumber = bestNumber,
      safeDownloadTarget = targetNumber + 10
    )

    startWithState(startState)
    Thread.sleep(1.second.toMillis)

    syncController ! SyncController.Start
    updateHandshakedPeers(HandshakedPeers(singlePeer))
    val fast: ActorRef = syncController.getSingleChild(FastSyncName)
    Thread.sleep(2.seconds.toMillis)

    // Send block that is way forward, we should ignore that block and blacklist that peer
    val futureHeaders: Seq[BlockHeader] = getHeaders(BigInt(bestNumber + 5),BigInt(1))
    fast ! PeerRequestHandler.ResponseReceived(peer1, BlockHeaders(futureHeaders), 2L)

    // Persist current State
    persistFastSyncState()

    val syncState: SyncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    // State should not change after this rogue block
    syncState.bestBlockHeaderNumber shouldBe bestNumber
    syncState.nextBlockToFullyValidate shouldBe defaultState.nextBlockToFullyValidate
    syncState.blockBodiesQueue.isEmpty shouldBe true
    syncState.receiptsQueue.isEmpty shouldBe true
  }

  it should "update target block if target fail" in new TestSetup(_validators = new Mocks.MockValidatorsAlwaysSucceed {
    override val blockHeaderValidator: BlockHeaderValidator = { (blockHeader, _) => {
      if (blockHeader.number != 399500 + 10 ){
        Right(BlockHeaderValid)
      } else {
        Left(HeaderPoWError)
      }
    }}
  }) {

    val newSafeTarget: Int = defaultExpectedTargetBlock + syncConfig.fastSyncBlockValidationX
    val bestBlockNumber: Int = defaultExpectedTargetBlock
    val firstNewBlock: Int = bestBlockNumber + 1

    startWithState(defaultState.copy(bestBlockHeaderNumber = bestBlockNumber, safeDownloadTarget = newSafeTarget))

    Thread.sleep(1.second.toMillis)

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(singlePeer)
    updateHandshakedPeers(handshakedPeers)

    val newBlocks: Seq[BlockHeader] = getHeaders(firstNewBlock, syncConfig.blockHeadersPerRequest)
    sendBlockHeaders(firstNewBlock, newBlocks, peer1Id, syncConfig.blockHeadersPerRequest)

    val numberOfBLocksThatPassedValidation = 9
    val newBestBlock: Int = bestBlockNumber + numberOfBLocksThatPassedValidation

    Thread.sleep(1.second.toMillis)

    val newBestBlockHeader: BlockHeader =
      defaultTargetBlock.copy(number = defaultExpectedTargetBlock + syncConfig.targetBlockOffset + syncConfig.blockHeadersPerRequest)

    sendNewTargetBlock(newBestBlockHeader, peer1Id, peer1Status, handshakedPeers)

    persistFastSyncState()

    val syncState: SyncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    syncState.targetBlock shouldEqual  newBestBlockHeader
    syncState.safeDownloadTarget shouldEqual  newBestBlockHeader.number + syncConfig.fastSyncBlockValidationX
    syncState.bestBlockHeaderNumber shouldBe (newBestBlock - syncConfig.fastSyncBlockValidationN)
    syncState.blockBodiesQueue.isEmpty shouldBe true
    syncState.receiptsQueue.isEmpty shouldBe true
    syncState.nextBlockToFullyValidate shouldBe (newBestBlock - syncConfig.fastSyncBlockValidationN + 1)
    syncState.targetBlockUpdateFailures shouldEqual 1
  }

  it should "not process, out of date new target block" in new TestSetup() {

    val newSafeTarget: Int = defaultExpectedTargetBlock + syncConfig.fastSyncBlockValidationX
    val bestBlockNumber: Int = defaultExpectedTargetBlock
    val firstNewBlock: Int = bestBlockNumber + 1

    startWithState(defaultState.copy(bestBlockHeaderNumber = bestBlockNumber, safeDownloadTarget = newSafeTarget))

    Thread.sleep(1.second.toMillis)

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(singlePeer)
    updateHandshakedPeers(handshakedPeers)

    val newBlocks: Seq[BlockHeader] = getHeaders(firstNewBlock, syncConfig.blockHeadersPerRequest)
    sendBlockHeaders(firstNewBlock, newBlocks, peer1Id, syncConfig.blockHeadersPerRequest)

    Thread.sleep(1.second.toMillis)

    val newTarget: BlockHeader = defaultTargetBlock.copy(number = defaultTargetBlock.number - 1)

    sendNewTargetBlock(newTarget, peer1Id, peer1Status, handshakedPeers)

    persistFastSyncState()

    val syncState: SyncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    syncState.targetBlockUpdateFailures shouldEqual  1

    Thread.sleep(syncConfig.syncRetryInterval.toMillis)

    val goodTarget: BlockHeader = newTarget.copy(number = newTarget.number + syncConfig.blockHeadersPerRequest)
    sendNewTargetBlock(goodTarget, peer1Id, peer1Status, handshakedPeers, "$b")

    persistFastSyncState()

    val newSyncState: SyncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    newSyncState.safeDownloadTarget shouldEqual goodTarget.number + syncConfig.fastSyncBlockValidationX
    newSyncState.targetBlock shouldEqual goodTarget
    newSyncState.bestBlockHeaderNumber shouldEqual bestBlockNumber + syncConfig.blockHeadersPerRequest
    newSyncState.targetBlockUpdateFailures shouldEqual  1
  }

  it should "should start state download only when target block is fresh enough" in new TestSetup() {

    val newSafeTarget: Int = defaultExpectedTargetBlock + syncConfig.fastSyncBlockValidationX
    val bestBlockNumber: Int = defaultExpectedTargetBlock
    val firstNewBlock: Int = bestBlockNumber + 1

    startWithState(defaultState.copy(bestBlockHeaderNumber = bestBlockNumber, safeDownloadTarget = newSafeTarget))

    Thread.sleep(1.second.toMillis)

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(singlePeer)

    updateHandshakedPeers(handshakedPeers)

    val newBlocks: Seq[BlockHeader] = getHeaders(firstNewBlock, syncConfig.blockHeadersPerRequest)

    sendBlockHeaders(firstNewBlock, newBlocks, peer1Id, syncConfig.blockHeadersPerRequest)

    Thread.sleep(1.second.toMillis)

    val newTarget: BlockHeader = defaultTargetBlock.copy(number = defaultExpectedTargetBlock + syncConfig.maxTargetDifference)

    sendNewTargetBlock(newTarget, peer1Id, peer1Status, handshakedPeers)

    persistFastSyncState()

    val syncState: SyncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    // Target did not change as new target was close enough
    syncState.targetBlock shouldEqual defaultTargetBlock
  }

  it should "throttle requests to peer" in  new TestSetup() {
    val newBestBlock: BigInt = defaultExpectedTargetBlock - 2 * syncConfig.blockHeadersPerRequest
    val firstNewBlockNumber: BigInt = newBestBlock + 1
    val newBlocks: Seq[BlockHeader] = getHeaders(firstNewBlockNumber, syncConfig.blockHeadersPerRequest)
    val newReceipts: Seq[Seq[Receipt]] = newBlocks.map(_ => Seq.empty[Receipt])

    startWithState(defaultState.copy(bestBlockHeaderNumber = newBestBlock))

    syncController ! SyncController.Start

    updateHandshakedPeers(HandshakedPeers(singlePeer))

    sendBlockHeaders(firstNewBlockNumber, newBlocks, peer1Id, syncConfig.blockHeadersPerRequest)

    etcPeerManager.expectNoMessage(syncConfig.fastSyncThrottle)

    sendReceipts(newBlocks.map(_.hash), newReceipts, peer1Id)
  }

  it should "not use (blacklist) a peer that fails to respond within time limit" in new TestSetup() {

    startWithState(
      defaultState.copy(
        bestBlockHeaderNumber = defaultExpectedTargetBlock,
        pendingMptNodes = Seq(StateMptNodeHash(defaultTargetBlock.stateRoot))
      )
    )

    syncController ! SyncController.Start

    updateHandshakedPeers(HandshakedPeers(singlePeer))

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetNodeData(Seq(defaultTargetBlock.stateRoot)), peer1Id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer1Id))))
    peerMessageBus.expectMsg(Unsubscribe())

    // Response timeout
    Thread.sleep(2.seconds.toMillis)
    etcPeerManager.expectNoMessage()

    // Wait for blacklist timeout
    Thread.sleep(6.seconds.toMillis)

    // Peer should not be blacklisted anymore
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetNodeData(Seq(defaultTargetBlock.stateRoot)), peer1Id))
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer1Id))))
  }

  it should "only use ETC peer to choose target block" in new TestSetup() {
    val expectedTargetBlock = 399500
    val targetBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedTargetBlock)
    storagesInstance.storages.appStateStorage.putBestBlockNumber(targetBlockHeader.number)

    syncController ! SyncController.Start

    Thread.sleep(StartDelayMillis)

    updateHandshakedPeers(HandshakedPeers(allPeers))

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1Id))),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2Id)))
    )

    val peer1BlockHeader = GetBlockHeaders(Right(peer1Status.bestHash), 1, 0, reverse = false)
    val peer2BlockHeader = GetBlockHeaders(Right(peer2Status.bestHash), 1, 0, reverse = false)
    val blockHeaders = BlockHeaders(Seq(baseBlockHeader.copy(number = 300000)))
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(peer1BlockHeader, peer1Id))
    etcPeerManager.reply(MessageFromPeer(blockHeaders, peer1Id))
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(peer2BlockHeader, peer2Id))
    etcPeerManager.reply(MessageFromPeer(blockHeaders, peer2Id))

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer2Id))),
      Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1Id)))
    )
  }

  it should "re-enqueue block bodies when empty response is received" in new TestSetup {
    val blocks = Seq(ByteString("1"), ByteString("asd"))
    // There are 2 blocks queued
    val syncState = SyncState(targetBlock = Fixtures.Blocks.Block3125369.header, blockBodiesQueue = blocks)
    storagesInstance.storages.fastSyncStateStorage.putSyncState(syncState)

    // Start fastSync
    val fastSync = TestActorRef(Props(new FastSync(
      storagesInstance.storages.fastSyncStateStorage,
      storagesInstance.storages.appStateStorage,
      blockchain,
      new Mocks.MockValidatorsAlwaysSucceed,
      peerMessageBus.ref,
      etcPeerManager.ref,
      syncConfig,
      scheduler = system.scheduler
    )))

    fastSync ! FastSync.Start

    etcPeerManager.send(fastSync, HandshakedPeers(singlePeer))

    sendBlockBodies(blocks, Nil, peer1Id)

    persistFastSyncState()

    storagesInstance.storages.fastSyncStateStorage.getSyncState().get.blockBodiesQueue shouldBe blocks
  }

  it should "start fast sync after restart, if fast sync was partially ran and then regular sync started" in new TestSetup() with MockFactory {
    val nodeHash = ByteString("node_hash")
    // Save previous incomplete attempt to fastSync
    val syncState = SyncState(targetBlock = Fixtures.Blocks.Block3125369.header, pendingMptNodes = Seq(StateMptNodeHash(nodeHash)))
    storagesInstance.storages.fastSyncStateStorage.putSyncState(syncState)

    // Attempt to start regularSync
    override lazy val syncConfig: SyncConfig = defaultSyncConfig.copy(doFastSync = false)

    val syncControllerWithRegularSync = TestActorRef(Props(new SyncController(
      storagesInstance.storages.appStateStorage,
      blockchain,
      storagesInstance.storages.fastSyncStateStorage,
      ledger,
      new Mocks.MockValidatorsAlwaysSucceed,
      peerMessageBus.ref,
      pendingTransactionsManager.ref,
      ommersPool.ref,
      etcPeerManager.ref,
      syncConfig,
      () => (),
      externalSchedulerOpt = None
    )))

    syncControllerWithRegularSync ! SyncController.Start

    syncControllerWithRegularSync.getSingleChild(FastSyncName) ! HandshakedPeers(singlePeer)

    // FastSync node request should be received
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetNodeData(Seq(nodeHash)), peer1Id))
  }

  class TestSetup (
    blocksForWhichLedgerFails: Seq[BigInt] = Nil,
    _validators: Validators = new Mocks.MockValidatorsAlwaysSucceed
  ) extends EphemBlockchainTestSetup with SyncFixtures {

    //+ cake overrides
    override implicit lazy val system: ActorSystem = SyncControllerSpec.this.system

    override lazy val vm: VMImpl = new VMImpl

    override lazy val validators: Validators = _validators

    override lazy val consensus: TestConsensus = buildTestConsensus().withValidators(validators)

    override lazy val ledger: Ledger = mock[Ledger]
    //+ cake overrides

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

    override lazy val syncConfig: SyncConfig = defaultSyncConfig

    lazy val syncController = TestActorRef(Props(new SyncController(
      appStateStorage = storagesInstance.storages.appStateStorage,
      blockchain = blockchain,
      fastSyncStateStorage = storagesInstance.storages.fastSyncStateStorage,
      ledger = ledger,
      validators = validators,
      peerEventBus = peerMessageBus.ref,
      pendingTransactionsManager = pendingTransactionsManager.ref,
      ommersPool = ommersPool.ref,
      etcPeerManager = etcPeerManager.ref,
      syncConfig = syncConfig,
      shutdownAppFn = () => (),
      externalSchedulerOpt = None
    )))

    blockchain.save(baseBlockHeader.parentHash, BigInt(0))

    val StartDelayMillis = 200
    val Throttle: Long = syncConfig.fastSyncThrottle.toMillis

    val peer1TestProbe: TestProbe = TestProbe("peer1")
    val peer2TestProbe: TestProbe = TestProbe("peer2")
    val peer3TestProbe: TestProbe = TestProbe("peer3")
    val peer4TestProbe: TestProbe = TestProbe("peer4")

    val peer1: Peer = mkPeer(1, peer1TestProbe)
    val peer2: Peer = mkPeer(2, peer2TestProbe)
    val peer3: Peer = mkPeer(3, peer3TestProbe)
    val peer4: Peer = mkPeer(4, peer4TestProbe)

    val peer1Id: PeerId = peer1.id
    val peer2Id: PeerId = peer2.id

    val peer1Status: Status = mkPeerStatus(1)
    val peer2Status: Status = mkPeerStatus(2)
    val peer3Status: Status = mkPeerStatus(3)
    val peer4Status: Status = mkPeerStatus(4)

    val peer1Info: PeerInfo = mkPeerInfo(peer1Status)
    val peer2Info: PeerInfo = mkPeerInfo(peer2Status)

    val allPeers = Map(
      peer1 -> peer1Info,
      peer2 -> peer2Info,
      peer3 -> mkPeerInfo(peer3Status, fork = false),
      peer4 -> mkPeerInfo(peer4Status, fork = false)
    )

    val twoAcceptedPeers = Map(peer1 -> peer1Info, peer2 -> peer2Info)
    val singlePeer = Map(peer1 -> peer1Info)

    val FastSyncName = "fast-sync"

    def sendNewTargetBlock(
      targetBlockHeader: BlockHeader,
      peer: PeerId,
      peerStatus: Status,
      handshakedPeers: HandshakedPeers,
      actorName: String = "$a"
    ): Unit = {

      val messageClassifier = MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer))

      etcPeerManager.send(syncController.getSingleChild(FastSyncName).getChild(Seq(actorName).toIterator), handshakedPeers)
      peerMessageBus.expectMsg(Subscribe(messageClassifier))
      etcPeerManager.expectMsg(
        EtcPeerManagerActor.SendMessage(GetBlockHeaders(Right(peerStatus.bestHash), 1, 0, reverse = false), peer)
      )
      etcPeerManager.reply(
        MessageFromPeer(BlockHeaders(Seq(targetBlockHeader.copy(number = targetBlockHeader.number + syncConfig.targetBlockOffset))), peer)
      )
      peerMessageBus.expectMsg(Unsubscribe(messageClassifier))

      peerMessageBus.expectMsg(Subscribe(messageClassifier))
      etcPeerManager.expectMsg(
        EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(targetBlockHeader.number), 1, 0, reverse = false), peer))
      etcPeerManager.reply(
        MessageFromPeer(BlockHeaders(Seq(targetBlockHeader)), peer)
      )
      peerMessageBus.expectMsg(Unsubscribe(messageClassifier))
      peerMessageBus.expectMsg(Unsubscribe())
    }

    val defaultTargetBlockHeader: BlockHeader = baseBlockHeader.copy(
      number = defaultExpectedTargetBlock,
      stateRoot = ByteString(Hex.decode(defaultStateRoot)))

    def updateHandshakedPeers(handshakedPeers: HandshakedPeers): Unit = {
      val fastSyncActor = syncController.getSingleChild(FastSyncName)
      etcPeerManager.send(fastSyncActor.getChild(Seq("target-block-selector").toIterator), handshakedPeers)
      etcPeerManager.send(fastSyncActor, handshakedPeers)
    }

    def startWithState(state: SyncState): Unit = {
      storagesInstance.storages.fastSyncStateStorage.putSyncState(state)
    }

    def sendBlockHeaders(from: BigInt, response: Seq[BlockHeader], fromPeer: PeerId, responseLength: BigInt): Unit = {
      etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(from), responseLength, 0, reverse = false), fromPeer))
      peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(fromPeer))))
      peerMessageBus.reply(MessageFromPeer(BlockHeaders(response), fromPeer))
      peerMessageBus.expectMsg(Unsubscribe())
    }

    def sendReceipts(forBlocks: Seq[ByteString], response: Seq[Seq[Receipt]], fromPeer: PeerId): Unit = {
      etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetReceipts(forBlocks), fromPeer))
      peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(Receipts.code), PeerSelector.WithId(fromPeer))))
      peerMessageBus.reply(MessageFromPeer(Receipts(response), fromPeer))
      peerMessageBus.expectMsg(Unsubscribe())
    }

    def sendBlockBodies(forBlocks: Seq[ByteString], response: Seq[BlockBody], fromPeer: PeerId): Unit = {
      etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetBlockBodies(forBlocks), fromPeer))
      peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockBodies.code), PeerSelector.WithId(fromPeer))))
      peerMessageBus.reply(MessageFromPeer(BlockBodies(response), fromPeer))
      peerMessageBus.expectMsg(Unsubscribe())
    }

    def sendNodes(forBlocks: Seq[ByteString], response: Seq[ByteString], fromPeer: PeerId): Unit = {
      etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetNodeData(forBlocks), fromPeer))
      peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(fromPeer))))
      peerMessageBus.reply(MessageFromPeer(NodeData(response), fromPeer))
      peerMessageBus.expectMsg(Unsubscribe())
    }

    def persistFastSyncState(): Unit = {
      Thread.sleep(300)
      syncController.getSingleChild(FastSyncName) ! FastSync.PersistSyncState
      Thread.sleep(300)
    }
  }
}
