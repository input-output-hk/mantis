package io.iohk.ethereum.blockchain.sync

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestActor.AutoPilot
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSync.{StateMptNodeHash, SyncState}
import io.iohk.ethereum.consensus.TestConsensus
import io.iohk.ethereum.consensus.validators.BlockHeaderError.{HeaderParentNotFoundError, HeaderPoWError}
import io.iohk.ethereum.consensus.validators.{BlockHeaderValid, BlockHeaderValidator, Validators}
import io.iohk.ethereum.domain.{Account, BlockBody, BlockHeader, Receipt}
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.network.EtcPeerManagerActor.{HandshakedPeers, PeerInfo}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerDisconnectedClassifier}
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, GetReceipts, NodeData, Receipts}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.{Fixtures, Mocks}
import java.net.InetSocketAddress
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.Await
import scala.concurrent.duration._

// scalastyle:off file.size.limit
class SyncControllerSpec extends AnyFlatSpec with Matchers with BeforeAndAfter with MockFactory {

  implicit var system: ActorSystem = _

  before {
    system = ActorSystem("SyncControllerSpec_System")
  }

  after {
    Await.result(system.terminate(), 1.seconds)
  }

  "SyncController" should "download pivot block and request block headers" in new TestSetup() {
    syncController ! SyncController.Start

    Thread.sleep(startDelayMillis)

    updateHandshakedPeers(HandshakedPeers(twoAcceptedPeers))

    val pivotBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedPivotBlock)

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id)))
    )

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer1.id)
    )

    storagesInstance.storages.appStateStorage.putBestBlockNumber(pivotBlockHeader.number)
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer1.id))

    peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(1), 10, 0, reverse = false), peer1.id)
    )

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(),
      Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id)))
    )
  }

  it should "download pivot block, request state, blocks and finish when downloaded" in new TestSetup() {

    val newSafeTarget = defaultExpectedPivotBlock + syncConfig.fastSyncBlockValidationX
    val bestBlockNumber = defaultExpectedPivotBlock
    val firstNewBlock = bestBlockNumber + 1

    startWithState(defaultState.copy(bestBlockHeaderNumber = bestBlockNumber, safeDownloadTarget = newSafeTarget))

    Thread.sleep(1.seconds.toMillis)

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(singlePeer)
    updateHandshakedPeers(handshakedPeers)
    etcPeerManager.setAutoPilot(new AutoPilot {
      def run(sender: ActorRef, msg: Any): AutoPilot = {
        if (msg == EtcPeerManagerActor.GetHandshakedPeers) {
          sender ! handshakedPeers
        }

        this
      }
    })

    val watcher = TestProbe()
    watcher.watch(syncController)

    val newBlocks = getHeaders(firstNewBlock, syncConfig.blockHeadersPerRequest)
    val newReceipts = newBlocks.map(_.hash).map(_ => Seq.empty[Receipt])
    val newBodies = newBlocks.map(_ => BlockBody.empty)

    //wait for peers throttle
    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    sendBlockHeaders(firstNewBlock, newBlocks, peer1, newBlocks.size)

    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    sendNewPivotBlock(
      defaultpivotBlockHeader.copy(number = defaultpivotBlockHeader.number + 1),
      peer1,
      peer1Status,
      handshakedPeers
    )

    Thread.sleep(1.second.toMillis)
    sendReceipts(newBlocks.map(_.hash), newReceipts, peer1)

    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    sendBlockBodies(newBlocks.map(_.hash), newBodies, peer1)

    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    sendNodes(Seq(defaultpivotBlockHeader.stateRoot), Seq(defaultStateMptLeafWithAccount), peer1)

    //switch to regular download
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(
        GetBlockHeaders(
          Left(defaultpivotBlockHeader.number + 1),
          syncConfig.blockHeadersPerRequest,
          0,
          reverse = false
        ),
        peer1.id
      )
    )
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
  }

  it should "gracefully handle receiving empty receipts while syncing" in new TestSetup() {

    val newSafeTarget = defaultExpectedPivotBlock + syncConfig.fastSyncBlockValidationX
    val bestBlockNumber = defaultExpectedPivotBlock
    val firstNewBlock = bestBlockNumber + 1

    startWithState(defaultState.copy(bestBlockHeaderNumber = bestBlockNumber, safeDownloadTarget = newSafeTarget))

    Thread.sleep(1.seconds.toMillis)

    syncController ! SyncController.Start

    val handshakedPeers = HandshakedPeers(singlePeer)
    updateHandshakedPeers(handshakedPeers)
    etcPeerManager.setAutoPilot(new AutoPilot {
      def run(sender: ActorRef, msg: Any): AutoPilot = {
        if (msg == EtcPeerManagerActor.GetHandshakedPeers) {
          sender ! handshakedPeers
        }

        this
      }
    })

    val watcher = TestProbe()
    watcher.watch(syncController)

    val newBlocks = getHeaders(firstNewBlock, syncConfig.blockHeadersPerRequest)
    val newReceipts = newBlocks.map(_.hash).map(_ => Seq.empty[Receipt])
    val newBodies = newBlocks.map(_ => BlockBody.empty)

    //wait for peers throttle
    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    sendBlockHeaders(firstNewBlock, newBlocks, peer1, newBlocks.size)

    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    sendNewPivotBlock(
      defaultpivotBlockHeader.copy(number = defaultpivotBlockHeader.number + 1),
      peer1,
      peer1Status,
      handshakedPeers
    )

    Thread.sleep(1.second.toMillis)
    sendReceipts(newBlocks.map(_.hash), Seq(), peer1)

    // Peer will be blacklisted for empty response, so wait he is blacklisted
    Thread.sleep(2.second.toMillis)
    sendReceipts(newBlocks.map(_.hash), newReceipts, peer1)

    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    sendBlockBodies(newBlocks.map(_.hash), newBodies, peer1)

    Thread.sleep(syncConfig.fastSyncThrottle.toMillis)
    sendNodes(Seq(defaultpivotBlockHeader.stateRoot), Seq(defaultStateMptLeafWithAccount), peer1)

    //switch to regular download
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(
        GetBlockHeaders(
          Left(defaultpivotBlockHeader.number + 1),
          syncConfig.blockHeadersPerRequest,
          0,
          reverse = false
        ),
        peer1.id
      )
    )
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1.id))))
  }

  it should "handle blocks that fail validation" in new TestSetup(_validators = new Mocks.MockValidatorsAlwaysSucceed {
    override val blockHeaderValidator: BlockHeaderValidator = { (blockHeader, getBlockHeaderByHash) =>
      Left(HeaderPoWError)
    }
  }) {

    val bestBlockNumber = defaultExpectedPivotBlock - 1

    startWithState(defaultState.copy(bestBlockHeaderNumber = bestBlockNumber))

    Thread.sleep(1.seconds.toMillis)

    syncController ! SyncController.Start

    updateHandshakedPeers(HandshakedPeers(singlePeer))

    sendBlockHeaders(
      defaultpivotBlockHeader.number,
      Seq(defaultpivotBlockHeader.copy(number = defaultExpectedPivotBlock)),
      peer1,
      defaultExpectedPivotBlock - bestBlockNumber
    )

    persistState()

    val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    syncState.bestBlockHeaderNumber shouldBe (bestBlockNumber - syncConfig.fastSyncBlockValidationN)
    syncState.nextBlockToFullyValidate shouldBe (bestBlockNumber - syncConfig.fastSyncBlockValidationN + 1)
    syncState.blockBodiesQueue.isEmpty shouldBe true
    syncState.receiptsQueue.isEmpty shouldBe true
  }

  it should "rewind fast-sync state if received header have no known parent" in new TestSetup() {

    val bestBlockNumber = defaultExpectedPivotBlock - 1

    startWithState(defaultState.copy(bestBlockHeaderNumber = bestBlockNumber))

    Thread.sleep(1.seconds.toMillis)

    syncController ! SyncController.Start

    updateHandshakedPeers(HandshakedPeers(singlePeer))

    sendBlockHeaders(
      defaultpivotBlockHeader.number,
      Seq(defaultpivotBlockHeader.copy(number = defaultExpectedPivotBlock, parentHash = ByteString(0, 1))),
      peer1,
      defaultExpectedPivotBlock - bestBlockNumber
    )

    persistState()

    val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    syncState.bestBlockHeaderNumber shouldBe (bestBlockNumber - syncConfig.fastSyncBlockValidationN)
    syncState.nextBlockToFullyValidate shouldBe (bestBlockNumber - syncConfig.fastSyncBlockValidationN + 1)
    syncState.blockBodiesQueue.isEmpty shouldBe true
    syncState.receiptsQueue.isEmpty shouldBe true
  }

  it should "not change best block after receiving faraway block" in new TestSetup(
    _validators = new Mocks.MockValidatorsAlwaysSucceed {
      override val blockHeaderValidator: BlockHeaderValidator = { (blockHeader, getBlockHeaderByHash) =>
        Left(HeaderParentNotFoundError)
      }
    }
  ) {

    val targetNumber = 200000
    val bestNumber = targetNumber - 10
    val startState = defaultState.copy(
      pivotBlock = baseBlockHeader.copy(number = targetNumber),
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
    val futureHeaders = getHeaders(BigInt(bestNumber + 5), BigInt(1))
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
    override val blockHeaderValidator: BlockHeaderValidator = { (blockHeader, getBlockHeaderByHash) =>
      {
        if (blockHeader.number != 399500 + 10) {
          Right(BlockHeaderValid)
        } else {
          Left(HeaderPoWError)
        }
      }
    }
  }) {

    val newSafeTarget = defaultExpectedPivotBlock + syncConfig.fastSyncBlockValidationX
    val bestBlockNumber = defaultExpectedPivotBlock
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

    val newBestBlockHeader = defaultpivotBlockHeader.copy(number =
      defaultExpectedPivotBlock + syncConfig.pivotBlockOffset + syncConfig.blockHeadersPerRequest
    )

    sendNewPivotBlock(
      newBestBlockHeader,
      peer1,
      peer1Status,
      handshakedPeers
    )

    persistState()

    val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    syncState.pivotBlock shouldEqual newBestBlockHeader
    syncState.safeDownloadTarget shouldEqual newBestBlockHeader.number + syncConfig.fastSyncBlockValidationX
    syncState.bestBlockHeaderNumber shouldBe (newBestBlock - syncConfig.fastSyncBlockValidationN)
    syncState.blockBodiesQueue.isEmpty shouldBe true
    syncState.receiptsQueue.isEmpty shouldBe true
    syncState.nextBlockToFullyValidate shouldBe (newBestBlock - syncConfig.fastSyncBlockValidationN + 1)
    syncState.targetBlockUpdateFailures shouldEqual 1
  }

  it should "not process, out of date new target block" in new TestSetup() {

    val newSafeTarget = defaultExpectedPivotBlock + syncConfig.fastSyncBlockValidationX
    val bestBlockNumber = defaultExpectedPivotBlock
    val bestValid = bestBlockNumber
    val firstNewBlock = bestBlockNumber + 1

    startWithState(
      defaultState.copy(
        bestBlockHeaderNumber = bestBlockNumber,
        safeDownloadTarget = newSafeTarget
      )
    )

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

    val newTarget = defaultpivotBlockHeader.copy(number = defaultpivotBlockHeader.number - 1)

    sendNewPivotBlock(
      newTarget,
      peer1,
      peer1Status,
      handshakedPeers
    )

    persistState()

    val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    syncState.targetBlockUpdateFailures shouldEqual 1

    Thread.sleep(syncConfig.syncRetryInterval.toMillis)

    val goodTarget = newTarget.copy(number = newTarget.number + syncConfig.blockHeadersPerRequest)
    sendNewPivotBlock(
      goodTarget,
      peer1,
      peer1Status,
      handshakedPeers,
      "$b"
    )

    persistState()

    val newSyncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    newSyncState.safeDownloadTarget shouldEqual goodTarget.number + syncConfig.fastSyncBlockValidationX
    newSyncState.pivotBlock shouldEqual goodTarget
    newSyncState.bestBlockHeaderNumber shouldEqual bestBlockNumber + syncConfig.blockHeadersPerRequest
    newSyncState.targetBlockUpdateFailures shouldEqual 1
  }

  it should "should start state download only when target block is fresh enough" in new TestSetup() {

    val newSafeTarget = defaultExpectedPivotBlock + syncConfig.fastSyncBlockValidationX
    val bestBlockNumber = defaultExpectedPivotBlock
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

    val newTarget = defaultpivotBlockHeader.copy(number = defaultExpectedPivotBlock + syncConfig.maxTargetDifference)

    sendNewPivotBlock(
      newTarget,
      peer1,
      peer1Status,
      handshakedPeers
    )

    persistState()

    val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get

    // Target did not change as new target was close enough
    syncState.pivotBlock shouldEqual defaultpivotBlockHeader
  }

  it should "throttle requests to peer" in new TestSetup() {
    val newBestBlock = defaultExpectedPivotBlock - 2 * syncConfig.blockHeadersPerRequest
    val newBestValid = newBestBlock
    val firstNewBlockNumber = newBestBlock + 1
    startWithState(defaultState.copy(bestBlockHeaderNumber = newBestBlock))

    syncController ! SyncController.Start

    updateHandshakedPeers(HandshakedPeers(singlePeer))

    val newBlocks = getHeaders(firstNewBlockNumber, syncConfig.blockHeadersPerRequest)
    sendBlockHeaders(firstNewBlockNumber, newBlocks, peer1, 10)
    etcPeerManager.expectNoMessage(syncConfig.fastSyncThrottle)
    sendReceipts(newBlocks.map(_.hash), newBlocks.map(_ => Seq.empty[Receipt]), peer1)
  }

  it should "not use (blacklist) a peer that fails to respond within time limit" in new TestSetup() {

    Thread.sleep(1.seconds.toMillis)

    startWithState(
      defaultState.copy(
        bestBlockHeaderNumber = defaultExpectedPivotBlock,
        pendingMptNodes = Seq(StateMptNodeHash(defaultpivotBlockHeader.stateRoot))
      )
    )

    syncController ! SyncController.Start

    updateHandshakedPeers(HandshakedPeers(singlePeer))

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetNodeData(Seq(defaultpivotBlockHeader.stateRoot)), peer1.id)
    )
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer1.id))))
    peerMessageBus.expectMsg(Unsubscribe())

    // response timeout
    Thread.sleep(1.seconds.toMillis)

    etcPeerManager.expectNoMessage(1.second)

    // wait for blacklist timeout
    Thread.sleep(2.seconds.toMillis)

    // peer should not be blacklisted anymore
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetNodeData(Seq(defaultpivotBlockHeader.stateRoot)), peer1.id)
    )
    peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(NodeData.code), PeerSelector.WithId(peer1.id))))
  }

  it should "re-enqueue block bodies when empty response is received" in new TestSetup {
    // there are 2 blocks queued
    val syncState = SyncState(
      pivotBlock = Fixtures.Blocks.Block3125369.header,
      blockBodiesQueue = Seq(ByteString("1"), ByteString("asd"))
    )
    storagesInstance.storages.fastSyncStateStorage.putSyncState(syncState)

    // start fast sync
    val fastSync = TestActorRef(
      Props(
        new FastSync(
          storagesInstance.storages.fastSyncStateStorage,
          storagesInstance.storages.appStateStorage,
          blockchain,
          new Mocks.MockValidatorsAlwaysSucceed,
          peerMessageBus.ref,
          etcPeerManager.ref,
          syncConfig,
          scheduler = system.scheduler
        )
      )
    )

    fastSync ! FastSync.Start

    etcPeerManager.send(fastSync, HandshakedPeers(singlePeer))

    sendBlockBodies(Seq(ByteString("1"), ByteString("asd")), Nil, peer1)

    Thread.sleep(500)
    fastSync ! FastSync.PersistSyncState
    Thread.sleep(500)

    storagesInstance.storages.fastSyncStateStorage.getSyncState().get.blockBodiesQueue shouldBe Seq(
      ByteString("1"),
      ByteString("asd")
    )
  }

  it should "start fast sync after restart, if fast sync was partially ran and then regular sync started" in new TestWithRegularSyncOnSetup
    with MockFactory {
    //Save previous incomplete attempt to fast sync
    val syncState = SyncState(
      pivotBlock = Fixtures.Blocks.Block3125369.header,
      pendingMptNodes = Seq(StateMptNodeHash(ByteString("node_hash")))
    )
    storagesInstance.storages.fastSyncStateStorage.putSyncState(syncState)

    //Attempt to start regular sync

    override lazy val syncConfig = defaultSyncConfig.copy(doFastSync = false)

    syncControllerWithRegularSync ! SyncController.Start

    syncControllerWithRegularSync.getSingleChild("fast-sync") ! HandshakedPeers(singlePeer)

    //Fast sync node request should be received
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetNodeData(Seq(ByteString("node_hash"))), peer1.id))
  }

  class TestSetup(
      blocksForWhichLedgerFails: Seq[BigInt] = Nil,
      _validators: Validators = new Mocks.MockValidatorsAlwaysSucceed
  ) extends EphemBlockchainTestSetup
      with TestSyncConfig {

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
    etcPeerManager.ignoreMsg {
      case EtcPeerManagerActor.SendMessage(msg, _) if isNewBlock(msg.underlyingMsg) => true
      case EtcPeerManagerActor.GetHandshakedPeers => true
    }

    val peerMessageBus = TestProbe()
    peerMessageBus.ignoreMsg {
      case Subscribe(MessageClassifier(codes, PeerSelector.AllPeers))
          if codes == Set(NewBlock.code, NewBlockHashes.code) =>
        true
      case Subscribe(PeerDisconnectedClassifier(_)) => true
      case Unsubscribe(Some(PeerDisconnectedClassifier(_))) => true
    }
    val pendingTransactionsManager = TestProbe()
    val ommersPool = TestProbe()

    override def defaultSyncConfig: SyncConfig = super.defaultSyncConfig.copy(
      doFastSync = true,
      branchResolutionRequestSize = 20,
      checkForNewBlockInterval = 1.second,
      blockHeadersPerRequest = 10,
      blockBodiesPerRequest = 10,
      minPeersToChoosePivotBlock = 1,
      peersScanInterval = 500.milliseconds,
      redownloadMissingStateNodes = false,
      fastSyncBlockValidationX = 10,
      blacklistDuration = 1.second
    )

    lazy val syncController = TestActorRef(
      Props(
        new SyncController(
          storagesInstance.storages.appStateStorage,
          blockchain,
          storagesInstance.storages.fastSyncStateStorage,
          ledger,
          validators,
          peerMessageBus.ref,
          pendingTransactionsManager.ref,
          ommersPool.ref,
          etcPeerManager.ref,
          syncConfig,
          externalSchedulerOpt = None
        )
      )
    )

    val EmptyTrieRootHash: ByteString = Account.EmptyStorageRootHash
    val baseBlockHeader = Fixtures.Blocks.Genesis.header

    blockchain.storeTotalDifficulty(baseBlockHeader.parentHash, BigInt(0)).commit()

    val startDelayMillis = 200

    val peer1TestProbe: TestProbe = TestProbe("peer1")(system)
    val peer2TestProbe: TestProbe = TestProbe("peer2")(system)
    val peer3TestProbe: TestProbe = TestProbe("peer3")(system)

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref, false)
    val peer2 = Peer(new InetSocketAddress("127.0.0.2", 0), peer2TestProbe.ref, false)
    val peer3 = Peer(new InetSocketAddress("127.0.0.3", 0), peer3TestProbe.ref, false)

    val peer1Status = Status(1, 1, 20, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer2Status = Status(1, 1, 20, ByteString("peer2_bestHash"), ByteString("unused"))

    val bestBlock = 400000
    val expectedPivotBlock = bestBlock - syncConfig.pivotBlockOffset

    val twoAcceptedPeers = Map(
      peer1 -> PeerInfo(
        peer1Status,
        forkAccepted = true,
        totalDifficulty = peer1Status.totalDifficulty,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer1Status.bestHash
      ),
      peer2 -> PeerInfo(
        peer2Status,
        forkAccepted = true,
        totalDifficulty = peer1Status.totalDifficulty,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer2Status.bestHash
      )
    )

    val singlePeer = Map(
      peer1 -> PeerInfo(
        peer1Status,
        forkAccepted = true,
        totalDifficulty = peer1Status.totalDifficulty,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer1Status.bestHash
      )
    )

    def sendNewPivotBlock(
        pivotBlockHeader: BlockHeader,
        peer: Peer,
        peerStatus: Status,
        handshakedPeers: HandshakedPeers,
        actorName: String = "$a"
    ): Unit = {

      val pivotBlockSelector =
        syncController.getSingleChild("fast-sync").getChild(Seq(actorName).toIterator)

      val updatingPeer: Map[Peer, PeerInfo] = handshakedPeers.peers
        .mapValues(pi => pi.copy(maxBlockNumber = pivotBlockHeader.number + syncConfig.pivotBlockOffset))
        .take(1)

      val handshakedPeer = handshakedPeers.copy(peers = updatingPeer)

      etcPeerManager.send(pivotBlockSelector, handshakedPeer)

      val (peer, _) = updatingPeer.head

      peerMessageBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))

      etcPeerManager.expectMsg(
        EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(pivotBlockHeader.number), 1, 0, reverse = false), peer.id)
      )

      // Collecting pivot block (for voting)
      pivotBlockSelector.!(MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer.id))(peerMessageBus.ref)

      peerMessageBus.expectMsg(Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id))))
      peerMessageBus.expectMsg(Unsubscribe())
    }

    val defaultExpectedPivotBlock = 399500
    val defaultSafeDownloadTarget = defaultExpectedPivotBlock
    val defaulBestValidated = defaultExpectedPivotBlock - 1
    val defaultBestBlock = defaultExpectedPivotBlock - 1
    val defaultStateRoot = "deae1dfad5ec8dcef15915811e1f044d2543674fd648f94345231da9fc2646cc"
    val defaultpivotBlockHeader =
      baseBlockHeader.copy(number = defaultExpectedPivotBlock, stateRoot = ByteString(Hex.decode(defaultStateRoot)))

    val defaultState =
      SyncState(defaultpivotBlockHeader, defaultSafeDownloadTarget, bestBlockHeaderNumber = defaultBestBlock)

    val defaultStateMptLeafWithAccount =
      ByteString(
        Hex.decode(
          "f86d9e328415c225a782bb339b22acad1c739e42277bc7ef34de3623114997ce78b84cf84a0186cb7d8738d800a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
        )
      )

    def getHeaders(from: BigInt, number: BigInt): Seq[BlockHeader] = {
      val headers = (from until from + number).toSeq.map { nr =>
        defaultpivotBlockHeader.copy(number = nr)
      }

      def genChain(
          parenthash: ByteString,
          headers: Seq[BlockHeader],
          result: Seq[BlockHeader] = Seq.empty
      ): Seq[BlockHeader] = {
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
      etcPeerManager.send(
        syncController.getSingleChild("fast-sync").getChild(Seq("pivot-block-selector").toIterator),
        handshakedPeers
      )
      etcPeerManager.send(syncController.getSingleChild("fast-sync"), handshakedPeers)
    }

    def startWithState(state: SyncState): Unit = {
      storagesInstance.storages.fastSyncStateStorage.putSyncState(state)
    }

    def sendBlockHeaders(from: BigInt, response: Seq[BlockHeader], fromPeer: Peer, responseLength: BigInt): Unit = {
      etcPeerManager.expectMsg(
        EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(from), responseLength, 0, reverse = false), fromPeer.id)
      )
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
      etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetBlockBodies(forBlocks), fromPeer.id))
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

  class TestWithRegularSyncOnSetup extends TestSetup() {
    val syncControllerWithRegularSync = TestActorRef(
      Props(
        new SyncController(
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
          externalSchedulerOpt = None
        )
      )
    )
  }
}
