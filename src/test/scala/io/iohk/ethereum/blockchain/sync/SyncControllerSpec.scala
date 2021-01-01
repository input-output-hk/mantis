package io.iohk.ethereum.blockchain.sync

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestActor.AutoPilot
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.fast.FastSync
import io.iohk.ethereum.blockchain.sync.fast.FastSync.SyncState
import io.iohk.ethereum.consensus.TestConsensus
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderPoWError
import io.iohk.ethereum.consensus.validators.{BlockHeaderValid, BlockHeaderValidator, Validators}
import io.iohk.ethereum.domain.{Account, BlockBody, BlockHeader, ChainWeight, Receipt}
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.network.EtcPeerManagerActor
import io.iohk.ethereum.network.EtcPeerManagerActor.{HandshakedPeers, SendMessage}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.p2p.messages.PV62.GetBlockBodies.GetBlockBodiesEnc
import io.iohk.ethereum.network.p2p.messages.PV62.GetBlockHeaders.GetBlockHeadersEnc
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.GetNodeData.GetNodeDataEnc
import io.iohk.ethereum.network.p2p.messages.PV63.GetReceipts.GetReceiptsEnc
import io.iohk.ethereum.network.p2p.messages.PV63.{NodeData, Receipts}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.{Fixtures, Mocks}
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfter
import org.scalatest.concurrent.Eventually
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.Await
import scala.concurrent.duration._

// scalastyle:off file.size.limit
class SyncControllerSpec extends AnyFlatSpec with Matchers with BeforeAndAfter with MockFactory with Eventually {

  implicit var system: ActorSystem = _

  before {
    system = ActorSystem("SyncControllerSpec_System")
  }

  after {
    Await.result(system.terminate(), 10.seconds)
  }

  "SyncController" should "download pivot block and request block headers" in new TestSetup() {
    syncController ! SyncProtocol.Start

    val handshakedPeers = HandshakedPeers(twoAcceptedPeers)

    setupAutoPilot(etcPeerManager, handshakedPeers, defaultPivotBlockHeader, BlockchainData(Seq()))

    eventually(timeout = eventuallyTimeOut) {
      val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get
      syncState.bestBlockHeaderNumber shouldBe 0
      syncState.pivotBlock == defaultPivotBlockHeader
    }
  }

  it should "download better pivot block, request state, blocks and finish when downloaded" in new TestSetup() {
    startWithState(defaultStateBeforeNodeRestart)

    syncController ! SyncProtocol.Start

    val handshakedPeers = HandshakedPeers(singlePeer)

    val newBlocks =
      getHeaders(defaultStateBeforeNodeRestart.bestBlockHeaderNumber + 1, syncConfig.blockHeadersPerRequest)

    setupAutoPilot(etcPeerManager, handshakedPeers, defaultPivotBlockHeader, BlockchainData(newBlocks))

    val watcher = TestProbe()
    watcher.watch(syncController)

    eventually(timeout = eventuallyTimeOut) {
      //switch to regular download
      val children = syncController.children
      assert(storagesInstance.storages.appStateStorage.isFastSyncDone())
      assert(children.exists(ref => ref.path.name == "regular-sync"))
      assert(blockchain.getBestBlockNumber() == defaultPivotBlockHeader.number)
    }
  }

  it should "gracefully handle receiving empty receipts while syncing" in new TestSetup() {
    startWithState(defaultStateBeforeNodeRestart)

    syncController ! SyncProtocol.Start

    val handshakedPeers = HandshakedPeers(singlePeer)
    val watcher = TestProbe()
    watcher.watch(syncController)

    val newBlocks =
      getHeaders(defaultStateBeforeNodeRestart.bestBlockHeaderNumber + 1, syncConfig.blockHeadersPerRequest)

    setupAutoPilot(
      etcPeerManager,
      handshakedPeers,
      defaultPivotBlockHeader,
      BlockchainData(newBlocks),
      failedReceiptsTries = 1
    )

    eventually(timeout = eventuallyTimeOut) {
      assert(storagesInstance.storages.appStateStorage.isFastSyncDone())
      //switch to regular download
      val children = syncController.children
      assert(children.exists(ref => ref.path.name == "regular-sync"))
      assert(blockchain.getBestBlockNumber() == defaultPivotBlockHeader.number)
    }
  }

  it should "handle blocks that fail validation" in new TestSetup(
    _validators = new Mocks.MockValidatorsAlwaysSucceed {
      override val blockHeaderValidator: BlockHeaderValidator = { (blockHeader, getBlockHeaderByHash) =>
        Left(HeaderPoWError)
      }
    }
  ) {
    startWithState(
      defaultStateBeforeNodeRestart.copy(nextBlockToFullyValidate =
        defaultStateBeforeNodeRestart.bestBlockHeaderNumber + 1
      )
    )

    syncController ! SyncProtocol.Start

    val handshakedPeers = HandshakedPeers(singlePeer)

    val newBlocks =
      getHeaders(defaultStateBeforeNodeRestart.bestBlockHeaderNumber + 1, syncConfig.blockHeadersPerRequest)

    setupAutoPilot(etcPeerManager, handshakedPeers, defaultPivotBlockHeader, BlockchainData(newBlocks), 0, 0)

    val watcher = TestProbe()
    watcher.watch(syncController)

    eventually(timeout = eventuallyTimeOut) {
      val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get
      syncState.bestBlockHeaderNumber shouldBe (defaultStateBeforeNodeRestart.bestBlockHeaderNumber - syncConfig.fastSyncBlockValidationN)
      syncState.nextBlockToFullyValidate shouldBe (defaultStateBeforeNodeRestart.bestBlockHeaderNumber - syncConfig.fastSyncBlockValidationN + 1)
      syncState.blockBodiesQueue.isEmpty shouldBe true
      syncState.receiptsQueue.isEmpty shouldBe true
    }
  }

  it should "rewind fast-sync state if received header have no known parent" in new TestSetup() {
    startWithState(defaultStateBeforeNodeRestart)

    syncController ! SyncProtocol.Start

    val handshakedPeers = HandshakedPeers(singlePeer)

    val newBlocks = Seq(
      defaultPivotBlockHeader.copy(
        number = defaultStateBeforeNodeRestart.bestBlockHeaderNumber + 1,
        parentHash = ByteString(1, 2, 3)
      )
    )

    setupAutoPilot(etcPeerManager, handshakedPeers, defaultPivotBlockHeader, BlockchainData(newBlocks))

    val watcher = TestProbe()
    watcher.watch(syncController)

    eventually(timeout = eventuallyTimeOut) {
      val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get
      syncState.bestBlockHeaderNumber shouldBe (defaultStateBeforeNodeRestart.bestBlockHeaderNumber - syncConfig.fastSyncBlockValidationN)
      syncState.nextBlockToFullyValidate shouldBe (defaultStateBeforeNodeRestart.bestBlockHeaderNumber - syncConfig.fastSyncBlockValidationN + 1)
      syncState.blockBodiesQueue.isEmpty shouldBe true
      syncState.receiptsQueue.isEmpty shouldBe true
    }
  }

  it should "not change best block after receiving faraway block" in new TestSetup {

    startWithState(defaultStateBeforeNodeRestart)

    syncController ! SyncProtocol.Start

    val handshakedPeers = HandshakedPeers(singlePeer)
    val watcher = TestProbe()
    watcher.watch(syncController)

    val newBlocks =
      getHeaders(defaultStateBeforeNodeRestart.bestBlockHeaderNumber + 1, syncConfig.blockHeadersPerRequest)

    setupAutoPilot(etcPeerManager, handshakedPeers, defaultPivotBlockHeader, BlockchainData(newBlocks))
    val fast = syncController.getSingleChild("fast-sync")

    // Send block that is way forward, we should ignore that block and blacklist that peer
    val futureHeaders = Seq(defaultPivotBlockHeader.copy(number = defaultPivotBlockHeader.number + 20))
    val futureHeadersMessage = PeerRequestHandler.ResponseReceived(peer1, BlockHeaders(futureHeaders), 2L)
    implicit val ec = system.dispatcher
    system.scheduler.scheduleAtFixedRate(0.seconds, 0.1.second, fast, futureHeadersMessage)

    eventually(timeout = eventuallyTimeOut) {
      storagesInstance.storages.fastSyncStateStorage.getSyncState().get.pivotBlock shouldBe defaultPivotBlockHeader
    }

    // even though we receive this future headers fast sync should finish
    eventually(timeout = eventuallyTimeOut) {
      assert(storagesInstance.storages.appStateStorage.isFastSyncDone())
    }
  }

  it should "update pivot block if pivot fail" in new TestSetup(_validators = new Mocks.MockValidatorsAlwaysSucceed {
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
    startWithState(defaultStateBeforeNodeRestart)

    syncController ! SyncProtocol.Start

    val handshakedPeers = HandshakedPeers(singlePeer)

    val newPivot = defaultPivotBlockHeader.copy(number = defaultPivotBlockHeader.number + 20)
    val peerWithNewPivot = defaultPeer1Info.copy(maxBlockNumber = bestBlock + 20)
    val newHanshaked = HandshakedPeers(Map(peer1 -> peerWithNewPivot))

    val newBest = 399500 + 9

    val newBlocks =
      getHeaders(defaultStateBeforeNodeRestart.bestBlockHeaderNumber + 1, syncConfig.blockHeadersPerRequest)

    val autopilot = setupAutoPilot(etcPeerManager, handshakedPeers, defaultPivotBlockHeader, BlockchainData(newBlocks))

    eventually(timeout = eventuallyTimeOut) {
      storagesInstance.storages.fastSyncStateStorage.getSyncState().get.pivotBlock shouldBe defaultPivotBlockHeader
    }

    autopilot.updateAutoPilot(newHanshaked, newPivot, BlockchainData(newBlocks))

    val watcher = TestProbe()
    watcher.watch(syncController)

    eventually(timeout = eventuallyTimeOut) {
      val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get
      syncState.pivotBlock shouldBe newPivot
      syncState.safeDownloadTarget shouldEqual newPivot.number + syncConfig.fastSyncBlockValidationX
      syncState.blockBodiesQueue.isEmpty shouldBe true
      syncState.receiptsQueue.isEmpty shouldBe true
      syncState.bestBlockHeaderNumber shouldBe (newBest - syncConfig.fastSyncBlockValidationN)
    }
  }

  it should "not process, out of date new pivot block" in new TestSetup() {
    startWithState(defaultStateBeforeNodeRestart)
    syncController ! SyncProtocol.Start

    val staleNewPeer1Info = defaultPeer1Info.copy(maxBlockNumber = bestBlock - 2)
    val staleHeader = defaultPivotBlockHeader.copy(number = defaultPivotBlockHeader.number - 2)
    val staleHandshakedPeers = HandshakedPeers(Map(peer1 -> staleNewPeer1Info))

    val freshHeader = defaultPivotBlockHeader
    val freshPeerInfo1 = defaultPeer1Info
    val freshHandshakedPeers = HandshakedPeers(Map(peer1 -> freshPeerInfo1))

    val watcher = TestProbe()
    watcher.watch(syncController)

    val newBlocks =
      getHeaders(defaultStateBeforeNodeRestart.bestBlockHeaderNumber + 1, syncConfig.blockHeadersPerRequest)

    val pilot =
      setupAutoPilot(etcPeerManager, staleHandshakedPeers, staleHeader, BlockchainData(newBlocks), onlyPivot = true)

    eventually(timeout = eventuallyTimeOut) {
      storagesInstance.storages.fastSyncStateStorage.getSyncState().get.pivotBlockUpdateFailures shouldBe 1
    }

    pilot.updateAutoPilot(freshHandshakedPeers, freshHeader, BlockchainData(newBlocks), onlyPivot = true)

    eventually(timeout = eventuallyTimeOut) {
      storagesInstance.storages.fastSyncStateStorage.getSyncState().get.pivotBlock shouldBe defaultPivotBlockHeader
    }
  }

  it should "start state download only when pivot block is fresh enough" in new TestSetup() {
    startWithState(defaultStateBeforeNodeRestart)
    syncController ! SyncProtocol.Start

    val freshHeader = defaultPivotBlockHeader.copy(number = defaultPivotBlockHeader.number + 9)
    val freshPeerInfo1 = defaultPeer1Info.copy(maxBlockNumber = bestBlock + 9)
    val freshHandshakedPeers = HandshakedPeers(Map(peer1 -> freshPeerInfo1))

    val watcher = TestProbe()
    watcher.watch(syncController)

    val newBlocks = getHeaders(defaultStateBeforeNodeRestart.bestBlockHeaderNumber + 1, 50)

    val pilot = setupAutoPilot(etcPeerManager, freshHandshakedPeers, freshHeader, BlockchainData(newBlocks))
    eventually(timeout = longeventuallyTimeOut) {
      storagesInstance.storages.fastSyncStateStorage
        .getSyncState()
        .get
        .bestBlockHeaderNumber shouldBe freshHeader.number + syncConfig.fastSyncBlockValidationX
    }

    val freshHeader1 = defaultPivotBlockHeader.copy(number = defaultPivotBlockHeader.number + 19)
    val freshPeerInfo1a = defaultPeer1Info.copy(maxBlockNumber = bestBlock + 19)
    val freshHandshakedPeers1 = HandshakedPeers(Map(peer1 -> freshPeerInfo1a))

    // set up new received header previously received header will need update
    pilot.updateAutoPilot(freshHandshakedPeers1, freshHeader1, BlockchainData(newBlocks))

    eventually(timeout = longeventuallyTimeOut) {
      storagesInstance.storages.fastSyncStateStorage
        .getSyncState()
        .get
        .bestBlockHeaderNumber shouldBe freshHeader1.number + syncConfig.fastSyncBlockValidationX
    }

    eventually(timeout = longeventuallyTimeOut) {
      assert(storagesInstance.storages.appStateStorage.isFastSyncDone())
      //switch to regular download
      val children = syncController.children
      assert(children.exists(ref => ref.path.name == "regular-sync"))
      assert(blockchain.getBestBlockNumber() == freshHeader1.number)
    }
  }

  it should "re-enqueue block bodies when empty response is received" in new TestSetup {

    startWithState(defaultStateBeforeNodeRestart)

    syncController ! SyncProtocol.Start

    val handshakedPeers = HandshakedPeers(singlePeer)
    val watcher = TestProbe()
    watcher.watch(syncController)

    val newBlocks =
      getHeaders(defaultStateBeforeNodeRestart.bestBlockHeaderNumber + 1, syncConfig.blockHeadersPerRequest)

    setupAutoPilot(
      etcPeerManager,
      handshakedPeers,
      defaultPivotBlockHeader,
      BlockchainData(newBlocks),
      failedBodiesTries = 1
    )

    eventually(timeout = eventuallyTimeOut) {
      assert(storagesInstance.storages.appStateStorage.isFastSyncDone())
      //switch to regular download
      val children = syncController.children
      assert(children.exists(ref => ref.path.name == "regular-sync"))
      assert(blockchain.getBestBlockNumber() == defaultPivotBlockHeader.number)
    }
  }

  it should "update pivot block during state sync if it goes stale" in new TestSetup() {
    startWithState(defaultStateBeforeNodeRestart)

    syncController ! SyncProtocol.Start

    val handshakedPeers = HandshakedPeers(singlePeer)

    val newBlocks =
      getHeaders(defaultStateBeforeNodeRestart.bestBlockHeaderNumber + 1, 50)

    val pilot = setupAutoPilot(
      etcPeerManager,
      handshakedPeers,
      defaultPivotBlockHeader,
      BlockchainData(newBlocks),
      failedNodeRequest = true
    )

    // choose first pivot and as it is fresh enough start state sync
    eventually(timeout = eventuallyTimeOut) {
      val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get
      syncState.isBlockchainWorkFinished shouldBe true
      syncState.updatingPivotBlock shouldBe false
      stateDownloadStarted shouldBe true
    }
    val peerWithBetterBlock = defaultPeer1Info.copy(maxBlockNumber = bestBlock + syncConfig.maxPivotBlockAge)
    val newHandshakedPeers = HandshakedPeers(Map(peer1 -> peerWithBetterBlock))
    val newPivot = defaultPivotBlockHeader.copy(number = defaultPivotBlockHeader.number + syncConfig.maxPivotBlockAge)

    pilot.updateAutoPilot(
      newHandshakedPeers,
      newPivot,
      BlockchainData(newBlocks),
      failedNodeRequest = true
    )

    // sync to new pivot
    eventually(timeout = eventuallyTimeOut) {
      val syncState = storagesInstance.storages.fastSyncStateStorage.getSyncState().get
      syncState.pivotBlock shouldBe newPivot
    }

    // enable peer to respond with mpt nodes
    pilot.updateAutoPilot(newHandshakedPeers, newPivot, BlockchainData(newBlocks))

    val watcher = TestProbe()
    watcher.watch(syncController)

    eventually(timeout = longeventuallyTimeOut) {
      //switch to regular download
      val children = syncController.children
      assert(storagesInstance.storages.appStateStorage.isFastSyncDone())
      assert(children.exists(ref => ref.path.name == "regular-sync"))
      assert(blockchain.getBestBlockNumber() == newPivot.number)
    }
  }

  class TestSetup(
      blocksForWhichLedgerFails: Seq[BigInt] = Nil,
      _validators: Validators = new Mocks.MockValidatorsAlwaysSucceed
  ) extends EphemBlockchainTestSetup
      with TestSyncPeers
      with TestSyncConfig {

    @volatile
    var stateDownloadStarted = false

    val eventuallyTimeOut: Timeout = Timeout(Span(10, Seconds))
    val longeventuallyTimeOut = Timeout(Span(30, Seconds))
    //+ cake overrides
    override implicit lazy val system: ActorSystem = SyncControllerSpec.this.system

    override lazy val vm: VMImpl = new VMImpl

    override lazy val validators: Validators = _validators

    override lazy val consensus: TestConsensus = buildTestConsensus().withValidators(validators)

    override lazy val ledger: Ledger = mock[Ledger]
    //+ cake overrides

    val etcPeerManager = TestProbe()
    val peerMessageBus = TestProbe()
    val pendingTransactionsManager = TestProbe()

    val checkpointBlockGenerator = new CheckpointBlockGenerator()

    val ommersPool = TestProbe()

    override def defaultSyncConfig: SyncConfig = super.defaultSyncConfig.copy(
      doFastSync = true,
      branchResolutionRequestSize = 20,
      checkForNewBlockInterval = 1.second,
      blockHeadersPerRequest = 10,
      blockBodiesPerRequest = 10,
      minPeersToChoosePivotBlock = 1,
      peersScanInterval = 1.second,
      redownloadMissingStateNodes = false,
      fastSyncBlockValidationX = 10,
      blacklistDuration = 1.second,
      peerResponseTimeout = 2.seconds,
      persistStateSnapshotInterval = 0.1.seconds,
      fastSyncThrottle = 10.milliseconds,
      maxPivotBlockAge = 30
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
          checkpointBlockGenerator,
          ommersPool.ref,
          etcPeerManager.ref,
          syncConfig,
          externalSchedulerOpt = Some(system.scheduler)
        )
      )
    )

    val EmptyTrieRootHash: ByteString = Account.EmptyStorageRootHash
    val baseBlockHeader = Fixtures.Blocks.Genesis.header

    blockchain.storeChainWeight(baseBlockHeader.parentHash, ChainWeight.zero).commit()

    val startDelayMillis = 200

    case class BlockchainData(
        headers: Map[BigInt, BlockHeader],
        bodies: Map[ByteString, BlockBody],
        receipts: Map[ByteString, Seq[Receipt]]
    )
    object BlockchainData {
      def apply(headers: Seq[BlockHeader]): BlockchainData = {
        // assumes headers are correct chain
        headers.foldLeft(new BlockchainData(Map.empty, Map.empty, Map.empty)) { (state, header) =>
          state.copy(
            headers = state.headers + (header.number -> header),
            bodies = state.bodies + (header.hash -> BlockBody.empty),
            receipts = state.receipts + (header.hash -> Seq.empty)
          )
        }
      }
    }
    // scalastyle:off method.length
    case class SyncStateAutoPilot(
        handshakedPeers: HandshakedPeers,
        pivotHeader: BlockHeader,
        blockchainData: BlockchainData,
        failedReceiptsTries: Int,
        failedBodiesTries: Int,
        onlyPivot: Boolean,
        failedNodeRequest: Boolean,
        autoPilotProbeRef: ActorRef
    ) extends AutoPilot {
      override def run(sender: ActorRef, msg: Any): AutoPilot = {
        msg match {
          case EtcPeerManagerActor.GetHandshakedPeers =>
            sender ! handshakedPeers
            this

          case SendMessage(msg: GetBlockHeadersEnc, peer) =>
            val underlyingMessage = msg.underlyingMsg
            if (underlyingMessage.maxHeaders == 1) {
              // pivot block
              sender ! MessageFromPeer(BlockHeaders(Seq(pivotHeader)), peer)
            } else {
              if (!onlyPivot) {
                val start = msg.underlyingMsg.block.swap.toOption.get
                val stop = start + msg.underlyingMsg.maxHeaders
                val headers = (start until stop).flatMap(i => blockchainData.headers.get(i))
                sender ! MessageFromPeer(BlockHeaders(headers), peer)
              }
            }
            this

          case SendMessage(msg: GetReceiptsEnc, peer) if !onlyPivot =>
            val underlyingMessage = msg.underlyingMsg
            if (failedReceiptsTries > 0) {
              sender ! MessageFromPeer(Receipts(Seq()), peer)
              this.copy(failedReceiptsTries = failedReceiptsTries - 1)
            } else {
              val rec = msg.underlyingMsg.blockHashes.flatMap(h => blockchainData.receipts.get(h))
              sender ! MessageFromPeer(Receipts(rec), peer)
              this
            }

          case SendMessage(msg: GetBlockBodiesEnc, peer) if !onlyPivot =>
            val underlyingMessage = msg.underlyingMsg
            if (failedBodiesTries > 0) {
              sender ! MessageFromPeer(BlockBodies(Seq()), peer)
              this.copy(failedBodiesTries = failedBodiesTries - 1)
            } else {
              val bod = msg.underlyingMsg.hashes.flatMap(h => blockchainData.bodies.get(h))
              sender ! MessageFromPeer(BlockBodies(bod), peer)
              this
            }

          case SendMessage(msg: GetNodeDataEnc, peer) if !onlyPivot =>
            stateDownloadStarted = true
            val underlyingMessage = msg.underlyingMsg
            if (!failedNodeRequest) {
              sender ! MessageFromPeer(NodeData(Seq(defaultStateMptLeafWithAccount)), peer)
            }
            this

          case AutoPilotUpdateData(peers, pivot, data, failedReceipts, failedBodies, onlyPivot, failedNode) =>
            sender ! DataUpdated
            this.copy(peers, pivot, data, failedReceipts, failedBodies, onlyPivot, failedNode)
        }
      }

      def updateAutoPilot(
          handshakedPeers: HandshakedPeers,
          pivotHeader: BlockHeader,
          blockchainData: BlockchainData,
          failedReceiptsTries: Int = 0,
          failedBodiesTries: Int = 0,
          onlyPivot: Boolean = false,
          failedNodeRequest: Boolean = false
      ): Unit = {
        val sender = TestProbe()
        autoPilotProbeRef.tell(
          AutoPilotUpdateData(
            handshakedPeers,
            pivotHeader,
            blockchainData,
            failedReceiptsTries,
            failedBodiesTries,
            onlyPivot,
            failedNodeRequest
          ),
          sender.ref
        )
        sender.expectMsg(DataUpdated)
      }
    }

    // scalastyle:off method.length parameter.number
    def setupAutoPilot(
        testProbe: TestProbe,
        handshakedPeers: HandshakedPeers,
        pivotHeader: BlockHeader,
        blockchainData: BlockchainData,
        failedReceiptsTries: Int = 0,
        failedBodiesTries: Int = 0,
        onlyPivot: Boolean = false,
        failedNodeRequest: Boolean = false
    ): SyncStateAutoPilot = {
      val autopilot = SyncStateAutoPilot(
        handshakedPeers,
        pivotHeader,
        blockchainData,
        failedReceiptsTries,
        failedBodiesTries,
        onlyPivot,
        failedNodeRequest,
        testProbe.ref
      )
      testProbe.setAutoPilot(autopilot)
      autopilot
    }

    case class AutoPilotUpdateData(
        handshakedPeers: HandshakedPeers,
        pivotHeader: BlockHeader,
        blockchainData: BlockchainData,
        failedReceiptsTries: Int = 0,
        failedBodiesTries: Int = 0,
        onlyPivot: Boolean = false,
        failedNodeRequest: Boolean = false
    )
    case object DataUpdated

    val defaultExpectedPivotBlock = 399500

    val defaultSafeDownloadTarget = defaultExpectedPivotBlock

    val defaultBestBlock = defaultExpectedPivotBlock - 1

    val defaultStateRoot = "deae1dfad5ec8dcef15915811e1f044d2543674fd648f94345231da9fc2646cc"

    val defaultPivotBlockHeader =
      baseBlockHeader.copy(number = defaultExpectedPivotBlock, stateRoot = ByteString(Hex.decode(defaultStateRoot)))

    val defaultState =
      SyncState(
        defaultPivotBlockHeader,
        safeDownloadTarget = defaultSafeDownloadTarget,
        bestBlockHeaderNumber = defaultBestBlock
      )

    val defaultStateMptLeafWithAccount =
      ByteString(
        Hex.decode(
          "f86d9e328415c225a782bb339b22acad1c739e42277bc7ef34de3623114997ce78b84cf84a0186cb7d8738d800a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
        )
      )

    val beforeRestartPivot = defaultPivotBlockHeader.copy(number = defaultExpectedPivotBlock - 1)
    val defaultStateBeforeNodeRestart = defaultState.copy(
      pivotBlock = beforeRestartPivot,
      bestBlockHeaderNumber = defaultExpectedPivotBlock,
      nextBlockToFullyValidate = beforeRestartPivot.number + syncConfig.fastSyncBlockValidationX
    )

    def getHeaders(from: BigInt, number: BigInt): Seq[BlockHeader] = {
      val headers = (from until from + number).toSeq.map { nr =>
        defaultPivotBlockHeader.copy(number = nr)
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

    def startWithState(state: SyncState): Unit = {
      storagesInstance.storages.fastSyncStateStorage.putSyncState(state)
    }

    def persistState(): Unit = {
      Thread.sleep(300)
      syncController.getSingleChild("fast-sync") ! FastSync.PersistSyncState
      Thread.sleep(300)
    }
  }
}
