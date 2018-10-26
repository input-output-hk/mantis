package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.blockchain.sync.FastSync.ProcessSyncing
import io.iohk.ethereum.consensus.validators.{ BlockHeaderValidator, Validators }
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.Config.SyncConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ BeforeAndAfterAll, Matchers, WordSpec }

import scala.concurrent.duration.{ FiniteDuration, _ }

// scalastyle:off magic.number
class FastSyncBlockHeadersHandlerSpec
  extends WordSpec
    with MockFactory
    with Matchers
    with FastSyncBlockHeadersHandler
    with SyncFixtures
    with ObjectGenerators
    with BeforeAndAfterAll {

  "FastSyncBlockHeadersHandler" when {

    "calling handleBlockHeaders" should {
      "blacklist peer if headers not in chain order" in {
        val (resultHandler, resultMsg) = handleBlockHeaders(peer1, headers.reverse, handlerState, discardLastBlocks, blacklist)
        resultHandler shouldBe handlerState
        resultMsg shouldBe ProcessSyncing
      }

      "stop processing headers when parent difficulty not found" in {
//      todo: resolve problem with mock not changing handlerState
//        val firstBlock: BlockHeader = blockHeader1
//        // Process first block
//        val parentDifficulty: BigInt = firstBlock.difficulty + defaultTargetBlock.difficulty
//        (blockchain.getTotalDifficultyByHash _).expects(firstBlock.parentHash).returning(Some(parentDifficulty))
//
//        toMockFunction1[BlockHeader, Unit](blockchain.save(_: BlockHeader)).expects(firstBlock)
//        toMockFunction2[ByteString, BigInt, Unit](blockchain.save(_: ByteString, _: BigInt))
//          .expects(firstBlock.hash, firstBlock.difficulty + parentDifficulty)
//
////        val expectedHandlerState: FastSyncHandlerState =
////          handlerState.updateBestBlockNumber(firstBlock, parentDifficulty, shouldUpdate = false, syncConfig)
//
//        // Stop processing blocks at second block
//        val secondBlock = blockHeader2
//        (blockchain.getTotalDifficultyByHash _).expects(secondBlock.parentHash).returning(None)
//        (log.debug: (String, Any) => Unit).expects(*, *).returning(()).once()
//        val (_, resultMsg) = handleBlockHeaders(peer1, headers, handlerState, discardLastBlocks, blacklist)
////        resultHandler shouldBe expectedHandlerState
//        resultMsg shouldBe ProcessSyncing
      }

      "finish processing headers if headers list is empty" in {

      }

      "update target block when imported target block" in {}

      "handle block validation error if validation failed" in {}
    }
  }

  override val log: LoggingAdapter = mock[LoggingAdapter]
  override val blockchain: Blockchain = mock[FakeBlockchain]
  val appStateStorage: AppStateStorage = mock[AppStateStorage]

  val blockHeaderValidator: BlockHeaderValidator = mock[BlockHeaderValidator]
  override val validators: Validators = mock[Validators]
  override val syncConfig = SyncConfig(
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
    maxTargetDifference = 5,
    maximumTargetUpdateFailures = 1
  )

  implicit lazy val system: ActorSystem = ActorSystem("BlockHeadersHandler_System")
  val peer1: Peer = mkPeer(1, TestProbe())
  // When this are set up it won't be validating the header
  val baseSyncState: FastSyncState = FastSyncState(baseBlockHeader).copy(
    nextBlockToFullyValidate = defaultSafeDownloadTarget + 2,
    safeDownloadTarget = defaultSafeDownloadTarget + 2
  )
  val handlerState = FastSyncHandlerState(baseSyncState, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  def blacklist(id: BlackListId, duration: FiniteDuration, string: String): Unit = ()
  def discardLastBlocks(startBlock: BigInt, blocksToDiscard: Int): AppStateStorage = appStateStorage

  val hash1: ByteString = byteStringOfLengthNGen(32).sample.get
  val blockHeader1: BlockHeader = baseBlockHeader.copy(number = defaultBestBlock)

  val hash2: ByteString = byteStringOfLengthNGen(32).sample.get
  val blockHeader2: BlockHeader = baseBlockHeader.copy(number = defaultBestBlock + 1, parentHash = hash1)

  val emptyBlockBody: BlockBody = BlockBody.empty

  val requestedHashes = Seq(hash1, hash2)
  val bodies = Seq(emptyBlockBody, emptyBlockBody)
  val headers = Seq(blockHeader1, blockHeader2)
  override def afterAll(): Unit = system.terminate()

}
