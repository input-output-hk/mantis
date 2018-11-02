package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.blockchain.sync.FastSync.{ ImportedLastBlock, LastBlockValidationFailed, ProcessSyncing, UpdateTargetBlock }
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderParentNotFoundError
import io.iohk.ethereum.consensus.validators.{ BlockHeaderValidator, Validators }
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.Config.SyncConfig
import org.scalamock.handlers.CallHandler1
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
        val (resultHandler, resultMsg) = handleBlockHeaders(peer1, headers.reverse, baseHandlerState, discardLastBlocks, blacklist)
        resultHandler shouldBe baseHandlerState
        resultMsg shouldBe ProcessSyncing
      }

      "stop processing headers when parent difficulty not found" in {
        val firstBlock: BlockHeader = blockHeader1
        // Process first block
        val parentDifficulty: BigInt = firstBlock.difficulty + defaultTargetBlock.difficulty
        (blockchain.getTotalDifficultyByHash _).expects(firstBlock.parentHash).returning(Some(parentDifficulty))

        toMockFunction1[BlockHeader, Unit](blockchain.save(_: BlockHeader)).expects(firstBlock)
        toMockFunction2[ByteString, BigInt, Unit](blockchain.save(_: ByteString, _: BigInt))
          .expects(firstBlock.hash, firstBlock.difficulty + parentDifficulty)

        // Stop processing blocks at second block
        val secondBlock = blockHeader2
        (blockchain.getTotalDifficultyByHash _).expects(secondBlock.parentHash).returning(None)
        (log.debug: (String, Any) => Unit).expects(*, *).returning(()).once()
        val (resultHandler, resultMsg) = handleBlockHeaders(peer1, headers, baseHandlerState, discardLastBlocks, blacklist)
        resultHandler.syncState.receiptsQueue shouldBe blockHeader1ExpectedInQueue
        resultHandler.syncState.blockBodiesQueue shouldBe blockHeader1ExpectedInQueue
        resultMsg shouldBe ProcessSyncing
      }

      "finish processing headers if headers list is empty" in {
        val (resultHandler, resultMsg) = handleBlockHeaders(peer1, Seq.empty, baseHandlerState, discardLastBlocks, blacklist)
        resultHandler shouldBe baseHandlerState
        resultMsg shouldBe ProcessSyncing
      }

      "update target block when imported target block" in {
        val parentDifficulty: BigInt = blockHeader1.difficulty + defaultTargetBlock.difficulty
        (blockchain.getTotalDifficultyByHash _).expects(blockHeader1.parentHash).returning(Some(parentDifficulty))

        toMockFunction1[BlockHeader, Unit](blockchain.save(_: BlockHeader)).expects(blockHeader1)
        toMockFunction2[ByteString, BigInt, Unit](blockchain.save(_: ByteString, _: BigInt))
          .expects(blockHeader1.hash, blockHeader1.difficulty + parentDifficulty)

        val hs = baseHandlerState.copy(syncState = baseSyncState.copy(safeDownloadTarget = blockHeader1.number))
        val (resultHandler, resultMsg) = handleBlockHeaders(peer1, Seq(blockHeader1), hs, discardLastBlocks, blacklist)
        resultHandler.syncState.receiptsQueue shouldBe blockHeader1ExpectedInQueue
        resultHandler.syncState.blockBodiesQueue shouldBe blockHeader1ExpectedInQueue
        resultMsg shouldBe UpdateTargetBlock(ImportedLastBlock)
      }

      "handle block validation error if validation failed" when {

        "header number is greater than safe download target" in {
          prepareValidationFailed

          val syncState = FastSyncState(baseBlockHeader).copy(
            nextBlockToFullyValidate = blockHeader2.number,
            safeDownloadTarget = blockHeader2.number - 1
          )
          val handlerState = baseHandlerState.copy(syncState = syncState)
          val (resultHandler, resultMsg) =
            handleBlockHeaders(peer1, Seq(blockHeader2), handlerState, discardLastBlocks, blacklist)

          resultHandler shouldBe handlerState
          resultMsg shouldBe ProcessSyncing
        }

        "header number is less than safe download target and less than target block number" in {
          prepareValidationFailed

          val syncState = FastSyncState(baseBlockHeader).copy(
            nextBlockToFullyValidate = blockHeader2.number,
            safeDownloadTarget = blockHeader2.number + 1,
            targetBlock = defaultTargetBlock.copy(number = blockHeader2.number + 1)
          )
          val handlerState = baseHandlerState.copy(syncState = syncState)
          val (resultHandler, resultMsg) =
            handleBlockHeaders(peer1, Seq(blockHeader2), handlerState, discardLastBlocks, blacklist)

          val expectedNextBlockToFullyValidate = blockHeader2.number - syncConfig.fastSyncBlockValidationN
          val expectedBestBlockNumber = expectedNextBlockToFullyValidate - 1

          resultHandler should not equal handlerState
          resultHandler.syncState.bestBlockHeaderNumber shouldBe expectedBestBlockNumber.max(0)
          resultHandler.syncState.nextBlockToFullyValidate shouldBe expectedNextBlockToFullyValidate.max(0)
          resultMsg shouldBe ProcessSyncing
        }

        "header number is less than safe download target but greater than target block number" in {
          prepareValidationFailed

          val syncState = FastSyncState(baseBlockHeader).copy(
            nextBlockToFullyValidate = blockHeader2.number,
            safeDownloadTarget = blockHeader2.number + 1,
            targetBlock = defaultTargetBlock.copy(number = blockHeader2.number - 1)
          )
          val handlerState = baseHandlerState.copy(syncState = syncState)
          val (resultHandler, resultMsg) =
            handleBlockHeaders(peer1, Seq(blockHeader2), handlerState, discardLastBlocks, blacklist)

          val expectedNextBlockToFullyValidate = blockHeader2.number - syncConfig.fastSyncBlockValidationN
          val expectedBestBlockNumber = expectedNextBlockToFullyValidate - 1

          resultHandler should not equal handlerState
          resultHandler.syncState.bestBlockHeaderNumber shouldBe expectedBestBlockNumber.max(0)
          resultHandler.syncState.nextBlockToFullyValidate shouldBe expectedNextBlockToFullyValidate.max(0)
          resultMsg shouldBe UpdateTargetBlock(LastBlockValidationFailed)
        }
      }
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
  val baseSyncState: FastSyncState = FastSyncState(baseBlockHeader).copy(
    nextBlockToFullyValidate = defaultSafeDownloadTarget + 2, // When this is set up it won't be validating the header
    safeDownloadTarget = defaultSafeDownloadTarget + 2        // Not a target block
  )
  val baseHandlerState = FastSyncHandlerState(baseSyncState, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  def blacklist(id: BlackListId, duration: FiniteDuration, string: String): Unit = ()
  def discardLastBlocks(startBlock: BigInt, blocksToDiscard: Int): AppStateStorage = appStateStorage

  val hash1: ByteString = byteStringOfLengthNGen(32).sample.get
  val blockHeader1: BlockHeader = baseBlockHeader.copy(number = defaultBestBlock)

  val hash2: ByteString = byteStringOfLengthNGen(32).sample.get
  val blockHeader2: BlockHeader = baseBlockHeader.copy(number = defaultBestBlock + 1, parentHash = blockHeader1.hash)

  val emptyBlockBody: BlockBody = BlockBody.empty

  val requestedHashes = Seq(hash1, hash2)
  val bodies = Seq(emptyBlockBody, emptyBlockBody)
  val headers = Seq(blockHeader1, blockHeader2)
  override def afterAll(): Unit = system.terminate()

  val blockHeader1ExpectedInQueue = Seq(blockHeader1.hash)

  def prepareValidationFailed: CallHandler1[String, Unit] = {
    (blockHeaderValidator.validate _).expects(blockHeader2, *).returning(Left(HeaderParentNotFoundError))
    (validators.blockHeaderValidator _).expects().returning(blockHeaderValidator).once()
    (log.warning: String => Unit).expects(*)
  }

}
