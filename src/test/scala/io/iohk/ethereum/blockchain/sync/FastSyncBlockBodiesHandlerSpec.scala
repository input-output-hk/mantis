package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.{ BlockReceiptsHashError, BlockValid }
import io.iohk.ethereum.consensus.validators.{ BlockValidator, Validators }
import io.iohk.ethereum.domain.{ BlockHeader, Blockchain }
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.Config.SyncConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ BeforeAndAfterAll, Matchers, WordSpec }

import scala.concurrent.duration._

// scalastyle:off magic.number
class FastSyncBlockBodiesHandlerSpec
  extends WordSpec
    with MockFactory
    with Matchers
    with FastSyncBlockBodiesHandler
    with SyncFixtures
    with ObjectGenerators
    with BeforeAndAfterAll {

  "FastSyncBlockBodiesHandler" should {
    "blacklist peer" when {
      "got empty block bodies response from known hashes" in {
        handlerState.syncState.blockBodiesQueue shouldBe Nil
        val result: FastSyncHandlerState =
          handleBlockBodies(peer1, requestedHashes, Seq.empty, handlerState, blacklist, updateBestBlockIfNeeded)
        result.syncState.blockBodiesQueue shouldBe requestedHashes
      }

      "got block bodies not matching block headers" in {
        (blockchain.getBlockHeaderByHash _).expects(hash1).returning(Some(blockHeader1))
        (blockchain.getBlockHeaderByHash _).expects(hash2).returning(Some(blockHeader2))
        (blockValidator.validateHeaderAndBody _).expects(blockHeader1, emptyBlockBody).returning(Right(BlockValid))
        (blockValidator.validateHeaderAndBody _).expects(blockHeader2, emptyBlockBody).returning(Left(BlockReceiptsHashError))
        (validators.blockValidator _).expects().returning(blockValidator).twice()

        handlerState.syncState.blockBodiesQueue shouldBe Nil

        val result: FastSyncHandlerState =
          handleBlockBodies(peer1, requestedHashes, bodies, handlerState, blacklist, updateBestBlockIfNeeded)
        result.syncState.blockBodiesQueue shouldBe requestedHashes
      }
    }

    "restart download process if got DbError from blocks validation" in {
      (blockchain.getBlockHeaderByHash _).expects(hash1).returning(Some(blockHeader1))
      (blockchain.getBlockHeaderByHash _).expects(hash2).returning(None)
      (blockValidator.validateHeaderAndBody _).expects(blockHeader1, emptyBlockBody).returning(Right(BlockValid))
      (validators.blockValidator _).expects().returning(blockValidator).once()
      (log.debug: String => Unit).expects(*).returning(())

      val expectedBestBlock: BigInt =
        (handlerState.syncState.bestBlockHeaderNumber - 2 * syncConfig.blockHeadersPerRequest).max(0)

      val result: FastSyncState =
        handleBlockBodies(peer1, requestedHashes, bodies, handlerState, blacklist, updateBestBlockIfNeeded).syncState
      result.blockBodiesQueue shouldBe Nil
      result.receiptsQueue shouldBe Nil
      result.bestBlockHeaderNumber shouldBe expectedBestBlock
    }

    "insert blocks if validation passes" in {
      (blockchain.getBlockHeaderByHash _).expects(hash1).returning(Some(blockHeader1))
      (blockchain.getBlockHeaderByHash _).expects(hash2).returning(Some(blockHeader2))
      (blockValidator.validateHeaderAndBody _).expects(blockHeader1, emptyBlockBody).returning(Right(BlockValid))
      (blockValidator.validateHeaderAndBody _).expects(blockHeader2, emptyBlockBody).returning(Right(BlockValid))
      (validators.blockValidator _).expects().returning(blockValidator).twice()

      // ### Compiler bug? ###
      // For some reason implicit resolution is not working well when we reference method
      // using 'methodName(_: Type1, _: Type2)' notation instead of 'methodName _',
      // which is required in this case since there are many overloaded 'save' methods in Blockchain trait.
      //
      // When '(blockchain.save(_: ByteString, _: BlockBody)).expects(...)' is called
      // 'org.scalamock.clazz.Mock.toMockFunction1' implicit conversion is incorrectly picked up instead of
      // 'org.scalamock.clazz.Mock.toMockFunction2' which leads to runtime exception during test
      // (in 'org.scalatest.OutcomeOf.outcomeOf') where MockFunction1 is casted to MockFunction2.
      toMockFunction2[ByteString, BlockBody, Unit](blockchain.save(_: ByteString, _: BlockBody))
        .expects(*, emptyBlockBody).twice()

      val result: FastSyncHandlerState =
        handleBlockBodies(peer1, requestedHashes, bodies, handlerState, blacklist, updateBestBlockIfNeeded)
      result shouldBe handlerState
    }

  }

  override val log: LoggingAdapter = mock[LoggingAdapter]
  override val blockchain: Blockchain = mock[FakeBlockchain]
  val blockValidator: BlockValidator = mock[BlockValidator]
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

  implicit lazy val system: ActorSystem = ActorSystem("BlockBodiesHandler_System")
  val peer1: Peer = mkPeer(1, TestProbe())
  val handlerState = FastSyncHandlerState(FastSyncState(baseBlockHeader))

  def blacklist(id: BlackListId, duration: FiniteDuration, string: String): Unit = ()
  def updateBestBlockIfNeeded(receivedHashes: Seq[ByteString]): Unit = ()

  val hash1: ByteString = byteStringOfLengthNGen(32).sample.get
  val blockHeader1: BlockHeader = baseBlockHeader.copy(number = defaultBestBlock)
  val hash2: ByteString = byteStringOfLengthNGen(32).sample.get

  val blockHeader2: BlockHeader = baseBlockHeader.copy(number = defaultBestBlock + 1)

  val emptyBlockBody: BlockBody = BlockBody.empty

  val requestedHashes = Seq(hash1, hash2)
  val bodies = Seq(emptyBlockBody, emptyBlockBody)

  override def afterAll(): Unit = system.terminate()

}
