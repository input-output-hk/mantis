package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.{ BlockHeader, Blockchain }
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.Config.SyncConfig
import org.scalamock.function.{ MockFunction1, MockFunction3 }
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ BeforeAndAfterAll, Matchers, WordSpec }

import scala.concurrent.duration._

// scalastyle:off magic.number
trait FastSyncHandlersSetup extends WordSpec
  with MockFactory
  with Matchers
  with SyncFixtures
  with BeforeAndAfterAll {

  val log: LoggingAdapter = mock[LoggingAdapter]
  val blockchain: Blockchain = mock[FakeBlockchain]
  val validators: Validators = mock[Validators]
  val syncConfig = SyncConfig(
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

  implicit lazy val system: ActorSystem = ActorSystem("Handler_System")
  val peer1: Peer = mkPeer(1, TestProbe())
  val baseSyncState: FastSyncState = FastSyncState(baseBlockHeader).copy(
    nextBlockToFullyValidate = defaultSafeDownloadTarget + 2, // When this is set up it won't be validating the header
    safeDownloadTarget = defaultSafeDownloadTarget + 2        // Not a target block
  )
  val baseHandlerState = FastSyncHandlerState(baseSyncState)

  val blockHeader1: BlockHeader = baseBlockHeader.copy(number = defaultBestBlock)
  val hash1: ByteString = blockHeader1.hash
  val blockHeader2: BlockHeader = baseBlockHeader.copy(number = defaultBestBlock + 1, parentHash = hash1)
  val hash2: ByteString = blockHeader2.hash

  val emptyBlockBody: BlockBody = BlockBody.empty

  val requestedHashes = Seq(hash1, hash2)
  val bodies = Seq(emptyBlockBody, emptyBlockBody)
  val headers = Seq(blockHeader1, blockHeader2)

  val blacklist: MockFunction3[BlackListId, FiniteDuration, String, Unit] =
    mockFunction[BlackListId, FiniteDuration, String, Unit]
  val updateBestBlock: MockFunction1[Seq[ByteString], Unit] = mockFunction[Seq[ByteString], Unit]

  override def afterAll(): Unit = system.terminate()

}
