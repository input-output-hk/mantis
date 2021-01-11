package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.testkit.{TestKit, TestProbe}
import akka.util.Timeout
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status.Progress
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.GenOps.GenOps
import io.iohk.ethereum.{FreeSpecBase, ObjectGenerators, SpecFixtures, WithActorSystemShutDown}
import monix.eval.Task
import monix.reactive.Observable
import io.iohk.ethereum.BlockHelpers
import io.iohk.ethereum.blockchain.sync.fast.FastSync
import io.iohk.ethereum.domain.ChainWeight

import scala.concurrent.duration.DurationInt

class FastSyncSpec
    extends TestKit(ActorSystem("FastSync_testing"))
    with FreeSpecBase
    with SpecFixtures
    with WithActorSystemShutDown { self =>
  implicit val timeout: Timeout = Timeout(30.seconds)

  class Fixture extends EphemBlockchainTestSetup with TestSyncConfig with TestSyncPeers {
    override implicit lazy val system = self.system
    override implicit val scheduler = self.scheduler

    override lazy val syncConfig: SyncConfig =
      defaultSyncConfig.copy(pivotBlockOffset = 5, fastSyncBlockValidationX = 5, fastSyncThrottle = 1.millis)
    lazy val (stateRoot, trieProvider) = {
      val stateNodesData = ObjectGenerators.genMultipleNodeData(20).pickValue

      lazy val trieProvider = StateSyncUtils.TrieProvider()
      lazy val stateRoot = trieProvider.buildWorld(stateNodesData)

      (stateRoot, trieProvider)
    }

    lazy val testBlocks = BlockHelpers.generateChain(
      20,
      BlockHelpers.genesis,
      block => block.copy(header = block.header.copy(stateRoot = stateRoot))
    )

    lazy val bestBlockAtStart = testBlocks(10)
    lazy val expectedPivotBlockNumber = bestBlockAtStart.number - syncConfig.pivotBlockOffset
    lazy val expectedTargetBlockNumber = expectedPivotBlockNumber + syncConfig.fastSyncBlockValidationX
    lazy val testPeers = twoAcceptedPeers.map { case (k, peerInfo) =>
      val lastBlock = bestBlockAtStart
      k -> peerInfo
        .withBestBlockData(lastBlock.number, lastBlock.hash)
        .copy(remoteStatus = peerInfo.remoteStatus.copy(bestHash = lastBlock.hash))
    }
    lazy val etcPeerManager =
      new EtcPeerManagerFake(
        syncConfig,
        testPeers,
        testBlocks,
        req => trieProvider.getNodes(req).map(_.data)
      )
    lazy val peerEventBus = TestProbe("peer_event-bus")
    lazy val fastSync = system.actorOf(
      FastSync.props(
        fastSyncStateStorage = storagesInstance.storages.fastSyncStateStorage,
        appStateStorage = storagesInstance.storages.appStateStorage,
        blockchain = blockchain,
        validators = validators,
        peerEventBus = peerEventBus.ref,
        etcPeerManager = etcPeerManager.ref,
        syncConfig = syncConfig,
        scheduler = system.scheduler
      )
    )

    val saveGenesis: Task[Unit] = Task {
      blockchain.save(BlockHelpers.genesis, receipts = Nil, ChainWeight.totalDifficultyOnly(1), saveAsBestBlock = true)
    }

    val startSync: Task[Unit] = Task { fastSync ! SyncProtocol.Start }

    val getSyncStatus: Task[Status] =
      Task.deferFuture((fastSync ? SyncProtocol.GetStatus).mapTo[Status])
  }

  override def createFixture(): Fixture = new Fixture

  "FastSync" - {
    "for reporting progress" - {
      "returns NotSyncing until pivot block is selected and first data being fetched" in testCaseM { fixture: Fixture =>
        import fixture._

        (for {
          _ <- startSync
          status <- getSyncStatus
        } yield assert(status === Status.NotSyncing)).timeout(timeout.duration)
      }

      "returns Syncing when pivot block is selected and started fetching data" in testCaseM { fixture: Fixture =>
        import fixture._

        (for {
          _ <- saveGenesis
          _ <- startSync
          _ <- etcPeerManager.onPeersConnected
          _ <- etcPeerManager.pivotBlockSelected.firstL
          _ <- etcPeerManager.fetchedHeaders.firstL
          status <- getSyncStatus
        } yield {
          status match {
            case Status.Syncing(startingBlockNumber, blocksProgress, stateNodesProgress) =>
              assert(startingBlockNumber === BigInt(0))
              assert(blocksProgress.target === expectedPivotBlockNumber)
              assert(stateNodesProgress === Some(Progress(0, 1)))
            case Status.NotSyncing | Status.SyncDone => fail("Expected syncing status")
          }
        })
          .timeout(timeout.duration)
      }

      "returns Syncing with block progress once both header and body is fetched" in testCaseM { fixture: Fixture =>
        import fixture._

        (for {
          _ <- saveGenesis
          _ <- startSync
          _ <- etcPeerManager.onPeersConnected
          _ <- etcPeerManager.pivotBlockSelected.firstL
          blocksBatch <- etcPeerManager.fetchedBlocks.firstL
          status <- getSyncStatus
          lastBlockFromBatch = blocksBatch.last.number
        } yield {
          status match {
            case Status.Syncing(startingBlockNumber, blocksProgress, stateNodesProgress) =>
              assert(startingBlockNumber === BigInt(0))
              assert(blocksProgress.current >= lastBlockFromBatch)
              assert(blocksProgress.target === expectedPivotBlockNumber)
              assert(stateNodesProgress === Some(Progress(0, 1)))
            case Status.NotSyncing | Status.SyncDone => fail("Expected other state")
          }
        })
          .timeout(timeout.duration)
      }

      "returns Syncing with state nodes progress" in customTestCaseM(new Fixture {
        override lazy val syncConfig =
          defaultSyncConfig.copy(
            peersScanInterval = 1.second,
            pivotBlockOffset = 5,
            fastSyncBlockValidationX = 1,
            fastSyncThrottle = 1.millis
          )
      }) { fixture: Fixture =>
        import fixture._

        (for {
          _ <- saveGenesis
          _ <- startSync
          _ <- etcPeerManager.onPeersConnected
          _ <- etcPeerManager.pivotBlockSelected.firstL
          _ <- Observable
            .interval(10.millis)
            .mapEval(_ => getSyncStatus)
            .collect {
              case stat @ Status.Syncing(_, Progress(current, _), _) if current >= expectedTargetBlockNumber => stat
            }
            .firstL
          _ <- Observable
            .interval(10.millis)
            .mapEval(_ => getSyncStatus)
            .collect {
              case stat @ Status.Syncing(_, _, Some(stateNodesProgress)) if stateNodesProgress.target > 1 =>
                stat
            }
            .firstL
        } yield succeed).timeout(timeout.duration)
      }
    }
  }
}
