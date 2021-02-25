package io.iohk.ethereum.sync

import com.typesafe.config.ConfigValueFactory
import io.iohk.ethereum.FreeSpecBase
import io.iohk.ethereum.metrics.{Metrics, MetricsConfig}
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.sync.util.RegularSyncItSpecUtils.FakePeer
import io.iohk.ethereum.sync.util.SyncCommonItSpec._
import io.iohk.ethereum.utils.Config
import io.prometheus.client.CollectorRegistry
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class RegularSyncItSpec extends FreeSpecBase with Matchers with BeforeAndAfterAll {
  implicit val testScheduler = Scheduler.fixedPool("test", 16)

  override def beforeAll(): Unit = {
    Metrics.configure(
      MetricsConfig(Config.config.withValue("metrics.enabled", ConfigValueFactory.fromAnyRef(true)))
    )
  }

  override def afterAll(): Unit = {
    testScheduler.shutdown()
    testScheduler.awaitTermination(120.second)
  }

  "peer 2 should sync to the top of peer1 blockchain" - {
    "given a previously imported blockchain" in customTestCaseResourceM(FakePeer.start2FakePeersRes()) {
      case (peer1, peer2) =>
        val blockNumber: Int = 2000
        for {
          _ <- peer1.importBlocksUntil(blockNumber)(IdentityUpdate)
          _ <- peer2.startRegularSync()
          _ <- peer2.connectToPeers(Set(peer1.node))
          _ <- peer2.waitForRegularSyncLoadLastBlock(blockNumber)
        } yield {
          assert(peer1.bl.getBestBlock().get.hash == peer2.bl.getBestBlock().get.hash)
        }
    }

    "given a previously mined blockchain" in customTestCaseResourceM(FakePeer.start2FakePeersRes()) {
      case (peer1, peer2) =>
        val blockHeadersPerRequest = peer2.syncConfig.blockHeadersPerRequest
        for {
          _ <- peer1.startRegularSync()
          _ <- peer1.mineNewBlocks(500.milliseconds, blockHeadersPerRequest + 1)(IdentityUpdate)
          _ <- peer1.waitForRegularSyncLoadLastBlock(blockHeadersPerRequest + 1)
          _ <- peer2.startRegularSync()
          _ <- peer2.connectToPeers(Set(peer1.node))
          _ <- peer2.waitForRegularSyncLoadLastBlock(blockHeadersPerRequest + 1)
        } yield {
          assert(peer1.bl.getBestBlock().get.hash == peer2.bl.getBestBlock().get.hash)
        }
    }
  }

  "peers should keep synced the same blockchain while their progressing forward" in customTestCaseResourceM(
    FakePeer.start2FakePeersRes()
  ) { case (peer1, peer2) =>
    val blockNumer: Int = 2000
    for {
      _ <- peer1.importBlocksUntil(blockNumer)(IdentityUpdate)
      _ <- peer1.startRegularSync()
      _ <- peer2.startRegularSync()
      _ <- peer2.connectToPeers(Set(peer1.node))
      _ <- peer2.waitForRegularSyncLoadLastBlock(blockNumer)
      _ <- peer2.mineNewBlocks(100.milliseconds, 2)(IdentityUpdate)
      _ <- peer1.waitForRegularSyncLoadLastBlock(blockNumer + 2)
      _ <- peer1.mineNewBlocks(100.milliseconds, 2)(IdentityUpdate)
      _ <- peer2.waitForRegularSyncLoadLastBlock(blockNumer + 4)
    } yield {
      assert(peer1.bl.getBestBlock().get.hash == peer2.bl.getBestBlock().get.hash)
    }
  }

  "peers should keep being synced on checkpoints" in customTestCaseResourceM(
    FakePeer.start2FakePeersRes()
  ) { case (peer1, peer2) =>
    val blockNumber: Int = 2000
    for {
      _ <- peer1.importBlocksUntil(blockNumber)(IdentityUpdate)
      _ <- peer1.startRegularSync()
      _ <- peer2.startRegularSync()
      _ <- peer2.connectToPeers(Set(peer1.node))
      _ <- peer2.waitForRegularSyncLoadLastBlock(blockNumber)
      _ <- peer2.addCheckpointedBlock(peer2.bl.getBestBlock().get)
      _ <- peer1.waitForRegularSyncLoadLastBlock(blockNumber + 1)
    } yield {
      assert(peer1.bl.getBestBlock().get.hash == peer2.bl.getBestBlock().get.hash)
      assert(peer1.bl.getLatestCheckpointBlockNumber() == peer2.bl.getLatestCheckpointBlockNumber())
    }
  }

  "peers should keep being synced on checkpoints even if 2 checkpoints are issued to different forks at the same time" in customTestCaseResourceM(
    FakePeer.start2FakePeersRes()
  ) { case (peer1, peer2) =>
    val blockNumber: Int = 2000
    for {
      _ <- peer1.importBlocksUntil(blockNumber)(IdentityUpdate)
      _ <- peer1.startRegularSync()
      _ <- peer2.startRegularSync()
      _ <- peer2.connectToPeers(Set(peer1.node))
      _ <- peer2.waitForRegularSyncLoadLastBlock(blockNumber)
      _ <- peer2.mineNewBlocks(100.milliseconds, 2)(IdentityUpdate)
      _ <- peer2.waitForRegularSyncLoadLastBlock(blockNumber + 2)
      _ <- peer2.addCheckpointedBlock(peer2.bl.getBestBlock().get)
      _ <- peer2.waitForRegularSyncLoadLastBlock(blockNumber + 3)
      _ <- peer1.addCheckpointedBlock(peer1.bl.getBestBlock().get)
      _ <- peer2.waitForRegularSyncLoadLastBlock(blockNumber + 4)
      _ <- peer1.mineNewBlocks(100.milliseconds, 1)(IdentityUpdate)
      _ <- peer2.waitForRegularSyncLoadLastBlock(blockNumber + 5)
    } yield {
      assert(peer1.bl.getBestBlock().get.hash == peer2.bl.getBestBlock().get.hash)
      assert(peer1.bl.getLatestCheckpointBlockNumber() == peer2.bl.getLatestCheckpointBlockNumber())
    }
  }

  "peers should chose the branch with a checkpoint discarding blocks that come after the checkpoint" in customTestCaseResourceM(
    FakePeer.start2FakePeersRes()
  ) { case (peer1, peer2) =>
    val length = 26
    for {
      _ <- peer1.importBlocksUntil(20)(IdentityUpdate)
      _ <- peer2.importBlocksUntil(30)(IdentityUpdate)
      _ <- peer1.startRegularSync()
      _ <- peer2.startRegularSync()
      _ <- peer2.addCheckpointedBlock(peer2.bl.getBlockByNumber(25).get)
      _ <- peer2.waitForRegularSyncLoadLastBlock(length)
      _ <- peer1.connectToPeers(Set(peer2.node))
      _ <- peer1.waitForRegularSyncLoadLastBlock(length)
    } yield {
      assert(peer1.bl.getBestBlock().get.hash == peer2.bl.getBestBlock().get.hash)
      assert(peer1.bl.getBestBlock().get.number == peer2.bl.getBestBlock().get.number && peer1.bl.getBestBlock().get.number  == length)
      assert(peer1.bl.getLatestCheckpointBlockNumber() == peer2.bl.getLatestCheckpointBlockNumber())
    }
  }

  "peers should choose the branch with a checkpoint even if it's shorter" in customTestCaseResourceM(
    FakePeer.start2FakePeersRes()
  ) { case (peer1, peer2) =>
    for {
      _ <- peer1.importBlocksUntil(4)(IdentityUpdate)
      _ <- peer2.importBlocksUntil(8)(IdentityUpdate)
      _ <- peer1.startRegularSync()
      _ <- peer2.startRegularSync()
      _ <- peer1.addCheckpointedBlock(peer1.bl.getBestBlock().get)
      _ = Thread.sleep(5000)
      _ <- peer1.waitForRegularSyncLoadLastBlock(5)
      _ <- peer1.connectToPeers(Set(peer2.node))
      //TODO: remove the following line and the test should still work. Currently reorganisation is only triggered when the checkpoint is explicitly propagated,
      // but it should also work when 2 new peers with conflicting branches connect
      _ <- peer2.getCheckpointFromPeer(peer1.bl.getBestBlock().get, PeerId("Peer1"))
      _ <- peer2.waitForRegularSyncLoadLastBlock(5)
    } yield {
      assert(peer1.bl.getBestBlock().get.hash == peer2.bl.getBestBlock().get.hash)
      assert(peer1.bl.getLatestCheckpointBlockNumber() == peer2.bl.getLatestCheckpointBlockNumber())
    }
  }

  "a peer should reorganise when receives a checkpoint older than the current best from a peer" in customTestCaseResourceM(
    FakePeer.start2FakePeersRes()
  ) { case (peer1, peer2) =>
    for {
      _ <- peer1.importBlocksUntil(20)(IdentityUpdate)
      _ <- peer2.importBlocksUntil(30)(IdentityUpdate)
      _ <- peer1.startRegularSync()
      _ <- peer2.startRegularSync()
      _ <- peer1.addCheckpointedBlock(peer1.bl.getBestBlock().get)
      _ <- peer1.waitForRegularSyncLoadLastBlock(21)
      _ <- peer2.getCheckpointFromPeer(peer1.bl.getBestBlock().get, PeerId("Peer1"))
      _ <- peer2.waitForRegularSyncLoadLastBlock(21)
    } yield {
      assert(peer1.bl.getBestBlock().get.hash == peer2.bl.getBestBlock().get.hash)
      assert(peer1.bl.getLatestCheckpointBlockNumber() == peer2.bl.getLatestCheckpointBlockNumber())
    }
  }

  "peers with divergent chains will be forced to resolve branches" in customTestCaseResourceM(
    FakePeer.start2FakePeersRes()
  ) { case (peer1, peer2) =>
    val blockNumer: Int = 2000
    for {
      _ <- peer1.importBlocksUntil(blockNumer)(IdentityUpdate)
      _ <- peer2.importBlocksUntil(blockNumer)(IdentityUpdate)
      _ <- peer1.startRegularSync()
      _ <- peer2.startRegularSync()
      _ <- peer1.mineNewBlock()(IdentityUpdate)
      _ <- peer2.mineNewBlocks(100.milliseconds, 3)(IdentityUpdate)
      _ <- peer2.waitForRegularSyncLoadLastBlock(blockNumer + 3)
      _ <- peer1.waitForRegularSyncLoadLastBlock(blockNumer + 1)
      _ <- peer2.connectToPeers(Set(peer1.node))
      _ <- peer1.waitForRegularSyncLoadLastBlock(blockNumer + 3)
      _ <- peer2.waitForRegularSyncLoadLastBlock(blockNumer + 3)
    } yield {
      assert(
        peer1.bl.getChainWeightByHash(peer1.bl.getBestBlock().get.hash) == peer2.bl.getChainWeightByHash(
          peer2.bl.getBestBlock().get.hash
        )
      )
      (peer1.bl.getBlockByNumber(blockNumer + 1), peer2.bl.getBlockByNumber(blockNumer + 1)) match {
        case (Some(blockP1), Some(blockP2)) =>
          assert(peer1.bl.getChainWeightByHash(blockP1.hash) == peer2.bl.getChainWeightByHash(blockP2.hash))
        case (_, _) => fail("invalid difficulty validation")
      }
    }
  }

  "A metric about mining a new block should be available" in customTestCaseResourceM(
    FakePeer.start2FakePeersRes()
  ) { case (peer1, peer2) =>
    import MantisRegistries._

    val minedMetricBefore = sampleMetric(TimerCountMetric, MinedBlockPropagation)
    val defaultMetricBefore = sampleMetric(TimerCountMetric, DefaultBlockPropagation)

    for {
      _ <- peer1.startRegularSync()
      _ <- peer1.mineNewBlocks(10.milliseconds, 1)(IdentityUpdate)
      _ <- peer1.waitForRegularSyncLoadLastBlock(1)
      _ <- peer2.startRegularSync()
      _ <- peer2.connectToPeers(Set(peer1.node))
      _ <- peer2.waitForRegularSyncLoadLastBlock(1)
    } yield {

      val minedMetricAfter = sampleMetric(TimerCountMetric, MinedBlockPropagation).doubleValue()
      val defaultMetricAfter = sampleMetric(TimerCountMetric, DefaultBlockPropagation).doubleValue()

      minedMetricAfter shouldBe minedMetricBefore + 1.0d
      defaultMetricAfter shouldBe defaultMetricBefore + 1.0d
    }
  }

  object MantisRegistries {
    val TimerCountMetric = "app_regularsync_blocks_propagation_timer_seconds_count"
    val DefaultBlockPropagation = "DefaultBlockPropagation"
    val MinedBlockPropagation = "MinedBlockPropagation"
    def sampleMetric(metricName: String, blockType: String): Double = CollectorRegistry.defaultRegistry.getSampleValue(
      metricName,
      Array("blocktype"),
      Array(blockType)
    )
  }
}
