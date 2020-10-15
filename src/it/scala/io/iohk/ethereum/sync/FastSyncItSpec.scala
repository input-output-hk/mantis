package io.iohk.ethereum.sync

import io.iohk.ethereum.FlatSpecBase
import io.iohk.ethereum.sync.util.FakePeerFastSync
import io.iohk.ethereum.sync.util.SyncUtils.{identityUpdate, updateStateAtBlock}
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class FastSyncItSpec extends FlatSpecBase with Matchers with BeforeAndAfter {
  implicit val testScheduler = Scheduler.fixedPool("test", 16)

  "FastSync" should "should sync blockchain without state nodes" in customTestCaseResourceM(
    FakePeerFastSync.start3FakePeersRes()
  ) { case (peer1, peer2, peer3) =>
    val blockNumer: Int = 1000
    for {
      _ <- peer2.importBlocksUntil(blockNumer)(identityUpdate)
      _ <- peer3.importBlocksUntil(blockNumer)(identityUpdate)
      _ <- peer1.connectToPeers(Set(peer2.node, peer3.node))
      _ <- peer1.start.delayExecution(50.milliseconds)
      _ <- peer1.waitForFastSyncFinish()
    } yield {
      assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.targetBlockOffset)
      assert(peer1.bl.getBestBlockNumber() == peer3.bl.getBestBlockNumber() - peer3.testSyncConfig.targetBlockOffset)
    }
  }

  it should "should sync blockchain with state nodes" in customTestCaseResourceM(FakePeerFastSync.start3FakePeersRes()) {
    case (peer1, peer2, peer3) =>
      val blockNumer: Int = 1000
      val blockWithUpdate: Int = 500
      for {
        _ <- peer2.importBlocksUntil(blockNumer)(updateStateAtBlock(blockWithUpdate))
        _ <- peer3.importBlocksUntil(blockNumer)(updateStateAtBlock(blockWithUpdate))
        _ <- peer1.connectToPeers(Set(peer2.node, peer3.node))
        _ <- peer1.start.delayExecution(50.milliseconds)
        _ <- peer1.waitForFastSyncFinish()
      } yield {
        val trie = peer1.getBestBlockTrie()
        // due to the fact that function generating state is deterministic both peer2 and peer3 ends up with exactly same
        // state, so peer1 can get whole trie from both of them.
        assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.targetBlockOffset)
        assert(peer1.bl.getBestBlockNumber() == peer3.bl.getBestBlockNumber() - peer3.testSyncConfig.targetBlockOffset)
        assert(trie.isDefined)
      }
  }

  it should "should update target block" in customTestCaseResourceM(FakePeerFastSync.start2FakePeersRes()) {
    case (peer1, peer2) =>
      for {
        _ <- peer2.importBlocksUntil(1000)(identityUpdate)
        _ <- peer1.connectToPeers(Set(peer2.node))
        _ <- peer2.importBlocksUntil(2000)(identityUpdate).startAndForget
        _ <- peer1.start.delayExecution(50.milliseconds)
        _ <- peer1.waitForFastSyncFinish()
      } yield {
        assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.targetBlockOffset)
      }
  }
}
