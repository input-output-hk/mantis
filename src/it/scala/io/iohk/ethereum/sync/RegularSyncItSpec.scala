package io.iohk.ethereum.sync

import io.iohk.ethereum.FlatSpecBase
import io.iohk.ethereum.sync.util.RegularSyncItSpecUtils.FakePeer
import io.iohk.ethereum.sync.util.SyncCommonItSpec._
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class RegularSyncItSpec extends FlatSpecBase with Matchers with BeforeAndAfter {
  implicit val testScheduler = Scheduler.fixedPool("test", 16)

  it should "should sync blockchain with same best block" in customTestCaseResourceM(FakePeer.start2FakePeersRes()) {
    case (peer1, peer2) =>
      val blockNumer: Int = 2000
      for {
        _ <- peer2.importBlocksUntil(blockNumer)(IdentityUpdate)
        _ <- peer1.connectToPeers(Set(peer2.node))
        _ <- peer1.startRegularSync().delayExecution(50.milliseconds)
        _ <- peer2.broadcastBlock()(IdentityUpdate).delayExecution(500.milliseconds)
        _ <- peer1.waitForRegularSyncLoadLastBlock(blockNumer)
      } yield {
        assert(peer1.bl.getBestBlock().hash == peer2.bl.getBestBlock().hash)
      }
  }

  it should "should sync blockchain progressing forward in the same time" in customTestCaseResourceM(FakePeer.start2FakePeersRes()) {
    case (peer1, peer2) =>
      val blockNumer: Int = 2000
      for {
        _ <- peer2.startRegularSync().delayExecution(50.milliseconds)
        _ <- peer2.importBlocksUntil(blockNumer)(IdentityUpdate)
        _ <- peer1.connectToPeers(Set(peer2.node))
        _ <- peer1.startRegularSync().delayExecution(500.milliseconds)
        _ <- peer2.mineNewBlocks(100.milliseconds, 10)(IdentityUpdate)
        _ <- peer1.waitForRegularSyncLoadLastBlock(blockNumer + 10)
      } yield {
        assert(peer1.bl.getBestBlock().hash == peer2.bl.getBestBlock().hash)
      }
  }

  it should "should sync peers with divergent chains will be forced to resolve branches"in customTestCaseResourceM(FakePeer.start2FakePeersRes()) {
    case (peer1, peer2) =>
      val blockNumer: Int = 2000
      for {
        _ <- peer2.importBlocksUntil(blockNumer)(IdentityUpdate)
        _ <- peer2.startRegularSync().delayExecution(50.milliseconds)
        _ <- peer1.importBlocksUntil(blockNumer)(IdentityUpdate)
        _ <- peer1.startRegularSync().delayExecution(50.milliseconds)
        _ <- peer2.mineNewBlock(10)(IdentityUpdate).delayExecution(500.milliseconds)
        _ <- peer2.mineNewBlock(10)(IdentityUpdate).delayExecution(500.milliseconds)
        _ <- peer2.mineNewBlock(10)(IdentityUpdate).delayExecution(500.milliseconds)
        _ <- peer2.waitForRegularSyncLoadLastBlock(blockNumer + 3)
        _ <- peer1.mineNewBlock()(IdentityUpdate).delayExecution(500.milliseconds)
        _ <- peer1.waitForRegularSyncLoadLastBlock(blockNumer + 1)
        _ <- peer1.connectToPeers(Set(peer2.node)).delayExecution(500.milliseconds)
        _ <- peer1.waitForRegularSyncLoadLastBlock(blockNumer + 3)
        _ <- peer2.waitForRegularSyncLoadLastBlock(blockNumer + 3)
      } yield {
        assert(peer1.bl.getTotalDifficultyByHash(peer1.bl.getBestBlock().hash) == peer2.bl.getTotalDifficultyByHash(peer2.bl.getBestBlock().hash))
        (peer1.bl.getBlockByNumber(blockNumer + 1), peer2.bl.getBlockByNumber(blockNumer + 1)) match {
          case (Some(blockP1), Some(blockP2)) =>
            assert(peer1.bl.getTotalDifficultyByHash(blockP1.hash) == peer2.bl.getTotalDifficultyByHash(blockP2.hash))
          case (_ , _) => fail("invalid difficulty validation")
        }
      }
  }

}
