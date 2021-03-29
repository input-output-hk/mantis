package io.iohk.ethereum.sync

import akka.util.ByteString
import io.iohk.ethereum.FlatSpecBase
import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistReason.BlacklistReasonType
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.sync.FastSyncItSpec._
import io.iohk.ethereum.sync.util.FastSyncItSpecUtils.FakePeer
import io.iohk.ethereum.sync.util.SyncCommonItSpec._
import io.iohk.ethereum.sync.util.SyncCommonItSpecUtils._
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class FastSyncItSpec extends FlatSpecBase with Matchers with BeforeAndAfterAll {
  implicit val testScheduler = Scheduler.fixedPool("test", 16)

  override def afterAll(): Unit = {
    testScheduler.shutdown()
    testScheduler.awaitTermination(60.second)
  }

  "FastSync" should "sync blockchain without state nodes" in customTestCaseResourceM(
    FakePeer.start3FakePeersRes()
  ) { case (peer1, peer2, peer3) =>
    for {
      _ <- peer2.importBlocksUntil(1000)(IdentityUpdate)
      _ <- peer3.importBlocksUntil(1000)(IdentityUpdate)
      _ <- peer1.connectToPeers(Set(peer2.node, peer3.node))
      _ <- peer1.startFastSync().delayExecution(50.milliseconds)
      _ <- peer1.waitForFastSyncFinish()
    } yield {
      assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.pivotBlockOffset)
      assert(peer1.bl.getBestBlockNumber() == peer3.bl.getBestBlockNumber() - peer3.testSyncConfig.pivotBlockOffset)
    }
  }

  it should "sync blockchain with state nodes" in customTestCaseResourceM(FakePeer.start3FakePeersRes()) {
    case (peer1, peer2, peer3) =>
      for {
        _ <- peer2.importBlocksUntil(1000)(updateStateAtBlock(500))
        _ <- peer3.importBlocksUntil(1000)(updateStateAtBlock(500))
        _ <- peer1.connectToPeers(Set(peer2.node, peer3.node))
        _ <- peer1.startFastSync().delayExecution(50.milliseconds)
        _ <- peer1.waitForFastSyncFinish()
      } yield {
        val trie = peer1.getBestBlockTrie()
        val synchronizingPeerHaveAllData = peer1.containsExpectedDataUpToAccountAtBlock(1000, 500)
        // due to the fact that function generating state is deterministic both peer2 and peer3 ends up with exactly same
        // state, so peer1 can get whole trie from both of them.
        assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.pivotBlockOffset)
        assert(peer1.bl.getBestBlockNumber() == peer3.bl.getBestBlockNumber() - peer3.testSyncConfig.pivotBlockOffset)
        assert(trie.isDefined)
        assert(synchronizingPeerHaveAllData)
      }
  }

  it should "sync blockchain with state nodes when peer do not response with full responses" in
    customTestCaseResourceM(
      FakePeer.start4FakePeersRes(
        fakePeerCustomConfig2 = FakePeerCustomConfig(HostConfig()),
        fakePeerCustomConfig3 = FakePeerCustomConfig(HostConfig())
      )
    ) { case (peer1, peer2, peer3, peer4) =>
      for {
        _ <- peer2.importBlocksUntil(1000)(updateStateAtBlock(500))
        _ <- peer3.importBlocksUntil(1000)(updateStateAtBlock(500))
        _ <- peer4.importBlocksUntil(1000)(updateStateAtBlock(500))

        _ <- peer1.connectToPeers(Set(peer2.node, peer3.node, peer4.node))
        _ <- peer1.startFastSync().delayExecution(50.milliseconds)
        _ <- peer1.waitForFastSyncFinish()
      } yield {
        val trie = peer1.getBestBlockTrie()
        val synchronizingPeerHaveAllData = peer1.containsExpectedDataUpToAccountAtBlock(1000, 500)
        // due to the fact that function generating state is deterministic both peer3 and peer4 ends up with exactly same
        // state, so peer1 can get whole trie from both of them.
        assert(peer1.bl.getBestBlockNumber() == peer3.bl.getBestBlockNumber() - peer3.testSyncConfig.pivotBlockOffset)
        assert(peer1.bl.getBestBlockNumber() == peer4.bl.getBestBlockNumber() - peer4.testSyncConfig.pivotBlockOffset)
        assert(trie.isDefined)
        assert(synchronizingPeerHaveAllData)
      }
    }

  it should "sync blockchain with state nodes when one of the peers send empty state responses" in
    customTestCaseResourceM(
      FakePeer.start4FakePeersRes(
        fakePeerCustomConfig2 = FakePeerCustomConfig(HostConfig()),
        fakePeerCustomConfig3 = FakePeerCustomConfig(HostConfig().copy(maxMptComponentsPerMessage = 0))
      )
    ) { case (peer1, peer2, peer3, peer4) =>
      for {
        _ <- peer2.importBlocksUntil(1000)(updateStateAtBlock(500))
        _ <- peer3.importBlocksUntil(1000)(updateStateAtBlock(500))
        _ <- peer4.importBlocksUntil(1000)(updateStateAtBlock(500))

        _ <- peer1.connectToPeers(Set(peer2.node, peer3.node, peer4.node))
        _ <- peer1.startFastSync().delayExecution(50.milliseconds)
        _ <- peer1.waitForFastSyncFinish()
      } yield {
        val trie = peer1.getBestBlockTrie()
        val synchronizingPeerHaveAllData = peer1.containsExpectedDataUpToAccountAtBlock(1000, 500)
        // due to the fact that function generating state is deterministic both peer3 and peer4 ends up with exactly same
        // state, so peer1 can get whole trie from both of them.
        assert(peer1.bl.getBestBlockNumber() == peer3.bl.getBestBlockNumber() - peer3.testSyncConfig.pivotBlockOffset)
        assert(peer1.bl.getBestBlockNumber() == peer4.bl.getBestBlockNumber() - peer4.testSyncConfig.pivotBlockOffset)
        assert(trie.isDefined)
        assert(synchronizingPeerHaveAllData)
      }
    }

  it should "update pivot block" in customTestCaseResourceM(FakePeer.start2FakePeersRes()) { case (peer1, peer2) =>
    for {
      _ <- peer2.importBlocksUntil(1000)(IdentityUpdate)
      _ <- peer1.connectToPeers(Set(peer2.node))
      _ <- peer2.importBlocksUntil(2000)(IdentityUpdate).startAndForget
      _ <- peer1.startFastSync().delayExecution(50.milliseconds)
      _ <- peer1.waitForFastSyncFinish()
    } yield {
      assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.pivotBlockOffset)
    }
  }

  it should "update pivot block and sync this new pivot block state" in customTestCaseResourceM(
    FakePeer.start2FakePeersRes()
  ) { case (peer1, peer2) =>
    for {
      _ <- peer2.importBlocksUntil(1000)(IdentityUpdate)
      _ <- peer1.connectToPeers(Set(peer2.node))
      _ <- peer2.importBlocksUntil(2000)(updateStateAtBlock(1500)).startAndForget
      _ <- peer1.startFastSync().delayExecution(50.milliseconds)
      _ <- peer1.waitForFastSyncFinish()
    } yield {
      assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.pivotBlockOffset)
    }
  }

  it should "sync state to peer from partially synced state" in customTestCaseResourceM(
    FakePeer.start2FakePeersRes()
  ) { case (peer1, peer2) =>
    for {
      _ <- peer2.importBlocksUntil(2000)(updateStateAtBlock(1500))
      _ <- peer2.importBlocksUntil(3000)(updateStateAtBlock(2500, 1000, 2000))
      _ <- peer1.importBlocksUntil(2000)(updateStateAtBlock(1500))
      _ <- peer1.startWithState()
      _ <- peer1.connectToPeers(Set(peer2.node))
      _ <- peer1.startFastSync().delayExecution(50.milliseconds)
      _ <- peer1.waitForFastSyncFinish()
    } yield {
      assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.pivotBlockOffset)
    }
  }

  it should "follow the longest chains" in customTestCaseResourceM(
    FakePeer.start4FakePeersRes()
  ) { case (peer1, peer2, peer3, peer4) =>
    for {
      _ <- peer2.importBlocksUntil(1000)(IdentityUpdate)
      _ <- peer3.importBlocksUntil(1000)(IdentityUpdate)
      _ <- peer4.importBlocksUntil(1000)(IdentityUpdate)

      _ <- peer2.importBlocksUntil(2000)(IdentityUpdate)
      _ <- peer3.importBlocksUntil(3000)(updateStateAtBlock(1001, endAccount = 3000))
      _ <- peer4.importBlocksUntil(3000)(updateStateAtBlock(1001, endAccount = 3000))

      _ <- peer1.connectToPeers(Set(peer2.node, peer3.node, peer4.node))
      _ <- peer1.startFastSync().delayExecution(50.milliseconds)
      _ <- peer1.waitForFastSyncFinish()
    } yield {
      val trie = peer1.getBestBlockTrie()
      val synchronizingPeerHaveAllData = peer1.containsExpectedDataUpToAccountAtBlock(3000, 1001)
      // due to the fact that function generating state is deterministic both peer3 and peer4 ends up with exactly same
      // state, so peer1 can get whole trie from both of them.
      assert(peer1.bl.getBestBlockNumber() == peer3.bl.getBestBlockNumber() - peer3.testSyncConfig.pivotBlockOffset)
      assert(peer1.bl.getBestBlockNumber() == peer4.bl.getBestBlockNumber() - peer4.testSyncConfig.pivotBlockOffset)
      assert(trie.isDefined)
      assert(synchronizingPeerHaveAllData)
    }
  }

  it should "switch to regular sync once `safeDownloadTarget` is reached" in customTestCaseResourceM(
    FakePeer.start3FakePeersRes()
  ) { case (peer1, peer2, peer3) =>
    for {
      _ <- peer2.importBlocksUntil(1200)(IdentityUpdate)
      _ <- peer3.importBlocksUntil(1200)(IdentityUpdate)
      _ <- peer1.connectToPeers(Set(peer2.node, peer3.node))
      _ <- peer1.startFastSync().delayExecution(50.milliseconds)
      _ <- peer1.waitForFastSyncFinish()
    } yield {
      assert(peer1.bl.getBestBlockNumber() == peer3.bl.getBestBlockNumber() - peer3.testSyncConfig.pivotBlockOffset)
    }
  }

  it should "blacklist peer on Invalid batch last header number" in customTestCaseResourceM(
    FakePeer.start3FakePeersRes()
  ) { case (peer1, peer2, peer3) =>
    for {
      _ <- peer2.importBlocksUntil(1000)(IdentityUpdate)
      _ <- peer3.importInvalidBlockNumbers(201, 1200)(IdentityUpdate)

      _ <- peer1.connectToPeers(Set(peer2.node, peer3.node))
      _ <- peer1.startFastSync().delayExecution(50.milliseconds)
      _ <- peer1.waitForFastSyncFinish()
    } yield {
      // Peer3 is blacklisted
      val blacklistedPeer = PeerId(s"${peer3.node.addr.getHostAddress}:${peer3.node.tcpPort}")
      val blacklistReason = peer1.blacklist.cache.getIfPresent(blacklistedPeer)

      assert(peer1.blacklist.isBlacklisted(blacklistedPeer))
      assert(blacklistReason.get == BlacklistReasonType.BlockHeaderValidationFailedType)
    }
  }

  it should "sync blockchain when peer responds with invalid batch last header hash" in customTestCaseResourceM(
    FakePeer.start4FakePeersRes()
  ) { case (peer1, peer2, peer3, peer4) =>
    for {
      _ <- peer1.importBlocksUntil(400)(IdentityUpdate)
      _ <- peer2.importBlocksUntil(1000)(IdentityUpdate)

      _ <- peer3.importInvalidBlocks(600, 800)(IdentityUpdate)
      _ <- peer3.importBlocksUntil(1200)(updateStateAtBlock(1000))

      _ <- peer4.importBlocksUntil(1100)(IdentityUpdate)

      _ <- peer1.connectToPeers(Set(peer2.node, peer3.node, peer4.node))
      _ <- peer1.startFastSync().delayExecution(50.milliseconds)
      _ <- peer2.importBlocksUntil(1200)(IdentityUpdate).startAndForget
      _ <- peer1.waitForFastSyncFinish()
    } yield {
      // Peer3 is blacklisted
      val blacklistedPeer = PeerId(s"${peer3.node.addr.getHostAddress}:${peer3.node.tcpPort}")
      val blacklistReason = peer1.blacklist.cache.getIfPresent(blacklistedPeer)

      assert(peer1.blacklist.isBlacklisted(blacklistedPeer))
      assert(blacklistReason.get == BlacklistReasonType.BlockHeaderValidationFailedType)
    }
  }
}

object FastSyncItSpec {

  def updateWorldWithAccounts(
      startAccount: Int,
      endAccount: Int,
      world: InMemoryWorldStateProxy
  ): InMemoryWorldStateProxy = {
    val resultWorld = (startAccount until endAccount).foldLeft(world) { (world, num) =>
      val randomBalance = num
      val randomAddress = Address(num)
      val codeBytes = BigInt(num).toByteArray
      val storage = world.getStorage(randomAddress)
      val changedStorage = (num until num + 20).foldLeft(storage)((storage, value) => storage.store(value, value))
      world
        .saveAccount(randomAddress, Account.empty().copy(balance = randomBalance))
        .saveCode(randomAddress, ByteString(codeBytes))
        .saveStorage(randomAddress, changedStorage)
    }
    InMemoryWorldStateProxy.persistState(resultWorld)
  }

  def updateStateAtBlock(
      blockWithUpdate: BigInt,
      startAccount: Int = 0,
      endAccount: Int = 1000
  ): (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy = {
    (blockNr: BigInt, world: InMemoryWorldStateProxy) =>
      if (blockNr == blockWithUpdate) {
        updateWorldWithAccounts(startAccount, endAccount, world)
      } else {
        IdentityUpdate(blockNr, world)
      }
  }
}
