package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.cache.{Cache, LruCache, MapCache}
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, BasicPruning, InMemoryPruning}
import io.iohk.ethereum.mpt.NodesKeyValueStorage
import io.iohk.ethereum.utils.Config.NodeCacheConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

class StateStorageSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks with ObjectGenerators {

  def saveNodeToDbTest(storage: StateStorage, nodeStorage: NodesKeyValueStorage): Unit =
    forAll(keyValueByteStringGen(32)) { keyvals =>
      keyvals.foreach { case (key, value) =>
        storage.saveNode(key, value, 10)
      }

      keyvals.foreach { case (key, value) =>
        val result = nodeStorage.get(key)
        assert(result.isDefined)
        assert(result.get.sameElements(value))
      }
    }

  def getNodeFromDbTest(stateStorage: StateStorage): Unit =
    forAll(nodeGen) { node =>
      val storage = stateStorage.getBackingStorage(0)
      storage.updateNodesInStorage(Some(node), Nil)
      val fromStorage = stateStorage.getNode(ByteString(node.hash))
      assert(fromStorage.isDefined)
      assert(fromStorage.get == node)
    }

  def provideStorageForTrieTest(stateStorage: StateStorage): Unit =
    forAll(nodeGen) { node =>
      val storage = stateStorage.getBackingStorage(0)
      storage.updateNodesInStorage(Some(node), Nil)
      val fromStorage = storage.get(node.hash)
      assert(fromStorage.hash.sameElements(node.hash))
    }

  "ArchiveStateStorage" should "save node directly to db" in new TestSetup {
    saveNodeToDbTest(archiveStateStorage, archiveNodeStorage)
  }

  it should "provide storage for trie" in new TestSetup {
    provideStorageForTrieTest(archiveStateStorage)
  }

  it should "enable way to get node directly" in new TestSetup {
    getNodeFromDbTest(archiveStateStorage)
  }

  it should "provide function to act on block save" in new TestSetup {
    var ints = List.empty[Int]

    forAll(listOfNodes(minNodes, maxNodes)) { nodes =>
      val storage = archiveStateStorage.getBackingStorage(0)
      nodes.foreach(node => storage.updateNodesInStorage(Some(node), Nil))

      if (testCache.shouldPersist) {
        val sizeBefore = ints.size
        archiveStateStorage.onBlockSave(1, 0) { () =>
          ints = 1 :: ints
        }

        assert(ints.size == sizeBefore + 1)
      }
    }
  }

  it should "provide function to act on block rollback" in new TestSetup {
    var ints = List.empty[Int]

    forAll(listOfNodes(minNodes, maxNodes)) { nodes =>
      val storage = archiveStateStorage.getBackingStorage(0)
      nodes.foreach(node => storage.updateNodesInStorage(Some(node), Nil))

      if (testCache.shouldPersist) {
        val sizeBefore = ints.size
        archiveStateStorage.onBlockRollback(1, 0) { () =>
          ints = 1 :: ints
        }

        assert(ints.size == sizeBefore + 1)
      }
    }
  }

  "ReferenceCountedStorage" should "save node directly to db" in new TestSetup {
    saveNodeToDbTest(referenceCounteStateStorage, refCountNodeStorage)
  }

  it should "provide storage for trie" in new TestSetup {
    provideStorageForTrieTest(referenceCounteStateStorage)
  }

  it should "enable way to get node directly" in new TestSetup {
    getNodeFromDbTest(referenceCounteStateStorage)
  }

  it should "provide function to act on block save" in new TestSetup {
    var ints = List.empty[Int]

    forAll(listOfNodes(minNodes, maxNodes)) { nodes =>
      val storage = referenceCounteStateStorage.getBackingStorage(0)
      nodes.foreach(node => storage.updateNodesInStorage(Some(node), Nil))

      if (testCache.shouldPersist) {
        val sizeBefore = ints.size
        referenceCounteStateStorage.onBlockSave(1, 0) { () =>
          ints = 1 :: ints
        }

        assert(ints.size == sizeBefore + 1)
      }
    }
  }

  it should "provide function to act on block rollback" in new TestSetup {
    var ints = List.empty[Int]

    forAll(listOfNodes(minNodes, maxNodes)) { nodes =>
      val storage = referenceCounteStateStorage.getBackingStorage(0)
      nodes.foreach(node => storage.updateNodesInStorage(Some(node), Nil))

      if (testCache.shouldPersist) {
        val sizeBefore = ints.size
        referenceCounteStateStorage.onBlockRollback(1, 0) { () =>
          ints = 1 :: ints
        }

        assert(ints.size == sizeBefore + 1)
      }
    }
  }

  "CachedReferenceCountedStorage" should "save node directly to db" in new TestSetup {
    saveNodeToDbTest(cachedStateStorage, cachedPrunedNodeStorage)
  }

  it should "provide storage for trie" in new TestSetup {
    provideStorageForTrieTest(cachedStateStorage)
  }

  it should "enable way to get node directly" in new TestSetup {
    getNodeFromDbTest(cachedStateStorage)
  }

  trait TestSetup {
    val minNodes = 5
    val maxNodes = 15
    val dataSource: EphemDataSource = EphemDataSource()
    val testCache: Cache[NodeHash,NodeEncoded] = MapCache.createTestCache[NodeHash, NodeEncoded](10)
    val nodeStorage = new NodeStorage(dataSource)
    val cachedNodeStorage = new CachedNodeStorage(nodeStorage, testCache)
    object TestCacheConfig extends NodeCacheConfig {
      override val maxSize: Long = 100
      override val maxHoldTime: FiniteDuration = FiniteDuration(10, TimeUnit.MINUTES)
    }

    val changeLog = new ChangeLog(nodeStorage)
    val lruCache = new LruCache[NodeHash, HeapEntry](TestCacheConfig)

    val archiveNodeStorage = new ArchiveNodeStorage(nodeStorage)
    val archiveStateStorage: StateStorage = StateStorage(ArchivePruning, nodeStorage, cachedNodeStorage, lruCache)

    val refCountNodeStorage = new ReferenceCountNodeStorage(nodeStorage, 10)
    val referenceCounteStateStorage: StateStorage = StateStorage(BasicPruning(10), nodeStorage, cachedNodeStorage, lruCache)

    val cachedStateStorage: StateStorage = StateStorage(InMemoryPruning(10), nodeStorage, cachedNodeStorage, lruCache)
    val cachedPrunedNodeStorage = new CachedReferenceCountedStorage(nodeStorage, lruCache, changeLog, 10)
  }
}
