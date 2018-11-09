package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.cache.MapCache
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, BasicPruning}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class StateStorageSpec extends FlatSpec with Matchers with PropertyChecks with ObjectGenerators {

  "ArchiveStateStorage" should "save node directly to db" in new TestSetup {
    forAll(keyValueByteStringGen(32)) { keyvals =>
      keyvals.foreach { case (key, value) =>
        archiveStateStorage.saveNode(key, value , 10)
      }

      keyvals.foreach { case (key, value) =>
        val result = nodeStorage.get(key)
        assert(result.isDefined)
        assert(result.get sameElements value)
      }
    }
  }

  it should "provide storage for trie" in new TestSetup {
    forAll(nodeGen) { node =>
      val storage = archiveStateStorage.getBackingStorage(0)
      storage.updateNodesInStorage(Some(node), Nil)
      val fromStorage = storage.get(node.hash)
      assert(fromStorage.hash sameElements node.hash)
    }
  }

  it should "enable way to get node directly" in new TestSetup {
    forAll(nodeGen) { node =>
      val storage = archiveStateStorage.getBackingStorage(0)
      storage.updateNodesInStorage(Some(node), Nil)
      val fromStorage = archiveStateStorage.getNode(ByteString(node.hash))
      assert(fromStorage.isDefined)
      assert(fromStorage.get == node)
    }
  }

  it should "provide function to act on block save" in new TestSetup {
    var ints = List.empty[Int]

    forAll(listOfNodes(minNodes, maxNodes)) {nodes =>

      val storage = archiveStateStorage.getBackingStorage(0)
      nodes.foreach(node => storage.updateNodesInStorage(Some(node), Nil))

      if (testCache.shouldPersist) {
        val sizeBefore =  ints.size
        archiveStateStorage.onBlockSave(1, 0) { () =>
          ints = 1 :: ints
        }

        assert(ints.size == sizeBefore + 1)
      }
    }
  }

  it should "provide function to act on block rollback" in new TestSetup {
    var ints = List.empty[Int]

    forAll(listOfNodes(minNodes, maxNodes)) {nodes =>

      val storage = archiveStateStorage.getBackingStorage(0)
      nodes.foreach(node => storage.updateNodesInStorage(Some(node), Nil))

      if (testCache.shouldPersist) {
        val sizeBefore =  ints.size
        archiveStateStorage.onBlockRollback(1, 0) { () =>
          ints = 1 :: ints
        }

        assert(ints.size == sizeBefore + 1)
      }
    }
  }

  "ReferenceCountedStorage" should "save node directly to db" in new TestSetup {
    forAll(keyValueByteStringGen(32)) { keyvals =>
      keyvals.foreach { case (key, value) =>
        referenceCounteStateStorage.saveNode(key, value , 10)
      }

      keyvals.foreach { case (key, value) =>
        val result = refCountNodeStorage.get(key)
        assert(result.isDefined)
        assert(result.get sameElements value)
      }
    }
  }

  it should "provide storage for trie" in new TestSetup {
    forAll(nodeGen) { node =>
      val storage = referenceCounteStateStorage.getBackingStorage(0)
      storage.updateNodesInStorage(Some(node), Nil)
      val fromStorage = storage.get(node.hash)
      assert(fromStorage.hash sameElements node.hash)
    }
  }

  it should "enable way to get node directly" in new TestSetup {
    forAll(nodeGen) { node =>
      val storage = referenceCounteStateStorage.getBackingStorage(0)
      storage.updateNodesInStorage(Some(node), Nil)
      val fromStorage = referenceCounteStateStorage.getNode(ByteString(node.hash))
      assert(fromStorage.isDefined)
      assert(fromStorage.get == node)
    }
  }

  it should "provide function to act on block save" in new TestSetup {
    var ints = List.empty[Int]

    forAll(listOfNodes(minNodes, maxNodes)) {nodes =>

      val storage = referenceCounteStateStorage.getBackingStorage(0)
      nodes.foreach(node => storage.updateNodesInStorage(Some(node), Nil))

      if (testCache.shouldPersist) {
        val sizeBefore =  ints.size
        referenceCounteStateStorage.onBlockSave(1, 0) { () =>
          ints = 1 :: ints
        }

        assert(ints.size == sizeBefore + 1)
      }
    }
  }

  it should "provide function to act on block rollback" in new TestSetup {
    var ints = List.empty[Int]

    forAll(listOfNodes(minNodes, maxNodes)) {nodes =>

      val storage = referenceCounteStateStorage.getBackingStorage(0)
      nodes.foreach(node => storage.updateNodesInStorage(Some(node), Nil))

      if (testCache.shouldPersist) {
        val sizeBefore =  ints.size
        referenceCounteStateStorage.onBlockRollback(1, 0) { () =>
          ints = 1 :: ints
        }

        assert(ints.size == sizeBefore + 1)
      }
    }
  }

  trait TestSetup {
    val minNodes = 5
    val maxNodes = 15
    val dataSource = EphemDataSource()
    val testCache = MapCache.createTestCache[NodeHash, NodeEncoded](10)
    val nodeStorage = new NodeStorage(dataSource)
    val cachedNodeStorage = new CachedNodeStorage(nodeStorage, testCache)
    val refCountNodeStorage = new ReferenceCountNodeStorage(nodeStorage, 10)
    val archiveStateStorage = StateStorage(ArchivePruning, nodeStorage, cachedNodeStorage)
    val referenceCounteStateStorage = StateStorage(BasicPruning(10), nodeStorage, cachedNodeStorage)
  }
}

