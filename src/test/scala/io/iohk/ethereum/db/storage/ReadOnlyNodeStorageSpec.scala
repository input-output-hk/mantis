package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.cache.{Cache, LruCache, MapCache}
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.StateStorage.GenesisDataLoad
import io.iohk.ethereum.db.storage.pruning.InMemoryPruning
import io.iohk.ethereum.mpt.LeafNode
import io.iohk.ethereum.utils.Config.NodeCacheConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

class ReadOnlyNodeStorageSpec extends AnyFlatSpec with Matchers {

  "ReadOnlyNodeStorage" should "not update dataSource" in new TestSetup {
    val readOnlyNodeStorage = stateStorage.getReadOnlyStorage
    readOnlyNodeStorage.updateNodesInStorage(Some(newLeaf), Nil)
    dataSource.storage.size shouldEqual 0
  }

  it should "be able to persist to underlying storage when needed" in new TestSetup {
    val (nodeKey, nodeVal) = MptStorage.collapseNode(Some(newLeaf))._2.head
    val readOnlyNodeStorage = stateStorage.getReadOnlyStorage

    readOnlyNodeStorage.updateNodesInStorage(Some(newLeaf), Nil)

    val previousSize = dataSource.storage.size
    readOnlyNodeStorage.get(nodeKey.toArray[Byte]) shouldEqual newLeaf

    previousSize shouldEqual 0

    readOnlyNodeStorage.persist()

    stateStorage.forcePersist(GenesisDataLoad)
    dataSource.storage.size shouldEqual 1
  }

  it should "be able to persist to underlying storage when Genesis loading" in new TestSetup {
    val (nodeKey, nodeVal) = MptStorage.collapseNode(Some(newLeaf))._2.head
    val readOnlyNodeStorage = cachedStateStorage.getReadOnlyStorage

    readOnlyNodeStorage.updateNodesInStorage(Some(newLeaf), Nil)

    val previousSize = dataSource.storage.size
    readOnlyNodeStorage.get(nodeKey.toArray[Byte]) shouldEqual newLeaf

    previousSize shouldEqual 0

    readOnlyNodeStorage.persist()

    cachedStateStorage.forcePersist(GenesisDataLoad) shouldEqual true
    dataSource.storage.size shouldEqual 1
  }

  trait TestSetup {
    val newLeaf: LeafNode = LeafNode(ByteString(1), ByteString(1))
    val dataSource: EphemDataSource = EphemDataSource()
    val (stateStorage, nodeStorage, cachedStorage) = StateStorage.createTestStateStorage(dataSource)

    object TestCacheConfig extends NodeCacheConfig {
      override val maxSize: Long = 100
      override val maxHoldTime: FiniteDuration = FiniteDuration(10, TimeUnit.MINUTES)
    }
    val lruCache = new LruCache[NodeHash, HeapEntry](TestCacheConfig)
    val newNodeStorage = new NodeStorage(dataSource)
    val testCache: Cache[NodeHash, NodeEncoded] = MapCache.createTestCache[NodeHash, NodeEncoded](10)
    val newCachedNodeStorage = new CachedNodeStorage(newNodeStorage, testCache)

    val cachedStateStorage: StateStorage =
      StateStorage(InMemoryPruning(10), newNodeStorage, newCachedNodeStorage, lruCache)
  }
}
