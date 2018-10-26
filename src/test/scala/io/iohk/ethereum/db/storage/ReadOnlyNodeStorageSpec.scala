package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.mpt.LeafNode
import org.scalatest.{FlatSpec, Matchers}

class ReadOnlyNodeStorageSpec extends FlatSpec with Matchers {

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
    stateStorage.forcePersist

    dataSource.storage.size shouldEqual 1
  }

  trait TestSetup {
    val newLeaf = LeafNode(ByteString(1), ByteString(1))
    val dataSource = EphemDataSource()
    val (stateStorage, nodeStorage, cachedStorage) = StateStorage.createTestStateStorage(dataSource)
  }
}


