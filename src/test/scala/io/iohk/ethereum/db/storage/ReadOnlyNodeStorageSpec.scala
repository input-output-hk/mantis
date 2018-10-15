package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.mpt.LeafNode
import org.scalatest.{FlatSpec, Matchers}

class ReadOnlyNodeStorageSpec extends FlatSpec with Matchers {

  "ReadOnlyNodeStorage" should "not update dataSource" in new TestSetup {
    val newLeaf = LeafNode(ByteString(1), ByteString(1))
    val readOnlyNodeStorage = stateStorage.getReadOnlyStorage
    readOnlyNodeStorage.updateNodesInStorage(Some(newLeaf), Nil)
    dataSource.storage.size shouldEqual 0
  }

  it should "be able to persist to underlying storage when needed" in new TestSetup {
    val newLeaf = LeafNode(ByteString(1), ByteString(1))
    val (nodeKey, nodeVal) = MptStorage.collapseNode(Some(newLeaf))._2.head
    val readOnlyNodeStorage = stateStorage.getReadOnlyStorage

    readOnlyNodeStorage.updateNodesInStorage(Some(newLeaf), Nil)

    val previousSize = dataSource.storage.size
    readOnlyNodeStorage.get(nodeKey.toArray[Byte]) shouldEqual newLeaf
  }

  trait TestSetup {
    val dataSource = EphemDataSource()
    val stateStorage = StateStorage.createTestStateStorage(dataSource)._1
  }
}


