package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.mpt.LeafNode
import org.scalatest.{FlatSpec, Matchers}

class ReadOnlyNodeStorageSpec extends FlatSpec with Matchers {

  "ReadOnlyNodeStorage" should "not update dataSource" in new TestSetup {
    referenceCountNodeStorage.updateNodesInStorage(Some(LeafNode(ByteString(1), ByteString(1))), Nil)
    dataSource.storage.size shouldEqual 0
  }

  it should "be able to read from underlying storage but not change it" in new TestSetup {
    val key1 = ByteString("key1")
    val val1 = ByteString("Value1")

    referenceCountNodeStorage.put(key1, val1)

    val previousSize = dataSource.storage.size
    readOnlyNodeStorage.get(key1).get shouldEqual val1

    readOnlyNodeStorage.remove(key1)

    dataSource.storage.size shouldEqual previousSize
    readOnlyNodeStorage.get(key1).get shouldEqual val1
  }

  trait TestSetup {
    val dataSource = EphemDataSource()
    val nodeStorage = new NodeStorage(dataSource)
    val referenceCountNodeStorage = new ReferenceCountedReadOnlyStorage(new ReferenceCountNodeStorage(nodeStorage, 1))

  }
}


