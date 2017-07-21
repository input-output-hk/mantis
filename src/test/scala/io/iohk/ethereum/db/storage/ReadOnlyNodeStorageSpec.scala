package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import org.scalatest.{FlatSpec, Matchers}

class ReadOnlyNodeStorageSpec extends FlatSpec with Matchers {

  "ReadOnlyNodeStorage" should "not update dataSource" in new TestSetup {
    readOnlyNodeStorage.put(ByteString("key1"), ByteString("Value1").toArray)
    dataSource.storage.size shouldEqual 0
  }

  it should "be able to read from underlying storage but not change it" in new TestSetup {
    val key1 = ByteString("key1")
    val val1 = ByteString("Value1").toArray
    referenceCountNodeStorage.put(key1, val1)

    dataSource.storage.size shouldEqual 1
    readOnlyNodeStorage.get(key1).get shouldEqual val1

    readOnlyNodeStorage.remove(key1)

    dataSource.storage.size shouldEqual 1
  }

  trait TestSetup {
    val dataSource = EphemDataSource()
    val nodeStorage = new NodeStorage(dataSource)

    val referenceCountNodeStorage = new ReferenceCountNodeStorage(nodeStorage, pruningOffset = 0, blockNumber = Some(1))
    val readOnlyNodeStorage = ReadOnlyNodeStorage(referenceCountNodeStorage)
  }
}


