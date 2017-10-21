package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import org.scalatest.{FlatSpec, Matchers}

class ReferenceCountNodeStorageSpec extends FlatSpec with Matchers {

  "ReferenceCountNodeStorage" should "should remove a key if no more references" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(1))

    val inserted = insertRangeKeys(4, storage)
    val (key1, val1) = inserted.head

    storage.remove(key1)
    storage.get(key1) shouldBe None
  }

  it should "not delete a key that's still referenced" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(1))

    val inserted = insertRangeKeys(4, storage)
    val (key1, val1) :: (key2, val2) :: xs = inserted.toList

    storage.put(key1, val1).remove(key1) // add key1 again and remove it
    storage.remove(key2).put(key2, val2) // remove and add key2

    storage.get(key1).get sameElements val1.toArray[Byte]
    storage.get(key2).get sameElements val2.toArray[Byte]

    // After removal, they should be remmoved
    storage.remove(key1)
    storage.remove(key2)
    storage.get(key1) shouldEqual None
    storage.get(key2) shouldEqual None
  }

  it should "not throw an error when deleting a key that does not exist" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(1))

    storage.remove(ByteString("doesnotexist"))

    dataSource.storage.size shouldEqual 0
  }

  it should "allow to rollback operations within the same block" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(1))

    val inserted = insertRangeKeys(4, storage)
    val (key1, val1) :: (key2, val2) :: xs = inserted.toList

    storage.remove(key1).remove(key2)
    val key3 = ByteString("anotherKey")
    val val3 = ByteString("anotherValue").toArray[Byte]
    storage.put(key3, val3)

    storage.get(key1) shouldEqual None
    storage.get(key2) shouldEqual None
    storage.get(key3).get sameElements val3

    ReferenceCountNodeStorage.rollback(1, nodeStorage)

    storage.get(key1).get sameElements val1
    storage.get(key2).get sameElements val2
    storage.get(key3) shouldEqual None

  }

  it should "allow to rollback operations from a specific block" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(1))

    val inserted = insertRangeKeys(4, storage)
    val (key1, val1) :: (key2, val2) :: xs = inserted.toList

    storage.remove(key1).remove(key2)
    val key3 = ByteString("anotherKey")
    val val3 = ByteString("anotherValue").toArray[Byte]
    val storage2 = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(2))
    storage2.put(key3, val3)

    storage2.get(key1) shouldEqual None
    storage2.get(key2) shouldEqual None
    storage2.get(key3).get sameElements val3

    ReferenceCountNodeStorage.rollback(2, nodeStorage)

    storage2.get(key1) shouldEqual None
    storage2.get(key2) shouldEqual None
    storage2.get(key3) shouldEqual None

  }

  it should "allow pruning" in new TestSetup {

    val storage = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(1))

    val inserted = insertRangeKeys(4, storage)
    val (key1, val1) :: (key2, val2) :: xs = inserted.toList

    storage.remove(key1).remove(key2)

    val storage2 = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(2))
    val key3 = ByteString("anotherKey")
    val val3 = ByteString("anotherValue").toArray[Byte]
    storage2.put(key3, val3)

    val prevDatasourceSize: Int = dataSource.storage.size
    ReferenceCountNodeStorage.prune(1, nodeStorage)
    (dataSource.storage.size < prevDatasourceSize) shouldBe true

    // Data is correct
    storage2.get(key1) shouldEqual None
    storage2.get(key2) shouldEqual None
    storage2.get(key3).get sameElements val3

    // We can still rollback
    ReferenceCountNodeStorage.rollback(2, nodeStorage)
    ReferenceCountNodeStorage.rollback(1, nodeStorage)
    storage2.get(key3) shouldEqual None

  }

  trait TestSetup {
    val dataSource = EphemDataSource()
    val nodeStorage = new NodeStorage(dataSource)

    def insertRangeKeys(n: Int, storage: ReferenceCountNodeStorage): Seq[(ByteString, Array[Byte])] = {
      val toInsert = (1 to n).map(i => ByteString(s"key$i") -> ByteString(s"value$i").toArray[Byte])
      toInsert.foreach(i => storage.put(i._1, i._2))
      toInsert
    }
  }
}


