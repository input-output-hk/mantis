package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import org.scalatest.{FlatSpec, Matchers}

class ReferenceCountNodeStorageSpec extends FlatSpec with Matchers {

  "ReferenceCountNodeStorage" should "not remove a key if no more references until pruning" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(1))

    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(4, storage)
    val (key1, val1) = inserted.head

    storage.remove(key1)
    storage.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(1, nodeStorage)

    storage.get(key1) shouldBe None
  }

  it should "not remove a key that was inserted after deletion when pruning" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(1))

    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(1, storage)
    val (key1, val1) :: Nil = inserted.toList

    val storage2 = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(2))
    storage2.remove(key1)
    storage2.get(key1).get shouldEqual val1

    val storage3 = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(3))
    storage3.put(key1, val1)
    storage3.get(key1).get shouldEqual val1

    val storage4 = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(4))
    storage4.remove(key1)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(1, nodeStorage)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(2, nodeStorage)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(3, nodeStorage)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(4, nodeStorage)
    storage3.get(key1) shouldEqual None

  }

  it should "not remove a key that it's still referenced when pruning" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(1))

    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(1, storage)
    val (key1, val1) :: Nil = inserted.toList

    val storage2 = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(2))
    storage2.put(key1, val1)
    storage2.get(key1).get shouldEqual val1

    val storage3 = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(3))
    storage3.remove(key1)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(1, nodeStorage)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(2, nodeStorage)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(3, nodeStorage)
    storage3.get(key1).get shouldEqual val1
  }

  it should "not delete a key that's was referenced in later blocks when pruning" in new TestSetup {

    val storage = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(1))
    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(4, storage)
    val (key1, val1) :: (key2, val2) :: (key3, val3) :: (key4, val4) :: Nil = inserted.toList

    storage.remove(key1) // remove key1 at block 1
    storage.remove(key4) // remove key4 at block 1, it should be pruned

    val storage2 = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(2))

    storage2.put(key1, val1).remove(key1) // add key1 again and remove it at block 2
    storage2.remove(key2).put(key2, val2) // remove and add key2 at block 2
    storage2.remove(key3) // Remove at block 2

    storage2.get(key1).get shouldEqual val1
    storage2.get(key2).get shouldEqual val2
    storage2.get(key3).get shouldEqual val3

    ReferenceCountNodeStorage.prune(1, nodeStorage)
    storage2.get(key1).get shouldEqual val1
    storage2.get(key2).get shouldEqual val2
    storage2.get(key3).get shouldEqual val3
    storage2.get(key4) shouldBe None

    ReferenceCountNodeStorage.prune(2, nodeStorage)
    storage2.get(key1) shouldBe None
    storage2.get(key2).get shouldEqual val2
    storage2.get(key3) shouldBe None
    storage2.get(key4) shouldBe None
  }

  it should "not throw an error when deleting a key that does not exist" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(1))

    storage.remove(ByteString("doesnotexist"))

    dataSource.storage.size shouldEqual 0
  }

  it should "allow to rollback operations" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(1))

    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(4, storage)
    val (key1, val1) :: (key2, val2) :: xs = inserted.toList

    storage.remove(key1).remove(key2)

    val storage2 = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(2))
    val key3 = ByteString("anotherKey")
    val val3: Array[Byte] = ByteString("anotherValue").toArray[Byte]
    storage2.put(key3, val3)

    storage2.get(key3).get shouldEqual val3

    ReferenceCountNodeStorage.rollback(2, nodeStorage)

    storage2.get(key1).get shouldEqual val1
    storage2.get(key2).get shouldEqual val2
    storage2.get(key3) shouldEqual None

  }

  it should "allow rollbacks after pruning" in new TestSetup {

    val storage = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(1))

    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(4, storage)
    val (key1, val1) :: (key2, val2) :: xs = inserted.toList

    storage.remove(key1).remove(key2)

    val storage2 = new ReferenceCountNodeStorage(nodeStorage, blockNumber = Some(2))
    val key3 = ByteString("anotherKey")
    val val3: Array[Byte] = ByteString("anotherValue").toArray[Byte]
    storage2.put(key3, val3)

    dataSource.storage.size shouldEqual (5 + 2 + 7) // 5 keys + 2 block index + 7 snapshots

    ReferenceCountNodeStorage.prune(1, nodeStorage)
    dataSource.storage.size shouldEqual (3 + 1 + 1) // 3 keys + 1 block index + 1 snapshots

    // Data is correct
    storage2.get(key1) shouldEqual None
    storage2.get(key2) shouldEqual None
    storage2.get(key3).get shouldEqual val3

    // We can still rollback without error
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


