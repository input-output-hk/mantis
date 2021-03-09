package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.cache.MapCache
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.mpt.NodesKeyValueStorage
import io.iohk.ethereum.utils.Config.NodeCacheConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

class ReferenceCountNodeStorageSpec extends AnyFlatSpec with Matchers {

  "ReferenceCountNodeStorage" should "not remove a key if no more references until pruning" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, 1)

    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(4, storage)
    val (key1, val1) = inserted.head

    storage.remove(key1)
    storage.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(1, nodeStorage, inMemory = false)

    storage.get(key1) shouldBe None
  }

  it should "not remove a key that was inserted after deletion when pruning" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, bn = 1)

    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(1, storage)
    val (key1, val1) :: Nil = inserted.toList

    val storage2 = new ReferenceCountNodeStorage(nodeStorage, bn = 2)
    storage2.remove(key1)
    storage2.get(key1).get shouldEqual val1

    val storage3 = new ReferenceCountNodeStorage(nodeStorage, bn = 3)
    storage3.put(key1, val1)
    storage3.get(key1).get shouldEqual val1

    val storage4 = new ReferenceCountNodeStorage(nodeStorage, bn = 4)
    storage4.remove(key1)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(1, nodeStorage, inMemory = false)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(2, nodeStorage, inMemory = false)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(3, nodeStorage, inMemory = false)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(4, nodeStorage, inMemory = false)
    storage3.get(key1) shouldEqual None

  }

  it should "not remove a key that it's still referenced when pruning" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, bn = 1)

    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(1, storage)
    val (key1, val1) :: Nil = inserted.toList

    val storage2 = new ReferenceCountNodeStorage(nodeStorage, bn = 2)
    storage2.put(key1, val1)
    storage2.get(key1).get shouldEqual val1

    val storage3 = new ReferenceCountNodeStorage(nodeStorage, bn = 3)
    storage3.remove(key1)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(1, nodeStorage, inMemory = false)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(2, nodeStorage, inMemory = false)
    storage3.get(key1).get shouldEqual val1

    ReferenceCountNodeStorage.prune(3, nodeStorage, inMemory = false)
    storage3.get(key1).get shouldEqual val1
  }

  it should "not delete a key that's was referenced in later blocks when pruning" in new TestSetup {

    val storage = new ReferenceCountNodeStorage(nodeStorage, bn = 1)
    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(4, storage)
    val (key1, val1) :: (key2, val2) :: (key3, val3) :: (key4, val4) :: Nil = inserted.toList

    storage.remove(key1) // remove key1 at block 1
    storage.remove(key4) // remove key4 at block 1, it should be pruned

    val storage2 = new ReferenceCountNodeStorage(nodeStorage, bn = 2)

    storage2.put(key1, val1).remove(key1) // add key1 again and remove it at block 2
    storage2.remove(key2).put(key2, val2) // remove and add key2 at block 2
    storage2.remove(key3) // Remove at block 2

    storage2.get(key1).get shouldEqual val1
    storage2.get(key2).get shouldEqual val2
    storage2.get(key3).get shouldEqual val3

    ReferenceCountNodeStorage.prune(1, nodeStorage, inMemory = false)
    storage2.get(key1).get shouldEqual val1
    storage2.get(key2).get shouldEqual val2
    storage2.get(key3).get shouldEqual val3
    storage2.get(key4) shouldBe None

    ReferenceCountNodeStorage.prune(2, nodeStorage, inMemory = false)
    storage2.get(key1) shouldBe None
    storage2.get(key2).get shouldEqual val2
    storage2.get(key3) shouldBe None
    storage2.get(key4) shouldBe None
  }

  it should "not throw an error when deleting a key that does not exist" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, bn = 1)

    storage.remove(ByteString("doesnotexist"))

    dataSource.storage.size shouldEqual 0
  }

  it should "allow to rollback operations" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, bn = 1)

    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(4, storage)
    val (key1, val1) :: (key2, val2) :: xs = inserted.toList

    storage.remove(key1).remove(key2)

    val storage2 = new ReferenceCountNodeStorage(nodeStorage, bn = 2)
    val key3 = ByteString("anotherKey")
    val val3: Array[Byte] = ByteString("anotherValue").toArray[Byte]
    storage2.put(key3, val3)

    storage2.get(key3).get shouldEqual val3

    ReferenceCountNodeStorage.rollback(2, nodeStorage, inMemory = false)

    storage2.get(key1).get shouldEqual val1
    storage2.get(key2).get shouldEqual val2
    storage2.get(key3) shouldEqual None
  }

  it should "allow rollbacks after pruning" in new TestSetup {

    val storage = new ReferenceCountNodeStorage(nodeStorage, bn = 1)

    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(4, storage)
    val (key1, val1) :: (key2, val2) :: xs = inserted.toList

    storage.remove(key1).remove(key2)

    val storage2 = new ReferenceCountNodeStorage(nodeStorage, bn = 2)
    val key3 = ByteString("anotherKey")
    val val3: Array[Byte] = ByteString("anotherValue").toArray[Byte]
    storage2.put(key3, val3)

    dataSource.storage.size shouldEqual (1 + 5 + 2 + 7) // 1 deathRowKey + 5 keys + 2 block index + 7 snapshots

    ReferenceCountNodeStorage.prune(1, nodeStorage, inMemory = false)
    dataSource.storage.size shouldEqual (3 + 1 + 1) // 3 keys + 1 block index + 1 snapshots

    // Data is correct
    storage2.get(key1) shouldEqual None
    storage2.get(key2) shouldEqual None
    storage2.get(key3).get shouldEqual val3

    // We can still rollback without error
    ReferenceCountNodeStorage.rollback(2, nodeStorage, inMemory = false)
    ReferenceCountNodeStorage.rollback(1, nodeStorage, inMemory = false)
    storage2.get(key3) shouldEqual None

  }

  it should "not save snapshots when requested" in new TestSetup {
    val storage = new FastSyncNodeStorage(nodeStorage, bn = 1)
    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(4, storage)
    dataSource.storage.size shouldEqual inserted.size // only inserted keys, no additional data
  }

  it should "allow rollbacks after pruning in memory" in new TestSetup {

    val storage = new ReferenceCountNodeStorage(cachedNodeStorage, bn = 1)

    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(4, storage)
    val (key1, val1) :: (key2, val2) :: xs = inserted.toList

    storage.remove(key1).remove(key2)

    val storage2 = new ReferenceCountNodeStorage(cachedNodeStorage, bn = 2)
    val key3 = ByteString("anotherKey")
    val val3: Array[Byte] = ByteString("anotherValue").toArray[Byte]
    storage2.put(key3, val3)

    underlying.size shouldEqual (1 + 5 + 2 + 7) // 1 deathrowkey + 5 keys + 2 block index + 7 snapshots

    ReferenceCountNodeStorage.prune(1, cachedNodeStorage, inMemory = true)
    underlying.size shouldEqual (3 + 1 + 1) // 3 keys + 1 block index + 1 snapshots

    // Data is correct
    storage2.get(key1) shouldEqual None
    storage2.get(key2) shouldEqual None
    storage2.get(key3).get shouldEqual val3

    // We can still rollback without error
    ReferenceCountNodeStorage.rollback(2, cachedNodeStorage, inMemory = true)
    ReferenceCountNodeStorage.rollback(1, cachedNodeStorage, inMemory = true)
    storage2.get(key3) shouldEqual None
  }

  it should "allow pruning which happens partially on disk, partially in memory" in new TestSetup {

    val storage = new ReferenceCountNodeStorage(cachedNodeStorage, bn = 1)

    insertRangeKeys(1, storage)

    val storage2 = new ReferenceCountNodeStorage(cachedNodeStorage, bn = 2)

    insertRangeKeys(1, storage2)

    val storage3 = new ReferenceCountNodeStorage(cachedNodeStorage, bn = 3)

    insertRangeKeys(1, storage3)

    // we are still in memory as cache size = 7 < 10
    cachedNodeStorage.persist() shouldEqual false
    dataSource.storage.size shouldEqual 0
    underlying.size shouldEqual 7 // 1 key + 3 block indexex + 3 snapshots

    new ReferenceCountNodeStorage(cachedNodeStorage, bn = 4)

    insertRangeKeys(4, storage3)
    ReferenceCountNodeStorage.prune(1, cachedNodeStorage, inMemory = true)

    // Number of nodes in cache > maxsize, so everything goes to data source, including unpruned blocks 2,3,4
    cachedNodeStorage.persist() shouldEqual true
    dataSource.storage.size shouldEqual 12
    underlying.size shouldEqual 0

    // Now as our block to prune(2) is <= best saved block(4), we need to prune junk from disk
    new ReferenceCountNodeStorage(cachedNodeStorage, bn = 5)
    insertRangeKeys(4, storage3)
    ReferenceCountNodeStorage.prune(2, cachedNodeStorage, inMemory = false)

    cachedNodeStorage.persist() shouldEqual false //
    underlying.size shouldEqual 9 // 4 keys + 4 snapshots + 1 block index
    dataSource.storage.size shouldEqual 10 // pruned 1 snapshot and 1 block index from disk from block 2
  }

  it should "allow to rollback operations which happens partially on disk, partially in memory" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(cachedNodeStorage, bn = 1)

    val inserted: Seq[(ByteString, Array[Byte])] = insertRangeKeys(4, storage)
    val (key1, val1) :: (key2, val2) :: xs = inserted.toList

    storage.remove(key1).remove(key2)

    val storage2 = new ReferenceCountNodeStorage(cachedNodeStorage, bn = 2)
    val key3 = ByteString("anotherKey")
    val val3: Array[Byte] = ByteString("anotherValue").toArray[Byte]
    storage2.put(key3, val3)
    storage2.get(key3).get shouldEqual val3

    cachedNodeStorage.persist() shouldEqual true
    underlying.size shouldEqual 0
    dataSource.storage.size shouldEqual 15

    val storage3 = new ReferenceCountNodeStorage(cachedNodeStorage, bn = 3)
    val key4 = ByteString("aanotherKey")
    val val4: Array[Byte] = ByteString("aanotherValue").toArray[Byte]
    storage3.put(key4, val4)
    storage3.get(key4).get shouldEqual val4

    cachedNodeStorage.persist() shouldEqual false
    underlying.size shouldEqual 3 // 1 key + 1 snapshot + 1 blockindex

    storage3.get(key4).get shouldEqual val4
    // Best saved block is 2, so all block 3 data are in memory
    ReferenceCountNodeStorage.rollback(3, cachedNodeStorage, inMemory = true)
    storage3.get(key4) shouldEqual None

    storage3.get(key3).get shouldEqual val3
    // Best saved block is 2, so all block 2 data are on disk
    ReferenceCountNodeStorage.rollback(2, cachedNodeStorage, inMemory = false)
    storage3.get(key3) shouldEqual None
  }

  trait TestSetup {
    val dataSource = EphemDataSource()
    val nodeStorage = new NodeStorage(dataSource)

    def insertRangeKeys(n: Int, storage: NodesKeyValueStorage): Seq[(ByteString, Array[Byte])] = {
      val toInsert = (1 to n).map(i => kec256(ByteString(s"key$i")) -> ByteString(s"value$i").toArray[Byte])
      toInsert.foreach(i => storage.put(i._1, i._2))
      toInsert
    }

    object testCacheConfig extends NodeCacheConfig {
      override val maxSize = 10
      override val maxHoldTime = FiniteDuration(5, TimeUnit.MINUTES)
    }

    val underlying = MapCache.getMap[ByteString, Array[Byte]]
    val cache = new MapCache[ByteString, Array[Byte]](underlying, testCacheConfig)
    val cachedNodeStorage = new CachedNodeStorage(nodeStorage, cache)

  }
}
