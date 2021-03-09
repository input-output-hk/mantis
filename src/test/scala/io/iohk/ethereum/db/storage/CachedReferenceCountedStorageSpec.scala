package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.cache.LruCache
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.mpt.NodesKeyValueStorage
import io.iohk.ethereum.utils.Config.NodeCacheConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

// scalastyle:off magic.number
class CachedReferenceCountedStorageSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with ObjectGenerators {

  "ChangeLog" should "record all changes" in new TestSetup {
    var blockNumber = 1
    forAll(keyValueByteStringGen(10)) { changes =>
      val toUpdate = changes
      val toDel = changes.take(changes.size / 2).map(_._1)

      changeLog.withChangeLog(blockNumber) { blockChangeLog =>
        toUpdate.foreach { case (key, value) =>
          blockChangeLog.registerChange(Increase(key), 1)
        }

        toDel.foreach { key =>
          blockChangeLog.registerChange(Decrease(key), 0)
        }
      }

      val forBlock = changeLog.getChangeLogForBlock(blockNumber)

      val nodesMarkedForDelete = forBlock.getAllToDelete
      val allUpdatesHistory = forBlock.getAllChanges

      nodesMarkedForDelete should contain theSameElementsAs toDel
      allUpdatesHistory.size shouldEqual (toUpdate.size + toDel.size)

      val (decreased, increased) = scatterUpdates(allUpdatesHistory)

      decreased should contain theSameElementsAs toDel
      increased should contain theSameElementsAs toUpdate.map(_._1)

      blockNumber = blockNumber + 1
    }
  }

  it should "save all recorded changes to storage" in new TestSetup {
    var blockNumber = 1
    forAll(keyValueByteStringGen(10)) { changes =>
      val toUpdate = changes
      val toDel = changes.take(changes.size / 2).map(_._1)

      changeLog.withChangeLog(blockNumber) { blockChangeLog =>
        toUpdate.foreach { case (key, value) =>
          blockChangeLog.registerChange(Increase(key), 1)
        }

        toDel.foreach { key =>
          blockChangeLog.registerChange(Decrease(key), 0)
        }
      }
      changeLog.persistChangeLog(blockNumber)

      val drFromStorage = changeLog.getDeathRowFromStorage(blockNumber)
      val updateLogFromStorage = changeLog.getChangeLogFromStorage(blockNumber)

      assert(drFromStorage.isDefined && updateLogFromStorage.isDefined)

      val deathRow = drFromStorage.get
      val updates = updateLogFromStorage.get

      deathRow should contain theSameElementsAs toDel

      val (decreased, increased) = scatterUpdates(updates)
      decreased should contain theSameElementsAs toDel
      increased should contain theSameElementsAs toUpdate.map(_._1)

      changeLog.removeBlockMetaData(blockNumber)

      val drFromStorage1 = changeLog.getDeathRowFromStorage(blockNumber)
      val updateLogFromStorage1 = changeLog.getChangeLogFromStorage(blockNumber)

      assert(drFromStorage1.isEmpty && updateLogFromStorage1.isEmpty)

      blockNumber = blockNumber + 1
    }
  }

  "CachedReferenceCountedStorage" should "prune not referenced nodes " in new TestSetup {
    val storage = updateStorage(1) { stor =>
      stor.update(generateKeys(5).map(_._1), generateKeys(10))
    }
    val storage1 = updateStorage(2) { stor =>
      stor.update(Nil, generateKeys(to = 20, from = 11))
    }

    assertKeysExists(storage1, generateKeys(20))

    // No deletes were made
    assert(testLruCache.getValues.size == 20)
    // No updates in db, only meta data: Changelog and DeathRow
    assert(dataSource.storage.size == 4)
    val deathrow = changeLog.getDeathRowFromStorage(1).get
    CachedReferenceCountedStorage.prune(deathrow, testLruCache, 1)

    assertKeysExists(storage1, generateKeys(20, 6))
    // Pruned 5 nodes marked for delete
    assert(testLruCache.getValues.size == 15)
  }

  it should "not prune nodes which became referenced" in new TestSetup {
    val storage = updateStorage(1) { stor =>
      stor.update(generateKeys(5).map(_._1), generateKeys(10))
    }

    val reAllocatedKey = generateKeys(1).head._1
    val storage1 = updateStorage(2) { stor =>
      // One of potentialy deltable keys is allocated from other block
      stor.update(Nil, generateKeys(1))
      stor.update(Nil, generateKeys(to = 20, from = 11))
    }

    // No deletes were made
    assert(testLruCache.getValues.size == 20)
    // No updates in db, only meta data: Changelog and DeathRow
    assert(dataSource.storage.size == 4)
    val deathrow = changeLog.getDeathRowFromStorage(1).get
    CachedReferenceCountedStorage.prune(deathrow, testLruCache, 1)

    // Pruned 4 nodes marked for delete, left one which became re-allocated
    assert(testLruCache.getValues.size == 16)

    val reAllocatedValue = testLruCache.get(reAllocatedKey)
    assert(reAllocatedValue.isDefined)
    val value = reAllocatedValue.get
    assert(value.numOfParents == 1 && value.bn == 2)
  }

  it should "enable roll-backing changes made by block" in new TestSetup {
    val storage = updateStorage(1) { stor =>
      stor.update(generateKeys(5).map(_._1), generateKeys(10))
    }
    val cacheStateBeforeChanges = testLruCache.getValues
    assert(cacheStateBeforeChanges.size == 10)

    val storage1 = updateStorage(2) { stor =>
      // 5 new nodes which need to be deleted during rollback
      stor.update(generateKeys(to = 10, from = 8).map(_._1), generateKeys(3) ++ generateKeys(15, 11))
      stor.update(Nil, generateKeys(15, 11))
    }

    assert(testLruCache.getValues.size == 15)

    val changes = changeLog.getChangeLogFromStorage(2).get

    // meta data from 2 block (2 death row + 2 change logs)
    assert(dataSource.storage.size == 4)

    CachedReferenceCountedStorage.rollback(testLruCache, nodeStorage, changes, 2)

    val cacheStateAfterRollback = testLruCache.getValues
    assert(cacheStateAfterRollback.size == 10)

    // All changes to reference counts, and possible new nodes are reversed
    cacheStateBeforeChanges should contain theSameElementsAs cacheStateAfterRollback
  }

  it should "flush exising nodes to disk" in new TestSetup {
    val storage = updateStorage(1) { stor =>
      stor.update(generateKeys(5).map(_._1), generateKeys(10))
    }
    val storage1 = updateStorage(2) { stor =>
      stor.update(Nil, generateKeys(to = 20, from = 11))
    }

    assertKeysExists(storage1, generateKeys(20))

    val result = CachedReferenceCountedStorage.persistCache(testLruCache, nodeStorage)

    if (result) {
      assert(testLruCache.getValues.isEmpty)
      assertKeysExists(storage1, generateKeys(20))
    }
  }

  trait TestSetup {
    val dataSource = EphemDataSource()
    val nodeStorage = new NodeStorage(dataSource)
    val changeLog = new ChangeLog(nodeStorage)

    object TestCacheConfig extends NodeCacheConfig {
      override val maxSize: Long = 100
      override val maxHoldTime: FiniteDuration = FiniteDuration(1, TimeUnit.NANOSECONDS)
    }

    val testLruCache = new LruCache[ByteString, HeapEntry](
      TestCacheConfig,
      Some(CachedReferenceCountedStorage.saveOnlyNotificationHandler(nodeStorage))
    )

    def generateKeys(to: Int, from: Int = 1): List[(ByteString, Array[Byte])] =
      (from to to).map(i => kec256(ByteString(s"key$i")) -> ByteString(s"value$i").toArray[Byte]).toList

    def insertRangeKeys(n: Int, storage: NodesKeyValueStorage): Seq[(ByteString, Array[Byte])] = {
      val toInsert = generateKeys(n)
      toInsert.foreach(i => storage.put(i._1, i._2))
      toInsert
    }

    def scatterUpdates(updates: List[Update]): (List[ByteString], List[ByteString]) =
      updates.foldLeft(List.empty[ByteString], List.empty[ByteString]) { (acc, up) =>
        up match {
          case Increase(hash) => acc.copy(_2 = hash :: acc._2)
          case New(hash)      => acc.copy(_2 = hash :: acc._2)
          case Decrease(hash) => acc.copy(_1 = hash :: acc._1)
        }
      }

    def updateStorage(bn: BigInt)(update: NodesKeyValueStorage => Unit): NodesKeyValueStorage = {
      val storage = new CachedReferenceCountedStorage(nodeStorage, testLruCache, changeLog, bn)
      update(storage)
      changeLog.persistChangeLog(bn)
      storage
    }

    def assertKeysExists(storage: NodesKeyValueStorage, keys: List[(ByteString, Array[Byte])]): Unit =
      keys.foreach { case (key, value) =>
        assert(storage.get(key).exists(enc => enc.sameElements(value)))
      }
  }
}
