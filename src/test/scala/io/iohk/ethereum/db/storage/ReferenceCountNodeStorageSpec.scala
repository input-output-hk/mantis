package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.pruning.PruneResult
import org.scalatest.{FlatSpec, Matchers}

class ReferenceCountNodeStorageSpec extends FlatSpec with Matchers {

  "ReferenceCountNodeStorage" should "prune nodes releasing dataSource space" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, pruningOffset = 0, blockNumber = Some(1))

    val inserted = insertRangeKeys(4, storage)
    val (key1, val1) = inserted.head

    dataSource.storage.size shouldEqual 4

    storage.remove(key1)
    storage.get(key1).get sameElements val1.toArray[Byte] // Data exists until pruning

    storage.prune(0, 2) shouldEqual PruneResult(1, pruned = 1)
    storage.get(key1) shouldBe None // Data exists until pruning
    dataSource.storage.size shouldEqual 3
  }

  it should "not prune any data if no values were removed" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, pruningOffset = 0, blockNumber = Some(1))

    insertRangeKeys(3, storage)
    storage.prune(0, 2) shouldEqual PruneResult(1, pruned = 0)
  }

  it should "not delete a key that's still referenced" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, pruningOffset = 0, blockNumber = Some(1))

    val inserted = insertRangeKeys(4, storage)
    val (key1, val1) :: (key2, val2) :: xs = inserted.toList

    storage.put(key1, val1).remove(key1) // add key1 again and remove it
    storage.remove(key2).put(key2, val2) // remove and add key2

    storage.get(key1).get sameElements val1.toArray[Byte] // Data exists until pruning
    storage.get(key2).get sameElements val2.toArray[Byte] // Data exists until pruning

    storage.prune(0, 2) shouldEqual PruneResult(1, pruned = 0)
    storage.get(key1).get sameElements val1 // Data after pruning
    storage.get(key2).get sameElements val2 // Data after pruning
    dataSource.storage.size shouldEqual 4
  }

  it should "not throw an error when deleting a key that does not exist" in new TestSetup {
    val storage = new ReferenceCountNodeStorage(nodeStorage, pruningOffset = 0, blockNumber = Some(1))

    storage.remove(ByteString("doesnotexist"))

    dataSource.storage.size shouldEqual 0
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


