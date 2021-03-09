package io.iohk.ethereum.db.storage

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.{DataSource, DataSourceUpdate, EphemDataSource}
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TransactionalKeyValueStorageSuite extends AnyFunSuite with ScalaCheckPropertyChecks with ObjectGenerators {
  val iterationsNumber = 100

  object IntStorage {
    val intNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('i'.toByte)
    val intSerializer: Int => IndexedSeq[Byte] = (i: Int) => rlpEncode(i).toIndexedSeq
    val intDeserializer: IndexedSeq[Byte] => Int =
      (encodedInt: IndexedSeq[Byte]) => rlpDecode[Int](encodedInt.toArray)
  }

  class IntStorage(val dataSource: DataSource) extends TransactionalKeyValueStorage[Int, Int] {
    import IntStorage._

    override val namespace: IndexedSeq[Byte] = intNamespace
    override def keySerializer: Int => IndexedSeq[Byte] = intSerializer
    override def keyDeserializer: IndexedSeq[Byte] => Int = intDeserializer
    override def valueSerializer: Int => IndexedSeq[Byte] = intSerializer
    override def valueDeserializer: IndexedSeq[Byte] => Int = intDeserializer
  }

  def newIntStorage(): IntStorage = new IntStorage(EphemDataSource())

  val dataGenerator: Gen[(List[Int], List[Int])] = for {
    intsInStorage <- Gen.nonEmptyListOf(intGen)
    intsNotInStorage <- Gen.nonEmptyListOf(intGen.suchThat(value => !intsInStorage.contains(value)))
  } yield (intsInStorage, intsNotInStorage)

  test("Get ints from KeyValueStorage") {
    forAll(dataGenerator) { case (intsInStorage, intsNotInStorage) =>
      val intsInStorageIndexedSeq = intsInStorage.map(IntStorage.intSerializer(_))
      val intDataSource = EphemDataSource()
      intDataSource.update(
        Seq(DataSourceUpdate(IntStorage.intNamespace, Seq(), intsInStorageIndexedSeq.zip(intsInStorageIndexedSeq)))
      )
      val keyValueStorage = new IntStorage(intDataSource)

      intsInStorage.foreach { i =>
        assert(keyValueStorage.get(i).contains(i))
      }
      intsNotInStorage.foreach { i =>
        assert(keyValueStorage.get(i).isEmpty)
      }
    }
  }

  test("Insert ints to KeyValueStorage") {
    forAll(Gen.listOfN(iterationsNumber, Gen.listOf(intGen))) { listOfListOfInt =>
      val keyValueStorage = newIntStorage()

      listOfListOfInt.foreach { intList =>
        keyValueStorage.update(Seq(), intList.zip(intList)).commit()
      }

      listOfListOfInt.flatten.foreach { i =>
        assert(keyValueStorage.get(i).contains(i))
      }
    }
  }

  test("Delete ints from KeyValueStorage") {
    forAll(Gen.listOf(intGen)) { listOfInt =>
      //Insert of keys
      val intStorage = newIntStorage()
      intStorage.update(Seq(), listOfInt.zip(listOfInt)).commit()

      //Delete of ints
      val (toDelete, toLeave) = listOfInt.splitAt(Gen.choose(0, listOfInt.size).sample.get)
      intStorage.update(toDelete, Seq()).commit()

      toDelete.foreach { i =>
        assert(intStorage.get(i).isEmpty)
      }
      toLeave.foreach { i =>
        assert(intStorage.get(i).contains(i))
      }
    }
  }

  test("Put ints into KeyValueStorage") {
    forAll(Gen.listOf(intGen)) { listOfInt =>
      val keyValueStorage = newIntStorage()

      val batchUpdates = listOfInt.foldLeft(keyValueStorage.emptyBatchUpdate) { case (updates, i) =>
        updates.and(keyValueStorage.put(i, i))
      }

      batchUpdates.commit()

      listOfInt.foreach { i =>
        assert(keyValueStorage.get(i).contains(i))
      }
    }
  }

  test("Remove ints from KeyValueStorage") {
    forAll(Gen.listOf(intGen)) { listOfInt =>
      //Insert of keys
      val intStorage = newIntStorage()
      intStorage.update(Seq(), listOfInt.zip(listOfInt)).commit()

      //Delete of ints
      val (toDelete, toLeave) = listOfInt.splitAt(Gen.choose(0, listOfInt.size).sample.get)
      val batchUpdates = toDelete.foldLeft(intStorage.emptyBatchUpdate) { case (updates, i) =>
        updates.and(intStorage.remove(i))
      }
      batchUpdates.commit()

      toDelete.foreach { i =>
        assert(intStorage.get(i).isEmpty)
      }
      toLeave.foreach { i =>
        assert(intStorage.get(i).contains(i))
      }
    }
  }
}
