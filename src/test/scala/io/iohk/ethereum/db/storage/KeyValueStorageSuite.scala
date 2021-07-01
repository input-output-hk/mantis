package io.iohk.ethereum.db.storage

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.dataSource.DataSourceUpdate
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{decode => rlpDecode}
import io.iohk.ethereum.rlp.{encode => rlpEncode}

class KeyValueStorageSuite extends AnyFunSuite with ScalaCheckPropertyChecks with ObjectGenerators {
  val iterationsNumber = 100

  object IntStorage {
    val intNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('i'.toByte)
    val intSerializer: Int => IndexedSeq[Byte] = (i: Int) => rlpEncode(i).toIndexedSeq
    val intDeserializer: IndexedSeq[Byte] => Int =
      (encodedInt: IndexedSeq[Byte]) => rlpDecode[Int](encodedInt.toArray)
  }

  class IntStorage(val dataSource: DataSource) extends KeyValueStorage[Int, Int, IntStorage] {
    import IntStorage._

    type T = IntStorage

    override val namespace: IndexedSeq[Byte] = intNamespace
    override def keySerializer: Int => IndexedSeq[Byte] = intSerializer
    override def keyDeserializer: IndexedSeq[Byte] => Int = intDeserializer
    override def valueSerializer: Int => IndexedSeq[Byte] = intSerializer
    override def valueDeserializer: IndexedSeq[Byte] => Int = intDeserializer

    protected def apply(dataSource: DataSource): IntStorage = new IntStorage(dataSource)
  }

  val initialIntStorage = new IntStorage(EphemDataSource())

  val dataGenerator: Gen[(List[Int], List[Int])] = for {
    intsInStorage <- Gen.nonEmptyListOf(intGen)
    intsNotInStorage <- Gen.nonEmptyListOf(intGen.suchThat(value => !intsInStorage.contains(value)))
  } yield (intsInStorage, intsNotInStorage)

  test("Get ints from KeyValueStorage") {
    forAll(dataGenerator) { case (intsInStorage, intsNotInStorage) =>
      val intsInStorageIndexedSeq = intsInStorage.map(IntStorage.intSerializer(_))
      val initialIntDataSource = EphemDataSource()
      initialIntDataSource.update(
        Seq(DataSourceUpdate(IntStorage.intNamespace, Seq(), intsInStorageIndexedSeq.zip(intsInStorageIndexedSeq)))
      )
      val keyValueStorage = new IntStorage(initialIntDataSource)
      intsInStorage.foreach(i => assert(keyValueStorage.get(i).contains(i)))
      intsNotInStorage.foreach(i => assert(keyValueStorage.get(i).isEmpty))
    }
  }

  test("Insert ints to KeyValueStorage") {
    forAll(Gen.listOfN(iterationsNumber, Gen.listOf(intGen))) { listOfListOfInt =>
      val keyValueStorage = listOfListOfInt.foldLeft(initialIntStorage) { case (recKeyValueStorage, intList) =>
        recKeyValueStorage.update(Seq(), intList.zip(intList))
      }

      listOfListOfInt.flatten.foreach { i =>
        assert(keyValueStorage.get(i).contains(i))
      }
    }
  }

  test("Delete ints from KeyValueStorage") {
    forAll(Gen.listOf(intGen)) { listOfInt =>
      //Insert of keys
      val intStorage = initialIntStorage.update(Seq(), listOfInt.zip(listOfInt))

      //Delete of ints
      val (toDelete, toLeave) = listOfInt.splitAt(Gen.choose(0, listOfInt.size).sample.get)
      val keyValueStorage = intStorage.update(toDelete, Seq())

      toDelete.foreach { i =>
        assert(keyValueStorage.get(i).isEmpty)
      }
      toLeave.foreach { i =>
        assert(keyValueStorage.get(i).contains(i))
      }
    }
  }

  test("Put ints into KeyValueStorage") {
    forAll(Gen.listOf(intGen)) { listOfInt =>
      val keyValueStorage = listOfInt.foldLeft(initialIntStorage) { case (recKeyValueStorage, i) =>
        recKeyValueStorage.put(i, i)
      }

      listOfInt.foreach { i =>
        assert(keyValueStorage.get(i).contains(i))
      }
    }
  }

  test("Remove ints from KeyValueStorage") {
    forAll(Gen.listOf(intGen)) { listOfInt =>
      //Insert of keys
      val intStorage = initialIntStorage.update(Seq(), listOfInt.zip(listOfInt))

      //Delete of ints
      val (toDelete, toLeave) = listOfInt.splitAt(Gen.choose(0, listOfInt.size).sample.get)
      val keyValueStorage = toDelete.foldLeft(intStorage) { case (recKeyValueStorage, i) =>
        recKeyValueStorage.remove(i)
      }

      toDelete.foreach { i =>
        assert(keyValueStorage.get(i).isEmpty)
      }
      toLeave.foreach { i =>
        assert(keyValueStorage.get(i).contains(i))
      }
    }
  }
}
