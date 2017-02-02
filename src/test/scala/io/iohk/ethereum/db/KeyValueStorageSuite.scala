package io.iohk.ethereum.db

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.network.p2p.messages.PV63.MptNode
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}
import io.iohk.ethereum.rlp.RLPImplicits._

import scala.util.Random

class KeyValueStorageSuite extends FunSuite with PropertyChecks with ObjectGenerators{
  val iterationsNumber = 100

  class IntStorage(val dataSource: DataSource) extends KeyValueStorage[Int, Int] {
    type T = IntStorage

    override def keySerializer: Int => Array[Byte] = rlpEncode(_)
    override def valueSerializer: Int => Array[Byte] = rlpEncode(_)
    override def valueDeserializer: Array[Byte] => Int = rlpDecode[Int]

    def apply(dataSource: DataSource): IntStorage = new IntStorage(dataSource)
  }

  val initialIntStorage = new IntStorage(EphemDataSource())

  test("Insert ints to KeyValueStorage") {
    forAll(Gen.listOfN(iterationsNumber, Gen.listOf(intGen))) { listOfListOfInt =>

      val keyValueStorage = listOfListOfInt.foldLeft(initialIntStorage){ case (recKeyValueStorage, intList) =>
        recKeyValueStorage.update(Seq(), intList.zip(intList))
      }

      listOfListOfInt.flatten.foreach{ i =>
        assert(keyValueStorage.get(i).contains(i))
      }
    }
  }

  test("Delete ints from KeyValueStorage") {
    forAll(Gen.listOf(intGen)) { listOfInt =>
      //Insert of keys
      val intStorage = initialIntStorage.update(Seq(), listOfInt.zip(listOfInt))

      //Delete of ints
      val (toDelete, toLeave) = Random.shuffle(listOfInt).splitAt(Gen.choose(0, listOfInt.size).sample.get)
      val keyValueStorage = toDelete.foldLeft(intStorage){ case (recKeyValueStorage, i) =>
        recKeyValueStorage.remove(i)
      }

      toDelete.foreach{ i =>
        assert(keyValueStorage.get(i).isEmpty)
      }
      toLeave.foreach{ i =>
        assert(keyValueStorage.get(i).contains(i))
      }
    }
  }
}
