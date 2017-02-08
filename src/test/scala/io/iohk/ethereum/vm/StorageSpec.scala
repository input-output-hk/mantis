package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.Generators._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks


class StorageSpec extends FunSuite with PropertyChecks {

  val MaxStorageSize = 10

  test("store") {
    forAll(getStorageGen(MaxStorageSize), getDataWordGen(), getDataWordGen()) {(storage: Storage, k: DataWord, v: DataWord) =>
      val updatedStorage: Storage = storage.store(k, v)
      assert(updatedStorage.toMap == storage.toMap + (k -> v))
      whenever(!storage.toMap.isEmpty) {
        storage.toMap.headOption.foreach { case (k, v) =>
          assert(storage.store(k, v) == storage)
        }
      }
    }
  }

  test("remove") {
    forAll(getStorageGen(MaxStorageSize)) {(storage: Storage) =>
      whenever(!storage.toMap.isEmpty) {
        storage.toMap.headOption.foreach { case (k, v) =>
          val updatedStorage = storage.store(k, DataWord.Zero)
          assert(updatedStorage.load(k) == DataWord.Zero)
          assert(updatedStorage.toMap.get(k).isEmpty)
        }
      }
    }
  }

  test("load") {
    forAll(getStorageGen(MaxStorageSize), getDataWordGen(), getDataWordGen()) {(storage: Storage, k: DataWord, v: DataWord) =>
        assert(storage.store(k, v).load(k) == v)
    }
  }

  test("==") {
    forAll(getListGen(0, MaxStorageSize, getDataWordGen())) {(dataWords: Seq[DataWord]) =>
      assert(Storage.fromSeq(dataWords) == Storage.fromSeq(dataWords))
    }
  }

}
