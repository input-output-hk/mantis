package io.iohk.ethereum.db.storage

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

import scala.util.Random

class MptNodeStorageSuite extends FunSuite with PropertyChecks with ObjectGenerators {
  test("NodeStorage insert") {
    forAll(Gen.listOf(nodeGen)){ unfilteredMptNodes =>
      val mptNodes = unfilteredMptNodes.distinct
      val initialNodeStorage = new MptNodeStorage(EphemDataSource())
      val nodeStorage = mptNodes.foldLeft(initialNodeStorage){
        case (recNodeStorage, node) =>
          recNodeStorage.put(node)
      }

      mptNodes.foreach{ node =>
        assert(nodeStorage.get(node.hash).contains(node))
      }
    }
  }

  test("NodeStorage delete") {
    forAll(Gen.listOf(nodeGen)){ unfilteredMptNodes =>
      val mptNodes = unfilteredMptNodes.distinct

      //Nodes are inserted
      val initialNodeStorage = new MptNodeStorage(EphemDataSource())
      val nodeStorage = mptNodes.foldLeft(initialNodeStorage){
        case (recNodeStorage, node) =>
          recNodeStorage.put(node)
      }

      //Nodes are deleted
      val (toDelete, toLeave) = Random.shuffle(mptNodes)
        .splitAt(Gen.choose(0, mptNodes.size).sample.get)
      val nodeStorageAfterDelete = toDelete.foldLeft(nodeStorage){
        case (recNodeStorage, node) =>
          recNodeStorage.remove(node.hash)
      }

      toLeave.foreach{ node => assert(nodeStorageAfterDelete.get(node.hash).contains(node)) }
      toDelete.foreach{ node => assert(nodeStorageAfterDelete.get(node.hash).isEmpty) }
    }
  }
}
