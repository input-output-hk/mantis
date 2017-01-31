package io.iohk.ethereum.db

import io.iohk.ethereum.ObjectGenerators
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

import scala.util.Random

class NodeStorageSuite extends FunSuite with PropertyChecks with ObjectGenerators {
  test("NodeStorage insert") {
    forAll(Gen.listOf(nodeGen)){ unfilteredMptNodes =>
      val mptNodes = unfilteredMptNodes.distinct
      val initialNodeStorage = NodeStorage(EphemDataSource())
      val nodeStorage = mptNodes.foldLeft(initialNodeStorage){
        case (recNodeStorage, node) =>
          NodeStorage.put(node, recNodeStorage)
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
      val initialNodeStorage = NodeStorage(EphemDataSource())
      val nodeStorage = mptNodes.foldLeft(initialNodeStorage){
        case (recNodeStorage, node) =>
          NodeStorage.put(node, recNodeStorage)
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
