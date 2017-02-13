package io.iohk.ethereum.db.storage

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import io.iohk.ethereum.rlp.{encode => rlpEncode, decode => rlpDecode}
import io.iohk.ethereum.network.p2p.messages.PV63.MptNode

class NodeStorageSuite extends FunSuite with PropertyChecks with ObjectGenerators {
  test("NodeStorage insert") {
    forAll(Gen.listOf(nodeGen)){ unfilteredMptNodes =>
      val mptNodes = unfilteredMptNodes.distinct
      val initialNodeStorage = new NodeStorage(EphemDataSource())
      val nodeStorage = mptNodes.foldLeft(initialNodeStorage){
        case (recNodeStorage, node) =>
          recNodeStorage.put(node.hash.toArray, rlpEncode(node))
      }

      mptNodes.foreach{ node =>
        val obtainedNode = nodeStorage.get(node.hash.toArray).map(rlpDecode[MptNode])
        assert(obtainedNode.contains(node))
      }
    }
  }

  test("NodeStorage delete") {
    forAll(Gen.listOf(nodeGen)){ unfilteredMptNodes =>
      val mptNodes = unfilteredMptNodes.distinct

      //Nodes are inserted
      val initialNodeStorage = new NodeStorage(EphemDataSource())
      val nodeStorage = mptNodes.foldLeft(initialNodeStorage){
        case (recNodeStorage, node) =>
          recNodeStorage.put(node.hash.toArray, rlpEncode(node))
      }

      //Nodes are deleted
      val (toDelete, toLeave) = mptNodes.splitAt(Gen.choose(0, mptNodes.size).sample.get)
      val nodeStorageAfterDelete = toDelete.foldLeft(nodeStorage){
        case (recNodeStorage, node) =>
          recNodeStorage.remove(node.hash.toArray)
      }

      toLeave.foreach{ node =>
        val obtainedNode = nodeStorageAfterDelete.get(node.hash.toArray).map(rlpDecode[MptNode])
        assert(obtainedNode.contains(node)) }
      toDelete.foreach{ node => assert(nodeStorageAfterDelete.get(node.hash.toArray).isEmpty) }
    }
  }
}
