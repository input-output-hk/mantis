package io.iohk.ethereum.db.storage

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.network.p2p.messages.PV63.MptNode
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class NodeStorageSuite extends FunSuite with PropertyChecks with ObjectGenerators {
  test("NodeStorage insert") {
    forAll(Gen.listOf(nodeGen)){ unfilteredMptNodes =>
      val mptNodes = unfilteredMptNodes.distinct
      val initialNodeStorage = new NodeStorage(EphemDataSource())
      val nodeStorage = mptNodes.foldLeft(initialNodeStorage){
        case (recNodeStorage, node) =>
          recNodeStorage.put(node.hash, rlpEncode(node))
      }

      mptNodes.foreach{ node =>
        val obtainedNode = nodeStorage.get(node.hash).map(rlpDecode[MptNode])
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
          recNodeStorage.put(node.hash, rlpEncode(node))
      }

      //Nodes are deleted
      val (toDelete, toLeave) = mptNodes.splitAt(Gen.choose(0, mptNodes.size).sample.get)
      val nodeStorageAfterDelete = toDelete.foldLeft(nodeStorage){
        case (recNodeStorage, node) =>
          recNodeStorage.remove(node.hash)
      }

      toLeave.foreach{ node =>
        val obtainedNode = nodeStorageAfterDelete.get(node.hash).map(rlpDecode[MptNode])
        assert(obtainedNode.contains(node)) }
      toDelete.foreach{ node => assert(nodeStorageAfterDelete.get(node.hash).isEmpty) }
    }
  }
}
