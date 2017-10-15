package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.common.{Removal, Upsert}
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.network.p2p.messages.PV63.MptNodeEncoders._
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class NodeStorageSuite extends FunSuite with PropertyChecks with ObjectGenerators {
  test("NodeStorage insert") {
    forAll(Gen.listOf(nodeGen)){ unfilteredMptNodes =>
      val mptNodes = unfilteredMptNodes.distinct
      val initialNodeStorage: NodeStorage = new NodeStorage(EphemDataSource())
      val nodeStorage = mptNodes.foldLeft(initialNodeStorage){
        case (recNodeStorage, node) =>
          recNodeStorage.update(Seq(Upsert(ByteString(node.hash) -> node.toBytes)))
      }

      mptNodes.foreach{ node =>
        val obtainedNode = nodeStorage.get(ByteString(node.hash)).map(_.toMptNode)
        assert(obtainedNode.contains(node))
      }
    }
  }

  test("NodeStorage delete") {
    forAll(Gen.listOf(nodeGen)){ unfilteredMptNodes =>
      val mptNodes = unfilteredMptNodes.distinct

      //Nodes are inserted
      val initialNodeStorage: NodeStorage = new NodeStorage(EphemDataSource())
      val nodeStorage = mptNodes.foldLeft(initialNodeStorage){
        case (recNodeStorage, node) =>
          recNodeStorage.update(Seq(Upsert(ByteString(node.hash) -> node.toBytes)))
      }

      //Nodes are deleted
      val (toDelete, toLeave) = mptNodes.splitAt(Gen.choose(0, mptNodes.size).sample.get)
      val nodeStorageAfterDelete = toDelete.foldLeft(nodeStorage){
        case (recNodeStorage, node) =>
          recNodeStorage.update(Seq(Removal(ByteString(node.hash))))
      }

      toLeave.foreach{ node =>
        val obtainedNode = nodeStorageAfterDelete.get(ByteString(node.hash)).map(_.toMptNode)
        assert(obtainedNode.contains(node)) }
      toDelete.foreach{ node => assert(nodeStorageAfterDelete.get(ByteString(node.hash)).isEmpty) }
    }
  }
}
