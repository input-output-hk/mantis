package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import io.iohk.ethereum.vm.Generators._
import org.scalacheck.Gen

class ArbitraryIntegerMptSpec extends FlatSpec with Matchers with PropertyChecks {

  implicit val bigIntListNoShrink = noShrink[List[BigInt]]

  def keyGen: Gen[BigInt] = byteArrayOfNItemsGen(128).map(BigInt.apply)
  def valueGen: Gen[BigInt] = byteArrayOfNItemsGen(128).map(BigInt.apply)

  "ArbitraryIntegerMpt" should "insert and retrieve values" in new TestSetup {
    forAll(Gen.listOfN(10, keyGen), Gen.listOfN(10, valueGen)) { (keys, values) =>
      val afterInsert = emptyMpt.update(Nil, keys zip values)

      (keys zip values).foreach { case (k, v) =>
        afterInsert.get(k) shouldBe Some(v)
      }
    }
  }

  it should "remove values" in new TestSetup {
    forAll(Gen.listOfN(10, keyGen), Gen.listOfN(10, valueGen)) { (keys, values) =>
      val afterInsert =
        emptyMpt.update(Nil, keys zip values)

      (keys zip values).foreach { case (k, v) =>
        afterInsert.get(k) shouldBe Some(v)
      }

      // remove every 2nd key
      val afterRemove = (keys zip values).zipWithIndex.filter(_._2 % 2 == 0).foldLeft(afterInsert) { case (mpt, ((k, _), _)) =>
        mpt.remove(k)
      }

      (keys zip values).zipWithIndex.foreach {
        case ((k, _), index) if index % 2 == 0 => afterRemove.get(k) shouldBe None
        case ((k, v), index) if index % 2 != 0 => afterRemove.get(k) shouldBe Some(v)
      }
    }
  }

  trait TestSetup extends EphemBlockchainTestSetup {
    val emptyMpt = ArbitraryIntegerMpt.storageMpt(ByteString(MerklePatriciaTrie.EmptyRootHash),
      blockchain.nodesKeyValueStorageFor(None, storagesInstance.storages.nodeStorage, withSnapshotsSave = false))
  }

}
