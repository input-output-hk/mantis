package io.iohk.ethereum.mpt

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.{ArchiveNodeStorage, NodeStorage}
import io.iohk.ethereum.mpt.MerklePatriciaTrie.{ProofSketch, defaultByteArraySerializable}
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.spongycastle.util.encoders.Hex

import scala.annotation.tailrec

object Helpers {
  implicit class String2Hex(val s: String) extends AnyVal {
    def toHexBytes: Array[Byte] = Hex.decode(s)
  }
}

class MerklePatriciaTrieProofSuite extends FunSuite with PropertyChecks with ObjectGenerators {

  import Helpers._

  val Key_000001 = "000001"
  val Key_000002 = "000002"
  val Key_000003 = "000003"
  val Key_10F000 = "10F000"
  val Key_101F00 = "101F00"
  val Key_1012F0 = "1012F0"
  val Key_20000F = "20000F"

  val Val_000001 = "one__"
  val Val_000002 = "two__"
  val Val_000003 = "three"
  val Val_10F000 = "do___"
  val Val_101F00 = "dog__"
  val Val_1012F0 = "doge_"
  val Val_20000F = "horse"

  val data = Map(
    Key_000001 → Val_000001,
    Key_000002 → Val_000002,
    Key_000003 → Val_000003,
    Key_10F000 → Val_10F000,
    Key_101F00 → Val_101F00,
    Key_1012F0 → Val_1012F0,
    Key_20000F → Val_20000F
  )

  type MPTKey = Array[Byte]
  type MPTValue = Array[Byte]
  type MPT = MerklePatriciaTrie[MPTKey, MPTValue]

  @tailrec final def addAll(mpt: MPT, items: Iterator[(String, String)]): MPT =
    if(items.hasNext) {
      val (key, value) = items.next()
      val decodedKey = Hex.decode(key)

      val newMpt = mpt.put(decodedKey, value.getBytes("UTF-8"))
      addAll(newMpt, items)
    }
    else {
      mpt
    }

  val db = new ArchiveNodeStorage(new NodeStorage(EphemDataSource()))
  val emptyMpt: MPT = MerklePatriciaTrie[MPTKey, MPTValue](db)
  val mpt: MPT = addAll(emptyMpt, data.iterator)
  val rootHash: Array[Byte] = mpt.getRootHash

  def mptFind(key: String): Option[String] = mpt.get(key.toHexBytes).map(s ⇒ new String(s, "UTF-8"))
  def mptGet(key: String): String = mptFind(key).get
  def mptProve(key: String): ProofSketch = mpt.prove(key.toHexBytes).getOrElse(ProofSketch(Nil))
  def mptVerify(proof: ProofSketch): Boolean = mpt.verify(proof)

  test("Proof that key exists") {
    // All keys in `data` are hand-crafted.
    // This is the initial key used in order to implement/debug the prove() and verify() methods in the MPT.
    val key = Key_1012F0

    for {
      // TODO: fully randomize the keys
      key ← data.keys
    } {
      val value = mptGet(key)
      assert(value == data(key))

      val proof = mptProve(key)
      val proofVerified = mptVerify(proof)

      assert(proofVerified)
    }
  }
}
