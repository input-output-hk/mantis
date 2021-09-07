package io.iohk.ethereum.consensus.validators
package std

import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.mpt.ByteArraySerializable
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.decode
import io.iohk.ethereum.rlp.encode

object MptListValidator {

  lazy val intByteArraySerializable: ByteArraySerializable[Int] = new ByteArraySerializable[Int] {
    override def fromBytes(bytes: Array[Byte]): Int = decode[Int](bytes)
    override def toBytes(input: Int): Array[Byte] = encode(input)
  }

  def rootHash[K](elements: Seq[K], vSerializable: ByteArraySerializable[K]): Array[Byte] = {
    val stateStorage = StateStorage.getReadOnlyStorage(EphemDataSource())
    val trie = MerklePatriciaTrie[Int, K](
      source = stateStorage
    )(intByteArraySerializable, vSerializable)
    elements.zipWithIndex.foldLeft(trie)((trie, r) => trie.put(r._2, r._1)).getRootHash
  }

  /** This function validates if a lists matches a Mpt Hash. To do so it inserts into an ephemeral MPT
    * (itemIndex, item) tuples and validates the resulting hash
    *
    * @param hash Hash to expect
    * @param toValidate Items to validate and should match the hash
    * @param vSerializable [[io.iohk.ethereum.mpt.ByteArraySerializable]] to encode Items
    * @tparam K Type of the items cointained within the Sequence
    * @return true if hash matches trie hash, false otherwise
    */
  def isValid[K](hash: Array[Byte], toValidate: Seq[K], vSerializable: ByteArraySerializable[K]): Boolean =
    hash.sameElements(rootHash(toValidate, vSerializable))
}
