package io.iohk.ethereum.validators

import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.NodeStorage
import io.iohk.ethereum.mpt.{ByteArraySerializable, MerklePatriciaTrie}
import io.iohk.ethereum.rlp.{encode, decode}

object MptListValidator {

  lazy val intByteArraySerializable = new ByteArraySerializable[Int] {
    override def fromBytes(bytes: Array[Byte]): Int = decode[Int](bytes)
    override def toBytes(input: Int): Array[Byte] = encode(input)
  }

  /**
    * This function validates if a lists matches a Mpt Hash. To do so it inserts into an ephemeral MPT
    * (itemIndex, item) tuples and validates the resulting hash
    *
    * @param hash Hash to expect
    * @param toValidate Items to validate and should match the hash
    * @param vSerializable [[io.iohk.ethereum.mpt.ByteArraySerializable]] to encode Items
    * @tparam K Type of the items cointained within the Sequence
    * @return true if hash matches trie hash, false otherwise
    */
  def isValid[K](hash: Array[Byte], toValidate: Seq[K], vSerializable: ByteArraySerializable[K]): Boolean = {

    val trie = MerklePatriciaTrie[Int, K](
      source = new NodeStorage(EphemDataSource()),
      hashFn = (input: Array[Byte]) => kec256(input)
    )(intByteArraySerializable, vSerializable)
    val trieRoot = toValidate.zipWithIndex.foldLeft(trie) { (trie, r) => trie.put(r._2, r._1) }.getRootHash
    hash sameElements trieRoot
  }
}
