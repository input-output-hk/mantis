/*package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.mpt.{MerklePatriciaTrie}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class ReferenceCountNodeStorageSpec extends FlatSpec with Matchers {

  val hashFn = kec256(_: Array[Byte])

  "ReferenceCountNodeStorageSpec" should "allow to prune unused entries in order" in new TestSetup {

    val storage: ReferenceCountNodeStorage = new ReferenceCountNodeStorage(nodeStorage, 1, Some(1))
    val EmptyTrie = MerklePatriciaTrie[ByteString, ByteString](storage, hashFn)

    val key1 = ByteString(Hex.decode("ba"))
    val key2 = ByteString(Hex.decode("aa"))
    val key3 = ByteString(Hex.decode(""))
    val value = ByteString(
      Hex.decode("012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789")
    )
    val trie = EmptyTrie.put(key1, value).put(key2, value)

    trie.get(key1).getOrElse(ByteString.empty) shouldEqual value
    trie.get(key2).getOrElse(ByteString.empty) shouldEqual value
    dataSource.storage.size shouldEqual 4 // 2 nodes (leaf are the same) + previous root node + 1 with PruneCandidates

    val storage2 = new ReferenceCountNodeStorage(new NodeStorage(dataSource), 1, Some(2))
    val trie2 = MerklePatriciaTrie[ByteString, ByteString](trie.getRootHash, storage2 , hashFn).put(key3, value)

    trie2.get(key1).getOrElse(ByteString.empty) shouldEqual value
    trie2.get(key2).getOrElse(ByteString.empty) shouldEqual value
    trie2.get(key3).getOrElse(ByteString.empty) shouldEqual value
    dataSource.storage.size shouldEqual 6 // 3 nodes + 1 previous root + 2 with PruneCandidates

    new ReferenceCountNodeStorage(nodeStorage, 1, None).prune(0, 2) //FIXME

    dataSource.storage.size shouldEqual 4 // previous root and 1 PruneCandidate is now gone

    val storage3 = new ReferenceCountNodeStorage(new NodeStorage(dataSource), 1, Some(3))
    val trie3 = MerklePatriciaTrie[ByteString, ByteString](trie2.getRootHash, storage3 , hashFn)

    trie3.get(key1).getOrElse(ByteString.empty) shouldEqual value
    trie3.get(key2).getOrElse(ByteString.empty) shouldEqual value
    trie3.get(key3).getOrElse(ByteString.empty) shouldEqual value
  }
}

trait TestSetup {

  val dataSource = EphemDataSource()
  val nodeStorage = new NodeStorage(dataSource)

}*/
