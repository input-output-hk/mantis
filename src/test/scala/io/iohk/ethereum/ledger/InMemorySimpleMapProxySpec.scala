package io.iohk.ethereum.ledger

import java.nio.ByteBuffer

import io.iohk.ethereum.common.SimpleMap
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.NodeStorage
import io.iohk.ethereum.mpt.{ByteArraySerializable, MerklePatriciaTrie}
import io.iohk.ethereum.ledger.MPTWrappedMap._
import org.scalatest.{FlatSpec, Matchers}

class InMemorySimpleMapProxySpec extends FlatSpec with Matchers {

  "InMemoryTrieProxy" should "not write inserts until commit" in new TestSetup {
    val updatedProxy = InMemorySimpleMapProxy.wrap(mpt).put(1, 1).put(2, 2)

    assertContains(updatedProxy, 1, 1)
    assertContains(updatedProxy, 2, 2)

    assertNotContainsKey(mpt, 1)
    assertNotContainsKey(mpt, 2)

    val commitedProxy: InMemorySimpleMapProxy[Int, Int, MerklePatriciaTrie[Int, Int]] = updatedProxy.persist()

    assertContains(commitedProxy.inner, 1, 1)
    assertContains(commitedProxy.inner, 2, 2)
  }

  "InMemoryTrieProxy" should "not perform removals until commit" in new TestSetup {
    val preloadedMpt = mpt.put(1, 1)
    val proxy = InMemorySimpleMapProxy.wrap(preloadedMpt)

    assertContains(preloadedMpt, 1, 1)
    assertContains(proxy, 1, 1)

    val updatedProxy = proxy.remove(1)
    assertNotContainsKey(updatedProxy, 1)
    assertContains(updatedProxy.inner, 1, 1)

    val commitedProxy = updatedProxy.persist
    assertNotContainsKey(commitedProxy, 1)
    assertNotContainsKey(commitedProxy.inner, 1)
  }

  "InMemoryTrieProxy" should "not write updates until commit" in new TestSetup {
    val preloadedMpt = mpt.put(1, 1)
    val proxy = InMemorySimpleMapProxy.wrap(preloadedMpt)

    assertContains(preloadedMpt, 1, 1)
    assertContains(proxy, 1, 1)
    assertNotContains(preloadedMpt, 1, 2)
    assertNotContains(proxy, 1, 2)

    val updatedProxy = proxy.put(1, 2)
    assertContains(updatedProxy, 1, 2)
    assertNotContains(updatedProxy.inner, 1, 2)

    val commitedProxy = updatedProxy.persist
    assertContains(commitedProxy, 1, 2)
    assertContains(commitedProxy.inner, 1, 2)
  }

  "InMemoryTrieProxy" should "handle sequential operations" in new TestSetup {
    val updatedProxy = InMemorySimpleMapProxy.wrap(mpt).put(1, 1).remove(1).put(2, 2).put(2, 3)
    assertNotContainsKey(updatedProxy, 1)
    assertContains(updatedProxy, 2, 3)
  }

  "InMemoryTrieProxy" should "handle batch operations" in new TestSetup {
    val updatedProxy = InMemorySimpleMapProxy.wrap(mpt).update(Seq(1), Seq((2, 2), (2, 3)))
    assertNotContainsKey(updatedProxy, 1)
    assertContains(updatedProxy, 2, 3)
  }

  "InMemoryTrieProxy" should "not fail when deleting an inexistent value" in new TestSetup {
    assertNotContainsKey(InMemorySimpleMapProxy.wrap(mpt).remove(1), 1)
  }

  def assertContains(trie: SimpleMap[Int, Int], key: Int, value: Int): Unit =
    assert(trie.get(key).isDefined && trie.get(key).get == value)

  def assertNotContains(trie: SimpleMap[Int, Int], key: Int, value: Int): Unit =
    assert(trie.get(key).isDefined && trie.get(key).get != value)

  def assertNotContainsKey(trie: SimpleMap[Int, Int], key: Int): Unit = assert(trie.get(key).isEmpty)

  trait TestSetup {
    implicit val intByteArraySerializable = new ByteArraySerializable[Int] {
      override def toBytes(input: Int): Array[Byte] = {
        val b: ByteBuffer = ByteBuffer.allocate(4)
        b.putInt(input)
        b.array
      }

      override def fromBytes(bytes: Array[Byte]): Int = ByteBuffer.wrap(bytes).getInt()
    }


    val nodeStorage = new NodeStorage(EphemDataSource())
    val mpt: MerklePatriciaTrie[Int, Int] = MerklePatriciaTrie[Int, Int](nodeStorage, kec256(_: Array[Byte]))
  }

}
