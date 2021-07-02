package io.iohk.ethereum.mpt

import java.nio.ByteBuffer
import java.security.MessageDigest

import scala.util.Random

import org.bouncycastle.util.encoders.Hex
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.mpt.MerklePatriciaTrie._
import io.iohk.ethereum.utils.Logger

class MerklePatriciaTreeIntegrationSuite
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ObjectGenerators
    with Logger
    with PersistentStorage {

  val KeySize: Int = 32 + 1 /* Hash size + prefix */

  implicit val intByteArraySerializable: ByteArraySerializable[Int] = new ByteArraySerializable[Int] {
    override def toBytes(input: Int): Array[Byte] = {
      val b: ByteBuffer = ByteBuffer.allocate(4)
      b.putInt(input)
      b.array
    }

    override def fromBytes(bytes: Array[Byte]): Int = ByteBuffer.wrap(bytes).getInt()
  }

  def md5(bytes: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("MD5").digest(bytes)

  test("EthereumJ compatibility - Insert of the first 40000 numbers") {
    withRocksDbNodeStorage { ns =>
      val EmptyTrie = MerklePatriciaTrie[Array[Byte], Array[Byte]](ns)
      val shuffledKeys = Random.shuffle(0 to 40000).map(intByteArraySerializable.toBytes)
      val trie = shuffledKeys.foldLeft(EmptyTrie) { case (recTrie, key) => recTrie.put(key, key) }
      assert(Hex.toHexString(trie.getRootHash) == "3f8b75707975e5c16588fa1ba3e69f8da39f4e7bf3ca28b029c7dcb589923463")
    }
  }

  test("EthereumJ compatibility - Insert of the first 20000 numbers hashed") {
    withRocksDbNodeStorage { ns =>
      val EmptyTrie = MerklePatriciaTrie[Array[Byte], Array[Byte]](ns)
      val shuffledKeys = Random.shuffle(0 to 20000).map(intByteArraySerializable.toBytes)
      val trie = shuffledKeys.foldLeft(EmptyTrie) { case (recTrie, key) => recTrie.put(md5(key), key) }

      // We insert keys that should have no effect so as to test that is the case (and for more code coverage)
      val trieAfterInsertNoEffect =
        shuffledKeys.take(20000 / 2).foldLeft(trie) { case (recTrie, key) => recTrie.put(md5(key), key) }
      assert(
        Hex.toHexString(
          trieAfterInsertNoEffect.getRootHash
        ) == "a522b23a640c5fdb726e3f9644863e8913fe86339909fe881957efa0c23cebaa"
      )
    }
  }

  test("EthereumJ compatibility - Insert of the first 20000 numbers hashed and then remove half of them") {
    withRocksDbNodeStorage { ns =>
      val EmptyTrie = MerklePatriciaTrie[Array[Byte], Array[Byte]](ns)
      val keys = (0 to 20000).map(intByteArraySerializable.toBytes)
      val trie = Random.shuffle(keys).foldLeft(EmptyTrie) { case (recTrie, key) => recTrie.put(md5(key), key) }

      // We delete have of the (key-value) pairs we had inserted
      val trieAfterDelete =
        Random.shuffle(keys.take(20000 / 2)).foldLeft(trie) { case (recTrie, key) => recTrie.remove(md5(key)) }

      // We delete keys with no effect so as to test that is the case (and for more code coverage)
      val trieAfterDeleteNoEffect =
        keys.take(20000 / 2).foldLeft(trieAfterDelete) { case (recTrie, key) => recTrie.remove(md5(key)) }
      assert(
        Hex.toHexString(
          trieAfterDeleteNoEffect.getRootHash
        ) == "a693b82dcc5a9e581e9bf9aa7af3aed31fe3eb61f97fd733ce44c9f9df2d7f45"
      )
    }
  }

  test("EthereumJ compatibility - Insert of the first 20000 numbers hashed (with some sliced)") {
    withRocksDbNodeStorage { ns =>
      val EmptyTrie = MerklePatriciaTrie[Array[Byte], Array[Byte]](ns)
      val keys = (0 to 20000).map(intByteArraySerializable.toBytes)

      // We slice some of the keys so that me test more code coverage (if not we only test keys with the same length)
      val slicedKeys = keys.zipWithIndex.map { case (key, index) =>
        val hashedKey = md5(key)
        if (index % 2 == 0) hashedKey.take(hashedKey.length / 2) else hashedKey
      }
      val keyValuePairs = slicedKeys.zip(keys)

      val trie =
        Random.shuffle(keyValuePairs).foldLeft(EmptyTrie) { case (recTrie, (key, value)) => recTrie.put(key, value) }
      assert(Hex.toHexString(trie.getRootHash) == "46cde8656f3be6ce93ba9dcb1017548f44c65d1ea659ac827fac8c9ac77cf6b3")
    }
  }

  test(
    "EthereumJ compatibility - Insert of the first 20000 numbers hashed (with some sliced) and then remove half of them"
  ) {
    withRocksDbNodeStorage { ns =>
      val EmptyTrie = MerklePatriciaTrie[Array[Byte], Array[Byte]](ns)
      val keys = (0 to 20000).map(intByteArraySerializable.toBytes)

      // We slice some of the keys so that me test more code coverage (if not we only test keys with the same length)
      val slicedKeys = keys.zipWithIndex.map { case (key, index) =>
        val hashedKey = md5(key)
        if (index % 2 == 0) hashedKey.take(hashedKey.length / 2) else hashedKey
      }
      val keyValuePairs = slicedKeys.zip(keys)

      val start: Long = System.currentTimeMillis
      val trie =
        Random.shuffle(keyValuePairs).foldLeft(EmptyTrie) { case (recTrie, (key, value)) => recTrie.put(key, value) }

      assert(Hex.toHexString(trie.getRootHash) == "46cde8656f3be6ce93ba9dcb1017548f44c65d1ea659ac827fac8c9ac77cf6b3")

      // We delete have of the (key-value) pairs we had inserted
      val trieAfterDelete =
        Random.shuffle(keyValuePairs.take(20000 / 2)).foldLeft(trie) { case (recTrie, (key, _)) => recTrie.remove(key) }

      assert(
        Hex.toHexString(
          trieAfterDelete.getRootHash
        ) == "ae7b65dddd3ac0428082160cf3ceff0276cf6e6deaa23b42c4c156b50a459822"
      )
      log.debug("Time taken(ms): " + (System.currentTimeMillis - start))
    }
  }
}
