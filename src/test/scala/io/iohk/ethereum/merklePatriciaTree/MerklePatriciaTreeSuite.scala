package io.iohk.ethereum.merklePatriciaTree

import java.io.File

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.crypto.Keccak
import io.iohk.ethereum.merklePatriciaTree.MerklePatriciaTree.defaultByteArraySerializable
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => encodeRLP, decode => decodeRLP}
import io.iohk.iodb.LSMStore
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.spongycastle.util.encoders.Hex

import scala.util.{Random, Try}

class MerklePatriciaTreeSuite extends FunSuite
  with PropertyChecks
  with ObjectGenerators {
  val hashFn = Keccak.hash256 _

  val EmptyTrie = MerklePatriciaTree[Array[Byte], Array[Byte]](HashMapDataSource(), hashFn)

  implicit val intByteArraySerializable = new ByteArraySerializable[Int] {
    override def toBytes(input: Int): Array[Byte] = encodeRLP(input)

    override def fromBytes(bytes: Array[Byte]): Int = decodeRLP[Int](bytes)
  }

  test("PatriciaTrie insert") {
    forAll(keyValueListGen()) { keyValueList: Seq[(Int, Int)] =>
      val trie = keyValueList.foldLeft(MerklePatriciaTree[Int, Int](HashMapDataSource(), hashFn)) {
        case (recTrie, (key, value)) => recTrie.put(key, value)
      }
      keyValueList.foreach { case (key, value) =>
        val obtained = trie.get(key)
        assert(obtained.isDefined)
        assert(obtained.get == value)
      }
    }
  }

  test("PatriciaTrie delete") {
    forAll(Gen.nonEmptyListOf(Arbitrary.arbitrary[Int])) { keyList: List[Int] =>
      val keyValueList = keyList.distinct.zipWithIndex
      val trieAfterInsert = keyValueList.foldLeft(MerklePatriciaTree[Int, Int](HashMapDataSource(), hashFn)) {
        case (recTrie, (key, value)) => recTrie.put(key, value)
      }
      val (keyValueToDelete, keyValueLeft) = Random.shuffle(keyValueList).splitAt(Gen.choose(0, keyValueList.size).sample.get)
      val trieAfterDelete = keyValueToDelete.foldLeft(trieAfterInsert) {
        case (recTrie, (key, value)) => recTrie.remove(key)
      }

      keyValueLeft.foreach { case (key, value) =>
        val obtained = trieAfterDelete.get(key)
        assert(obtained.isDefined)
        assert(obtained.get == value)
      }
      keyValueToDelete.foreach { case (key, value) =>
        val obtained = trieAfterDelete.get(key)
        assert(obtained.isEmpty)
      }

      val trieWithKeyValueLeft = keyValueLeft.foldLeft(MerklePatriciaTree[Int, Int](HashMapDataSource(), hashFn)) {
        case (recTrie, (key, value)) => recTrie.put(key, value)
      }
      assert(
        (trieAfterDelete.rootHash.isEmpty && keyValueLeft.isEmpty) ||
          (trieAfterDelete.rootHash.get sameElements trieWithKeyValueLeft.rootHash.get)
      )
    }
  }

  test("Remove key from an empty tree") {
    val emptyTrie = MerklePatriciaTree[Int, Int](HashMapDataSource(), hashFn)
    val afterDeleteTrie = emptyTrie.remove(1)
    assert(afterDeleteTrie.rootHash.isEmpty)
  }

  test("Remove a key that does not exist") {
    val emptyTrie = MerklePatriciaTree[Int, Int](HashMapDataSource(), hashFn)
    val trieWithOneElement = emptyTrie.put(1, 5)
    val obtained = trieWithOneElement.get(1)
    assert(obtained.isDefined)
    assert(obtained.get == 5)
    val trieAfterDelete = trieWithOneElement.remove(2)
    val obtainedAfterDelete = trieAfterDelete.get(1)
    assert(obtainedAfterDelete.get == 5)
  }

  test("Trie insert should have the same root independently on the order its pairs are inserted") {
    forAll(keyValueListGen()) { keyValueList: Seq[(Int, Int)] =>
      val trieAfterInsert = keyValueList.foldLeft(MerklePatriciaTree[Int, Int](HashMapDataSource(), hashFn)) {
        case (recTrie, (key, value)) => recTrie.put(key, value)
      }
      val keyValueListShuffle = Random.shuffle(keyValueList)

      val trieAfterInsertShuffle = keyValueListShuffle.foldLeft(MerklePatriciaTree[Int, Int](HashMapDataSource(), hashFn)) {
        case (recTrie, (key, value)) => recTrie.put(key, value)
      }

      assert(trieAfterInsert.rootHash.get sameElements trieAfterInsertShuffle.rootHash.get)
    }
  }

  test("Insert only one (key, value) pair to a trie and then deleted") {
    val emptyTrie = MerklePatriciaTree[Int, Int](HashMapDataSource(), hashFn)
    val trieWithOneElement = emptyTrie.put(1, 5)
    val obtained = trieWithOneElement.get(1)
    assert(obtained.isDefined)
    assert(obtained.get == 5)
    val trieAfterDelete = trieWithOneElement.remove(1)
    val obtainedAfterDelete = trieAfterDelete.get(1)
    assert(obtainedAfterDelete.isEmpty)
  }

  test("Insert two (key, value) pairs with the first hex not in common") {
    val emptyTrie = EmptyTrie
    val key1: Array[Byte] = Hex.decode("0001")
    val key2: Array[Byte] = Hex.decode("f001")
    val val1: Array[Byte] = Hex.decode("0101")
    val trieWithOneElement = emptyTrie.put(key1, val1)
    val trieWithTwoElements = trieWithOneElement.put(key2, val1)
    val obtained1 = trieWithTwoElements.get(key1)
    assert(obtained1.isDefined)
    assert(obtained1.get sameElements val1)
    val obtained2 = trieWithTwoElements.get(key2)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val1)
  }

  test("Insert two (key, value) pairs with one hex or more in common") {
    val emptyTrie = EmptyTrie
    val key1: Array[Byte] = Hex.decode("00000001")
    val key2: Array[Byte] = Hex.decode("0000f001")
    val val1: Array[Byte] = Hex.decode("0101")
    val trieWithOneElement = emptyTrie.put(key1, val1)
    val trieWithTwoElements = trieWithOneElement.put(key2, val1)
    val obtained1 = trieWithTwoElements.get(key1)
    assert(obtained1.isDefined)
    assert(obtained1.get sameElements val1)
    val obtained2 = trieWithTwoElements.get(key2)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val1)
  }

  test("Insert two (key, value) pairs with the same key") {
    val emptyTrie = EmptyTrie
    val key1: Array[Byte] = Hex.decode("00000001")
    val val1: Array[Byte] = Hex.decode("0101")
    val val2: Array[Byte] = Hex.decode("010101")
    val trieWithOneElement = emptyTrie.put(key1, val1)
    val trieAfterSecondInsert = trieWithOneElement.put(key1, val2)
    val obtained2 = trieAfterSecondInsert.get(key1)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val2)
  }

  test("Insert 3 (key, value) pairs with different first hex") {
    val emptyTrie = EmptyTrie
    val key1: Array[Byte] = Hex.decode("10000001")
    val key2: Array[Byte] = Hex.decode("20000002")
    val key3: Array[Byte] = Hex.decode("30000003")
    val val1: Array[Byte] = Hex.decode("0101")
    val val2: Array[Byte] = Hex.decode("0102")
    val val3: Array[Byte] = Hex.decode("0103")
    val trieWithOneElement = emptyTrie.put(key1, val1)
    val trieWithTwoElement = trieWithOneElement.put(key2, val2)
    val trieWithThreeElement = trieWithTwoElement.put(key3, val3)
    val obtained1 = trieWithThreeElement.get(key1)
    assert(obtained1.isDefined)
    assert(obtained1.get sameElements val1)
    val obtained2 = trieWithThreeElement.get(key2)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val2)
    val obtained3 = trieWithThreeElement.get(key3)
    assert(obtained3.isDefined)
    assert(obtained3.get sameElements val3)
  }

  test("Multiple insertions") {
    val key1: Array[Byte] = Hex.decode("123456")
    val key2: Array[Byte] = Hex.decode("234567")
    val key3: Array[Byte] = Hex.decode("123467")
    val key4: Array[Byte] = Hex.decode("12346789")
    val key5: Array[Byte] = Hex.decode("0123")
    val val1: Array[Byte] = Hex.decode("01")
    val val2: Array[Byte] = Hex.decode("02")
    val val3: Array[Byte] = Hex.decode("03")
    val val4: Array[Byte] = Hex.decode("04")
    val val5: Array[Byte] = Hex.decode("05")
    val keys = List(key1, key2, key3, key4, key5)
    val vals = List(val1, val2, val3, val4, val5)
    val keysWithVal = keys.zip(vals)
    val trie = keysWithVal.foldLeft(EmptyTrie) { (recTrie, elem) => recTrie.put(elem._1, elem._2) }
    keysWithVal.foreach { t =>
      val obtained = trie.get(t._1)
      assert(obtained.isDefined)
      assert(obtained.get sameElements t._2)
    }
  }

  test("Insert 3 (key-value) pairs with a common prefix") {
    val key1: Array[Byte] = Hex.decode("1234")
    val key2: Array[Byte] = Hex.decode("1235")
    val key3: Array[Byte] = Hex.decode("1245")
    val val1: Array[Byte] = Hex.decode("947e70f9460402290a3e487dae01f610a1a8218fda")
    val keys = List(key1, key2, key3)
    val vals = List(val1, val1, val1)
    val keysWithVal = keys.zip(vals).take(2)
    val trie = keysWithVal.foldLeft(EmptyTrie) { (recTrie, elem) => recTrie.put(elem._1, elem._2) }
    trie.put(key3, val1)
    keysWithVal.foreach { t =>
      val obtained = trie.get(t._1)
      assert(obtained.isDefined)
      assert(obtained.get sameElements t._2)
    }
  }

  test("Insert 2 (key-value) pairs with one hex in common and then delete one of them") {
    val key1: Array[Byte] = Hex.decode("123456")
    val key2: Array[Byte] = Hex.decode("223456")
    val val1: Array[Byte] = Hex.decode("01")
    val val2: Array[Byte] = Hex.decode("02")
    val trieWithTwoElements = EmptyTrie.put(key1, val1).put(key2, val2)
    val trieAfterDelete = trieWithTwoElements.remove(key1)
    val obtained1 = trieAfterDelete.get(key1)
    assert(obtained1.isEmpty)
    val obtained2 = trieAfterDelete.get(key2)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val2)
  }

  test("Insert 2 (key-value) pairs with more than one hex in common and then delete one of them") {
    val key1: Array[Byte] = Hex.decode("123456")
    val key2: Array[Byte] = Hex.decode("124456")
    val val1: Array[Byte] = Hex.decode("01")
    val val2: Array[Byte] = Hex.decode("02")
    val trieWithTwoElements = EmptyTrie.put(key1, val1).put(key2, val2)
    val trieAfterDelete = trieWithTwoElements.remove(key1)
    val obtained1 = trieAfterDelete.get(key1)
    assert(obtained1.isEmpty)
    val obtained2 = trieAfterDelete.get(key2)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val2)
  }

  test("Insert 2 (key-value) pairs with more than one hex in common and then delete one of them, has the same result as only adding the pair left") {
    val key1: Array[Byte] = Hex.decode("123456")
    val key2: Array[Byte] = Hex.decode("124456")
    val val1: Array[Byte] = Hex.decode("01")
    val val2: Array[Byte] = Hex.decode("02")
    val trieWithTwoElements = EmptyTrie.put(key1, val1).put(key2, val2)
    val trieAfterDelete = trieWithTwoElements.remove(key1)
    val obtained1 = trieAfterDelete.get(key1)
    assert(obtained1.isEmpty)
    val obtained2 = trieAfterDelete.get(key2)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val2)
    assert(trieAfterDelete.rootHash.get sameElements EmptyTrie.put(key2, val2).rootHash.get)
  }

  test("Insert 2 (key-value) pairs with nothing in common and then delete one of them, has the same result as only adding the pair left") {
    val key1: Array[Byte] = Hex.decode("")
    val key2: Array[Byte] = Hex.decode("12")
    val val1: Array[Byte] = Hex.decode("01")
    val val2: Array[Byte] = Hex.decode("02")
    val trieWithTwoElements = EmptyTrie.put(key1, val1).put(key2, val2)
    val trieAfterDelete = trieWithTwoElements.remove(key1)
    val obtained1 = trieAfterDelete.get(key1)
    assert(obtained1.isEmpty)
    val obtained2 = trieAfterDelete.get(key2)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val2)
    assert(trieAfterDelete.rootHash.get sameElements EmptyTrie.put(key2, val2).rootHash.get)
  }

  test("Simple test with IODB as Source") {
    //create temporary dir
    val dir = File.createTempFile("iodb", "iodb")
    dir.delete()
    dir.mkdir()

    //open new store
    val dataSource = IodbDataSource(new LSMStore(dir = dir, keySize = 32))
    val emptyTrie = MerklePatriciaTree[Int, Int](dataSource, hashFn)
    val trieWithOneElement = emptyTrie.put(1, 5)
    val obtained = trieWithOneElement.get(1)
    assert(obtained.isDefined)
    assert(obtained.get == 5)
    val trieAfterDelete = trieWithOneElement.remove(1)
    val obtainedAfterDelete = trieAfterDelete.get(1)
    assert(obtainedAfterDelete.isEmpty)
  }

  test("Remove of a trie with an extension whose next is not on source") {
    val key1: Array[Byte] = Hex.decode("123500")
    val key2: Array[Byte] = Hex.decode("123600")
    val key3: Array[Byte] = Hex.decode("123700")
    val key4: Array[Byte] = Hex.decode("123500")
    val trie = EmptyTrie.put(key1, key1).put(key2, key2).put(key3, key3).put(key4, key4)
    val wrongSource = HashMapDataSource().update(trie.rootHash.get, trie.dataSource.get(trie.rootHash.get).get)
    val trieWithWrongSource = MerklePatriciaTree[Array[Byte], Array[Byte]](trie.rootHash, wrongSource, trie.hashFn)
    val trieAfterDelete = Try {
      trieWithWrongSource.remove(key1)
    }
    assert(trieAfterDelete.isFailure)
  }

  /* EthereumJ tests */
  test("testDeleteCompletellyDiferentItems") {
    val val1: String = "1000000000000000000000000000000000000000000000000000000000000000"
    val val2: String = "2000000000000000000000000000000000000000000000000000000000000000"
    val val3: String = "3000000000000000000000000000000000000000000000000000000000000000"
    val trieWithOneElement = EmptyTrie.put(Hex.decode(val1), Hex.decode(val1))
    val trieWithTwoElements = trieWithOneElement.put(Hex.decode(val2), Hex.decode(val2))
    val root1: String = Hex.toHexString(trieWithTwoElements.rootHash.get)
    val trieWithThreeElements = trieWithTwoElements.put(Hex.decode(val3), Hex.decode(val3))
    val trieAfterDelete = trieWithThreeElements.remove(Hex.decode(val3))
    val root1Obtained: String = Hex.toHexString(trieAfterDelete.rootHash.get)
    assert(root1 == root1Obtained)
  }

  test("storageHashCalc_1") {
    val key1: Array[Byte] = Hex.decode("0000000000000000000000000000000000000000000000000000000000000010")
    val key2: Array[Byte] = Hex.decode("0000000000000000000000000000000000000000000000000000000000000014")
    val key3: Array[Byte] = Hex.decode("0000000000000000000000000000000000000000000000000000000000000016")
    val key4: Array[Byte] = Hex.decode("0000000000000000000000000000000000000000000000000000000000000017")
    val val1: Array[Byte] = Hex.decode("947e70f9460402290a3e487dae01f610a1a8218fda")
    val val2: Array[Byte] = Hex.decode("40")
    val val3: Array[Byte] = Hex.decode("94412e0c4f0102f3f0ac63f0a125bce36ca75d4e0d")
    val val4: Array[Byte] = Hex.decode("01")
    val storage0 = EmptyTrie
    val storage1 = storage0.put(key1, val1)
    val storage2 = storage1.put(key2, val2)
    val storage3 = storage2.put(key3, val3)
    val storage4 = storage3.put(key4, val4)
    val hash: String = Hex.toHexString(storage4.rootHash.get)
    storage4.get(key1)
    assert("517eaccda568f3fa24915fed8add49d3b743b3764c0bc495b19a47c54dbc3d62" == hash)
  }

  test("EtheruemJ compatibility - BranchNode hash") {
    val key1: Array[Byte] = Hex.decode("11")
    val key2: Array[Byte] = Hex.decode("00")
    val storage = EmptyTrie.put(key1, key1).put(key2, key2)
    assert(Hex.toHexString(storage.rootHash.get) == "47ec8b5bd21ca0cbd0bd003f2e791778ccae44e7e21476da60fe7a1e0e6ac838")
  }

  test("EthereumJ compatibility - ExtensionNode hash") {
    val key1: Array[Byte] = Hex.decode("1111")
    val key2: Array[Byte] = Hex.decode("1100")
    val storage = EmptyTrie.put(key1, key1).put(key2, key2)
    assert(Hex.toHexString(storage.rootHash.get) == "ad9523c662fc9654b79620685a67b1b521517c73d7915a8e18ef929e1c1e6807")
  }

  test("EthereumJ compatibility - BranchNode with extension hash") {
    val key1: Array[Byte] = Hex.decode("0111")
    val key2: Array[Byte] = Hex.decode("0211")
    val key3: Array[Byte] = Hex.decode("1100")
    val storage = EmptyTrie.put(key1, key1).put(key2, key2).put(key3, key3)
    assert(Hex.toHexString(storage.rootHash.get) == "a3d0686205c7ed10a85c3bce4118d5d559bcda47ca39e4dd4f09719958a179f1")
  }
}

case class HashMapDataSource(storage: Map[ByteString, Array[Byte]]) extends DataSource {

  override def update(key: Array[Byte], value: Array[Byte]): DataSource = HashMapDataSource(storage + (ByteString(key) -> value))

  override def get(key: Array[Byte]): Option[Array[Byte]] = storage.get(ByteString(key))

  override def remove(key: Array[Byte]): DataSource = HashMapDataSource(storage - ByteString(key))

  override def update(keys: Seq[(Array[Byte], Array[Byte])]): DataSource = keys.foldLeft[DataSource](this) {
    (curr, toPut) => curr.update(toPut._1, toPut._2)
  }

  override def update(toRemove: Seq[Key], toUpdate: Seq[(Key, Value)]): DataSource = {
    val afterRemoval = toRemove.foldLeft(storage)((storage, key) => storage - ByteString(key))
    val afterUpdate = toUpdate.foldLeft(afterRemoval)((storage, toUpdate) => storage + (ByteString(toUpdate._1) -> toUpdate._2))
    HashMapDataSource(afterUpdate)
  }
}

object HashMapDataSource {
  def apply(): HashMapDataSource = HashMapDataSource(Map())
}
