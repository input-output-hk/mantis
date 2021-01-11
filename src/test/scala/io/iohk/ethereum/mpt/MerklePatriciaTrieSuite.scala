package io.iohk.ethereum.mpt

import java.nio.ByteBuffer

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.{DataSourceUpdate, EphemDataSource}
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.db.storage.pruning.BasicPruning
import io.iohk.ethereum.mpt.MerklePatriciaTrie.{MPTException, defaultByteArraySerializable}
import org.scalacheck.{Arbitrary, Gen}
import org.bouncycastle.util.encoders.Hex
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.{Random, Try}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ArraySeq

class MerklePatriciaTrieSuite extends AnyFunSuite with ScalaCheckPropertyChecks with ObjectGenerators {

  val dataSource: EphemDataSource = EphemDataSource()
  val (stateStorage, emptyNodeStorage, _) = StateStorage.createTestStateStorage(dataSource)
  val emptyEphemNodeStorage = stateStorage.getBackingStorage(0)
  val emptyMpt = MerklePatriciaTrie[Array[Byte], Array[Byte]](emptyEphemNodeStorage)

  implicit val intByteArraySerializable: ByteArraySerializable[Int] = new ByteArraySerializable[Int] {
    override def toBytes(input: Int): Array[Byte] = {
      val b: ByteBuffer = ByteBuffer.allocate(4)
      b.putInt(input)
      b.array
    }

    override def fromBytes(bytes: Array[Byte]): Int = ByteBuffer.wrap(bytes).getInt()
  }

  test("PatriciaTrie gets inserted key-value pairs") {
    forAll(keyValueListGen()) { keyValueList: Seq[(Int, Int)] =>
      val trie = addEveryKeyValuePair(keyValueList)
      assertCanGetEveryKeyValue(trie, keyValueList)
    }
  }

  test("PatriciaTrie collapsing trie") {
    forAll(keyValueListGen()) { keyValueList: Seq[(Int, Int)] =>
      // given
      val trie = addEveryKeyValuePair(keyValueList)
      val unfoldedTrie = MptTraversals.parseTrieIntoMemory(HashNode(trie.getRootHash), emptyEphemNodeStorage)

      // when
      val collapsed = MptTraversals.collapseTrie(unfoldedTrie)

      // then rootHash
      assert(collapsed._1.hashNode sameElements trie.getRootHash)

      // then can recreate MPT
      val newTrie = MerklePatriciaTrie[Int, Int](collapsed._1.hashNode, emptyEphemNodeStorage)
      assertCanGetEveryKeyValue(newTrie, keyValueList)
    }
  }

  test("PatriciaTrie encoding decoding") {
    forAll(keyValueListGen()) { keyValueList: Seq[(Int, Int)] =>
      val trie = addEveryKeyValuePair(keyValueList)
      val unfoldedTrieNode = MptTraversals.parseTrieIntoMemory(HashNode(trie.getRootHash), emptyEphemNodeStorage)

      // when
      val encoded = MptTraversals.encodeNode(unfoldedTrieNode)
      val decoded = MptTraversals.decodeNode(encoded)

      assert(unfoldedTrieNode.hash sameElements decoded.hash)
    }
  }

  test("PatriciaTrie delete") {
    forAll(Gen.nonEmptyListOf(Arbitrary.arbitrary[Int])) { keyList: List[Int] =>
      val keyValueList = keyList.distinct.zipWithIndex
      val trieAfterInsert = addEveryKeyValuePair(keyValueList)
      val (keyValueToDelete, keyValueLeft) = keyValueList.splitAt(Gen.choose(0, keyValueList.size).sample.get)
      val trieAfterDelete: MerklePatriciaTrie[Int, Int] = keyValueToDelete.foldLeft(trieAfterInsert) {
        case (recTrie, (key, _)) =>
          recTrie.remove(key)
      }

      assertCanGetEveryKeyValue(trieAfterDelete, keyValueLeft)
      assertNotHave(trieAfterDelete, keyValueToDelete)

      val trieWithKeyValueLeft = addEveryKeyValuePair(keyValueLeft)
      assert(trieAfterDelete.getRootHash sameElements trieWithKeyValueLeft.getRootHash)
    }
  }

  test("Trie insert should have the same root independently on the order its pairs are inserted") {
    forAll(keyValueListGen()) { keyValueList: Seq[(Int, Int)] =>
      val trie = addEveryKeyValuePair(keyValueList)

      val keyValueListShuffle = Random.shuffle(keyValueList)
      val trieShuffled = addEveryKeyValuePair(keyValueListShuffle)

      assert(trie.getRootHash sameElements trieShuffled.getRootHash)
    }
  }

  /* MerklePatriciaTree API tests for particular cases */
  test("Remove key from an empty tree") {
    val emptyTrie = MerklePatriciaTrie[Int, Int](emptyEphemNodeStorage)
    val afterDeleteTrie = emptyTrie.remove(1)
    assert(afterDeleteTrie.getRootHash sameElements emptyTrie.getRootHash)
  }

  test("Remove a key that does not exist") {
    val emptyTrie = MerklePatriciaTrie[Int, Int](emptyEphemNodeStorage)
    val trieWithOneElement = emptyTrie.put(1, 5)
    val obtained = trieWithOneElement.get(1)
    assert(obtained.isDefined)
    assert(obtained.get == 5)
    val trieAfterDelete = trieWithOneElement.remove(2)
    val obtainedAfterDelete = trieAfterDelete.get(1)
    assert(obtainedAfterDelete.get == 5)
  }

  test("Insert only one (key, value) pair to a trie and then deleted") {
    val emptyTrie = MerklePatriciaTrie[Int, Int](emptyEphemNodeStorage)
    val trieWithOneElement = emptyTrie.put(1, 5)
    val obtained = trieWithOneElement.get(1)
    assert(obtained.isDefined)
    assert(obtained.get == 5)
    val trieAfterDelete = trieWithOneElement.remove(1)
    val obtainedAfterDelete = trieAfterDelete.get(1)
    assert(obtainedAfterDelete.isEmpty)
  }

  test("Insert two (key, value) pairs with the first hex not in common") {
    val key1: Array[Byte] = Hex.decode("0001")
    val key2: Array[Byte] = Hex.decode("f001")
    val val1: Array[Byte] = Hex.decode("0101")
    val trieWithOneElement = emptyMpt.put(key1, val1)
    val trieWithTwoElements = trieWithOneElement.put(key2, val1)
    val obtained1 = trieWithTwoElements.get(key1)
    assert(obtained1.isDefined)
    assert(obtained1.get sameElements val1)
    val obtained2 = trieWithTwoElements.get(key2)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val1)
  }

  test("Insert two (key, value) pairs with one hex or more in common") {
    val key1: Array[Byte] = Hex.decode("00000001")
    val key2: Array[Byte] = Hex.decode("0000f001")
    val val1: Array[Byte] = Hex.decode("0101")
    val trieWithOneElement = emptyMpt.put(key1, val1)
    val trieWithTwoElements = trieWithOneElement.put(key2, val1)
    val obtained1 = trieWithTwoElements.get(key1)
    assert(obtained1.isDefined)
    assert(obtained1.get sameElements val1)
    val obtained2 = trieWithTwoElements.get(key2)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val1)
  }

  test("Insert two (key, value) pairs with the same key") {
    val key1: Array[Byte] = Hex.decode("00000001")
    val val1: Array[Byte] = Hex.decode("0101")
    val val2: Array[Byte] = Hex.decode("010101")
    val trieWithOneElement = emptyMpt.put(key1, val1)
    val trieAfterSecondInsert = trieWithOneElement.put(key1, val2)
    val obtained2 = trieAfterSecondInsert.get(key1)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val2)
  }

  test("Insert 3 (key, value) pairs with different first hex") {
    val key1: Array[Byte] = Hex.decode("10000001")
    val key2: Array[Byte] = Hex.decode("20000002")
    val key3: Array[Byte] = Hex.decode("30000003")
    val val1: Array[Byte] = Hex.decode("0101")
    val val2: Array[Byte] = Hex.decode("0102")
    val val3: Array[Byte] = Hex.decode("0103")
    val trieWithOneElement = emptyMpt.put(key1, val1)
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
    val keys = List("123456", "234567", "123467", "12346789", "0123").map(Hex.decode)
    val vals = List("01", "02", "03", "04", "05").map(Hex.decode)
    val keysWithVal = keys.zip(vals)
    val trie = keysWithVal.foldLeft(emptyMpt) { (recTrie, elem) => recTrie.put(elem._1, elem._2) }

    assertCanGetEveryKeyValues(trie, keysWithVal)
  }

  test("Multiple insertions and the removals") {
    val keys =
      List("123456", "234567", "123467", "12346789", "0123", "1235", "234568", "125678", "124567", "23456789").map(
        Hex.decode
      )
    val vals = List("01", "02", "03", "04", "05", "06", "07", "08", "09", "10").map(Hex.decode)
    val keysWithVal = keys.zip(vals)
    val trie = keysWithVal.foldLeft(emptyMpt) { (recTrie, elem) => recTrie.put(elem._1, elem._2) }

    val (keysWithValToDelete, keysWithValLeft) = keysWithVal.splitAt(3)
    val trieAfterDelete = keysWithValToDelete.foldLeft(trie) { (recTrie, elem) => recTrie.remove(elem._1) }
    assertCanGetEveryKeyValues(trieAfterDelete, keysWithValLeft)
    assertNotHave(trieAfterDelete, keysWithValToDelete)
    trieAfterDelete.get(Hex.decode("01"))
  }

  test("Insert 3 (key-value) pairs with a common prefix") {
    val key1: Array[Byte] = Hex.decode("1234")
    val key2: Array[Byte] = Hex.decode("1235")
    val key3: Array[Byte] = Hex.decode("1245")
    val val1: Array[Byte] = Hex.decode("947e70f9460402290a3e487dae01f610a1a8218fda")
    val keys = List(key1, key2, key3)
    val vals = List(val1, val1, val1)
    val keysWithVal = keys.zip(vals)
    val trie = keysWithVal.foldLeft(emptyMpt) { (recTrie, elem) => recTrie.put(elem._1, elem._2) }

    assertCanGetEveryKeyValues(trie, keysWithVal)
  }

  test("Insert 2 (key-value) pairs with one hex in common and then delete one of them") {
    val key1: Array[Byte] = Hex.decode("123456")
    val key2: Array[Byte] = Hex.decode("223456")
    val val1: Array[Byte] = Hex.decode("01")
    val val2: Array[Byte] = Hex.decode("02")
    val trieWithTwoElements = emptyMpt.put(key1, val1).put(key2, val2)
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
    val trieWithTwoElements = emptyMpt.put(key1, val1).put(key2, val2)
    val trieAfterDelete = trieWithTwoElements.remove(key1)
    val obtained1 = trieAfterDelete.get(key1)
    assert(obtained1.isEmpty)
    val obtained2 = trieAfterDelete.get(key2)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val2)
  }

  test(
    "Insert 2 (key-value) pairs with more than one hex in common and then delete one of them, has the same result as only adding the pair left"
  ) {
    val key1: Array[Byte] = Hex.decode("123456")
    val key2: Array[Byte] = Hex.decode("124456")
    val val1: Array[Byte] = Hex.decode("01")
    val val2: Array[Byte] = Hex.decode("02")
    val trieWithTwoElements = emptyMpt.put(key1, val1).put(key2, val2)
    val trieAfterDelete = trieWithTwoElements.remove(key1)
    val obtained1 = trieAfterDelete.get(key1)
    assert(obtained1.isEmpty)
    val obtained2 = trieAfterDelete.get(key2)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val2)
    assert(trieAfterDelete.getRootHash sameElements emptyMpt.put(key2, val2).getRootHash)
  }

  test(
    "Insert 2 (key-value) pairs with nothing in common and then delete one of them, has the same result as only adding the pair left"
  ) {
    val key1: Array[Byte] = Hex.decode("")
    val key2: Array[Byte] = Hex.decode("12")
    val val1: Array[Byte] = Hex.decode("01")
    val val2: Array[Byte] = Hex.decode("02")
    val trieWithTwoElements = emptyMpt.put(key1, val1).put(key2, val2)
    val trieAfterDelete = trieWithTwoElements.remove(key1)
    val obtained1 = trieAfterDelete.get(key1)
    assert(obtained1.isEmpty)
    val obtained2 = trieAfterDelete.get(key2)
    assert(obtained2.isDefined)
    assert(obtained2.get sameElements val2)
    assert(trieAfterDelete.getRootHash sameElements emptyMpt.put(key2, val2).getRootHash)
  }

  test("Remove of a trie with an extension whose next is not on source") {
    val key1: Array[Byte] = Hex.decode("123500")
    val key2: Array[Byte] = Hex.decode("123600")
    val key3: Array[Byte] = Hex.decode("123700")
    val key4: Array[Byte] = Hex.decode("123500")
    val trie = emptyMpt.put(key1, key1).put(key2, key2).put(key3, key3).put(key4, key4)
    val wrongSource = EphemDataSource()
    val rootValue = trie.nodeStorage.get(trie.getRootHash).cachedRlpEncoded.get
    wrongSource.update(
      Seq(
        DataSourceUpdate(
          IndexedSeq[Byte]('e'.toByte),
          toRemove = Seq(),
          toUpsert = (ByteString(trie.getRootHash) -> ArraySeq.unsafeWrapArray(rootValue)) :: Nil
        )
      )
    )
    val trieAfterDelete = Try {
      val trieWithWrongSource =
        MerklePatriciaTrie[Array[Byte], Array[Byte]](trie.getRootHash, StateStorage.getReadOnlyStorage(wrongSource))
      trieWithWrongSource.remove(key1)
    }
    assert(trieAfterDelete.isFailure)
  }

  test("Get in an empty trie") {
    val obtained = emptyMpt.get(Hex.decode("1234"))
    assert(obtained.isEmpty)
  }

  test("Insert with an empty key in a branch node") {
    val key1: Array[Byte] = Hex.decode("00")
    val key2: Array[Byte] = Hex.decode("f0")
    val trieWithBranch = emptyMpt.put(key1, key1).put(key2, key2)
    val key3: Array[Byte] = Hex.decode("")
    val val3: Array[Byte] = Hex.decode("1234")
    val trie = trieWithBranch.put(key3, val3)
    val key1Get = trie.get(key1)
    val key2Get = trie.get(key2)
    val key3Get = trie.get(key3)
    assert(key1Get.isDefined && (key1Get.get sameElements key1))
    assert(key2Get.isDefined && (key2Get.get sameElements key2))
    assert(key3Get.isDefined && (key3Get.get sameElements val3))
  }

  test("Remove of a key (not in trie) whose value should be in a branch node") {
    val key1: Array[Byte] = Hex.decode("1100")
    val key2: Array[Byte] = Hex.decode("11f0")
    val trieWithBranch = emptyMpt.put(key1, key1).put(key2, key2)
    val trieAfterDelete = trieWithBranch.remove(Hex.decode("11"))
    assert(trieAfterDelete.getRootHash sameElements trieWithBranch.getRootHash)
  }

  test("Remove of a key (not in trie) that should be in the child of a BranchNode that is not present") {
    val key1: Array[Byte] = Hex.decode("1100")
    val key2: Array[Byte] = Hex.decode("11f0")
    val trieWithBranch = emptyMpt.put(key1, key1).put(key2, key2)
    val trieAfterDelete = trieWithBranch.remove(Hex.decode("11a0"))
    assert(trieAfterDelete.getRootHash sameElements trieWithBranch.getRootHash)
  }

  test("Invalid root hash should return an error accordingly") {
    val rootId = "2" * 64 // 32 Bytes, should be stored

    val thrown = intercept[MPTException] {
      val invalidTrie = MerklePatriciaTrie[Array[Byte], Array[Byte]](Hex.decode(rootId), emptyEphemNodeStorage)
      invalidTrie.get(Hex.decode("1111")) // Try to get anything
    }

    assert(thrown.getMessage === s"Root node not found $rootId")
  }

  /* EthereumJ tests */
  test("testDeleteCompletellyDiferentItems") {
    val val1: String = "1000000000000000000000000000000000000000000000000000000000000000"
    val val2: String = "2000000000000000000000000000000000000000000000000000000000000000"
    val val3: String = "3000000000000000000000000000000000000000000000000000000000000000"
    val trieWithOneElement = emptyMpt.put(Hex.decode(val1), Hex.decode(val1))
    val trieWithTwoElements = trieWithOneElement.put(Hex.decode(val2), Hex.decode(val2))
    val root1: String = Hex.toHexString(trieWithTwoElements.getRootHash)
    val trieWithThreeElements = trieWithTwoElements.put(Hex.decode(val3), Hex.decode(val3))
    val trieAfterDelete = trieWithThreeElements.remove(Hex.decode(val3))
    val root1Obtained: String = Hex.toHexString(trieAfterDelete.getRootHash)
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
    val storage0 = emptyMpt
    val storage1 = storage0.put(key1, val1)
    val storage2 = storage1.put(key2, val2)
    val storage3 = storage2.put(key3, val3)
    val storage4 = storage3.put(key4, val4)
    val hash: String = Hex.toHexString(storage4.getRootHash)
    storage4.get(key1)
    assert("517eaccda568f3fa24915fed8add49d3b743b3764c0bc495b19a47c54dbc3d62" == hash)
  }

  test("EthereumJ compatibility - Empty Trie") {
    assert(Hex.toHexString(emptyMpt.getRootHash) == "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")
  }

  test("EthereumJ compatibility - LeafNode Hash") {
    val key1: Array[Byte] = Hex.decode("10")
    val val1: Array[Byte] = Hex.decode("947e70f9460402290a3e487dae01f610a1a8218fda")
    val storage = emptyMpt.put(key1, val1)
    assert(Hex.toHexString(storage.getRootHash) == "e6fbee0b67e3a6f0b9ea775ce585509aa4a0c3fe3f83d1e49a7d484489b755bc")
  }

  test("EtheruemJ compatibility - BranchNode hash") {
    val key1: Array[Byte] = Hex.decode("11")
    val key2: Array[Byte] = Hex.decode("00")
    val storage = emptyMpt.put(key1, key1).put(key2, key2)
    assert(Hex.toHexString(storage.getRootHash) == "47ec8b5bd21ca0cbd0bd003f2e791778ccae44e7e21476da60fe7a1e0e6ac838")
  }

  test("EthereumJ compatibility - ExtensionNode hash") {
    val key1: Array[Byte] = Hex.decode("1111")
    val key2: Array[Byte] = Hex.decode("1100")
    val storage = emptyMpt.put(key1, key1).put(key2, key2)
    assert(Hex.toHexString(storage.getRootHash) == "ad9523c662fc9654b79620685a67b1b521517c73d7915a8e18ef929e1c1e6807")
  }

  test("EthereumJ compatibility - BranchNode with extension hash") {
    val key1: Array[Byte] = Hex.decode("0111")
    val key2: Array[Byte] = Hex.decode("0211")
    val key3: Array[Byte] = Hex.decode("1100")
    val storage = emptyMpt.put(key1, key1).put(key2, key2).put(key3, key3)
    assert(Hex.toHexString(storage.getRootHash) == "a3d0686205c7ed10a85c3bce4118d5d559bcda47ca39e4dd4f09719958a179f1")
  }

  // This test was created to fix an error when Some(emptyHash) was used to create the trie
  test("Using empty root as hash allow to create a MPT") {
    val mpt = MerklePatriciaTrie[Array[Byte], Array[Byte]](emptyMpt.getRootHash, emptyEphemNodeStorage)
    val key1: Array[Byte] = Hex.decode("10")
    val val1: Array[Byte] = Hex.decode("947e70f9460402290a3e487dae01f610a1a8218fda")
    val storage = mpt.put(key1, val1)
    assert(Hex.toHexString(storage.getRootHash) == "e6fbee0b67e3a6f0b9ea775ce585509aa4a0c3fe3f83d1e49a7d484489b755bc")

  }

  /**
    * The MPT tested in this example has duplicated nodes as the branch node has two children with the same node: LeafNode("a", value)
    * When one of the key-value that uses one of this nodes is removed, this shouldn't affect the use of the other key-value
    * which shares the same LeafNode
    */
  test("PatriciaTrie insert causes node duplicated and removal of one of them should not fail") {
    val key1 = Hex.decode("ba")
    val key2 = Hex.decode("aa")
    val key3 = Hex.decode("")
    val value = Hex.decode(
      "012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789"
    )
    val trie = emptyMpt.put(key1, value).put(key2, value).put(key3, value)
    val trieAfterRemoval = trie.remove(key1)

    //Old trie still works
    assert(trie.get(key1).getOrElse(Array.emptyByteArray) sameElements value)
    assert(trie.get(key2).getOrElse(Array.emptyByteArray) sameElements value)
    assert(trie.get(key3).getOrElse(Array.emptyByteArray) sameElements value)

    //New trie is consistent
    assert(trieAfterRemoval.get(key1).isEmpty)
    assert(trieAfterRemoval.get(key2).getOrElse(Array.emptyByteArray) sameElements value)
    assert(trieAfterRemoval.get(key3).getOrElse(Array.emptyByteArray) sameElements value)
  }

  /**
    * Tests whether the creation of a duplicated valid temporal extension node removes it's original copy.
    * The creation of this temporal nodes happens in the case that an insertion is done on an extension node with a
    * partial match between the extension node key and the search key
    * Case tested:
    *             BN:  ['a'-> EN1, 'b'->EN2]
    *             EN1: [Key: 'aab', value: ...otherBN...]
    *             EN2: [Key: 'b', value: ...otherBN...]     <- Temporal extension node will be equal to this
    */
  test("Creating temporal extension node already used in MPT doesn't cause missing node while pruning") {
    def decodeHexString(hexString: String): ByteString = ByteString(Hex.decode(hexString))

    val pruningOffset = 10

    val (stateStorage, nodeStorage, cachedNodeStorage) =
      StateStorage.createTestStateStorage(EphemDataSource(), BasicPruning(40))

    val referenceCountBlock0 = stateStorage.getBackingStorage(0)
    val emptyTrieAtBlock0 = MerklePatriciaTrie[ByteString, Int](referenceCountBlock0)
    val trieAtBlock0 = emptyTrieAtBlock0
      .put(decodeHexString("aaab"), 8)
      .put(decodeHexString("aaabaaaa"), 10)
      .put(decodeHexString("bb"), 8)
      .put(decodeHexString("bbaaaa"), 10)

    // Insertion that generates the duplicated valid temporal extension node
    val referenceCountBlock10 = stateStorage.getBackingStorage(10)
    val trieAtBlock10 = MerklePatriciaTrie[ByteString, Int](trieAtBlock0.getRootHash, referenceCountBlock10)
      .put(decodeHexString("aaaa"), 6)

    // Cause pruning of all nodes deleted in the previous inserts
    // This previously caused the temporal extension node to be deleted, and as the temporal version was never inserted
    // it's copy was deleted instead
    (0 to (10 + pruningOffset + 1)).foreach(ReferenceCountNodeStorage.prune(_, cachedNodeStorage, inMemory = true))

    assert(trieAtBlock10.get(decodeHexString("aaab")).contains(8))
    assert(trieAtBlock10.get(decodeHexString("aaabaaaa")).contains(10))
    assert(trieAtBlock10.get(decodeHexString("bb")).contains(8))
    assert(trieAtBlock10.get(decodeHexString("bbaaaa")).contains(10))
    assert(trieAtBlock10.get(decodeHexString("aaaa")).contains(6))
  }

  /**
    * Tests whether the creation of a duplicated valid temporal leaf node removes it's original copy.
    * The creation of this temporal nodes happens in the case that an insertion is done on an leaf node with a
    * partial match between the leaf node key and the search key
    * Case tested:
    *             BN:  ['a'-> LN1, 'b'->LN2]
    *             LN1: [Key: 'b', value: ""]                       <- Temporal leaf node will be equal to this
    *             LN2: [Key: 'bbb', value: ..large bytestring...]
    */
  test("Creating temporal leaf node already used in MPT doesn't cause missing node while pruning") {
    def decodeHexString(hexString: String): ByteString = ByteString(Hex.decode(hexString))

    // Used so that the LeafNode is not stored encoded in it's parent, but directed to them through their hash (so it's
    // stored on the database)
    val largeByteString = ByteString((0 until 1000).map(_.toByte).toArray)
    val pruningOffset = 10

    val (stateStorage, nodeStorage, cachedNodeStorage) =
      StateStorage.createTestStateStorage(EphemDataSource(), BasicPruning(40))

    val referenceCountBlock0 = stateStorage.getBackingStorage(0)
    val emptyTrieAtBlock0 = MerklePatriciaTrie[ByteString, ByteString](referenceCountBlock0)
    val trieAtBlock0 = emptyTrieAtBlock0
      .put(decodeHexString("ab"), largeByteString)
      .put(decodeHexString("bbbb"), largeByteString)

    // Insertion that generates the duplicated valid temporal extension node
    val referenceCountBlock10 = stateStorage.getBackingStorage(10)
    val trieAtBlock10 = MerklePatriciaTrie[ByteString, ByteString](trieAtBlock0.getRootHash, referenceCountBlock10)
      .put(decodeHexString("bbba"), ByteString())

    // Cause pruning of all nodes deleted in the previous inserts
    // This previously caused the temporal leaf node to be deleted, and as the temporal version was never inserted it's
    // copy was deleted instead
    (0 to (10 + pruningOffset + 1)).foreach(ReferenceCountNodeStorage.prune(_, cachedNodeStorage, inMemory = true))

    assert(trieAtBlock10.get(decodeHexString("ab")).contains(largeByteString))
    assert(trieAtBlock10.get(decodeHexString("bbbb")).contains(largeByteString))
    assert(trieAtBlock10.get(decodeHexString("bbba")).contains(ByteString()))
  }

  test("getProof returns empty result from an empty tree") {
    val emptyTrie = MerklePatriciaTrie[Int, Int](emptyEphemNodeStorage)
    val proof = emptyTrie.getProof(key = 0)
    assert(proof.isEmpty)
  }

  test("getProof returns valid proof for existing key") {
    import scala.util.Random

    forAll(Gen.nonEmptyListOf(Arbitrary.arbitrary[(Int, Int)])) { keyValueList: Seq[(Int, Int)] =>
      // given
      val input: Seq[(Array[Byte], Array[Byte])] = keyValueList
        .map { case (k, v) => k.toString.getBytes() -> v.toString.getBytes() }

      val keyToFind: Array[Byte] = input.headOption
        .getOrElse(fail("Cant check proof for empty collection"))
        ._1

      val trie = Random
        .shuffle(input)
        .foldLeft(emptyMpt) { case (recTrie, (key, value)) =>
          recTrie.put(key, value)
        }

      // when
      val proof: Option[Vector[MptNode]] = trie.getProof(keyToFind)

      // then we can get proof if we know key exist
      assert(proof.isDefined)

      // then we can recreate MPT and get value using this key
      val nodeStorage: NodeStorage = proof.get.foldLeft(emptyNodeStorage) { case (storage, node) =>
        val k = ByteString(node.hash)
        val v = node.encode
        storage.put(k, v)
      }
      val mptStore: SerializingMptStorage = StateStorage.mptStorageFromNodeStorage(nodeStorage)
      val recreatedTree: MerklePatriciaTrie[Array[Byte], Array[Byte]] =
        MerklePatriciaTrie[Array[Byte], Array[Byte]](
          rootHash = trie.getRootHash,
          source = mptStore
        )

      assert(recreatedTree.get(keyToFind).isDefined)
    }
  }

  private def addEveryKeyValuePair[K, V](kvs: Seq[(Int, Int)]): MerklePatriciaTrie[Int, Int] =
    kvs.foldLeft(MerklePatriciaTrie[Int, Int](emptyEphemNodeStorage)) { case (recTrie, (key, value)) =>
      recTrie.put(key, value)
    }

  private def assertCanGetEveryKeyValue[K, V](trie: MerklePatriciaTrie[K, V], kvs: Seq[(K, V)]): Unit =
    kvs.foreach { case (key, value) =>
      val obtained = trie.get(key)
      assert(obtained.isDefined)
      assert(obtained.get == value)
    }

  private def assertCanGetEveryKeyValues[K, V](trie: MerklePatriciaTrie[K, Array[V]], kvs: List[(K, Array[V])]): Unit =
    kvs.foreach { case (key, value) =>
      val obtained = trie.get(key)
      assert(obtained.isDefined)
      assert(obtained.get sameElements value)
    }

  private def assertNotHave[K, V](trie: MerklePatriciaTrie[K, V], kvs: List[(K, V)]): Unit =
    kvs.foreach { case (key, _) =>
      val obtained = trie.get(key)
      assert(obtained.isEmpty)
    }
}
