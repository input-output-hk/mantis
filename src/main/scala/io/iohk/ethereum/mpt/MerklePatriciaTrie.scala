package io.iohk.ethereum.mpt

import akka.util.ByteString
import io.iohk.ethereum.common.SimpleMap
import io.iohk.ethereum.db.storage.NodeStorage
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.mpt.MerklePatriciaTrie.HashFn
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{decode => decodeRLP, encode => encodeRLP, _}

import scala.annotation.tailrec

object MerklePatriciaTrie {

  case class MPTException(message: String) extends RuntimeException(message)

  private case class NodeInsertResult(newNode: Node,
    toDeleteFromStorage: Seq[Node] = Seq(),
    toUpdateInStorage: Seq[Node] = Seq())

  private case class NodeRemoveResult(hasChanged: Boolean, newNode: Option[Node],
    toDeleteFromStorage: Seq[Node] = Seq(),
    toUpdateInStorage: Seq[Node] = Seq())

  type HashFn = Array[Byte] => Array[Byte]

  private val PairSize: Byte = 2
  private[mpt] val ListSize: Byte = 17

  def apply[K, V](source: NodeStorage, hashFn: HashFn)
    (implicit kSerializer: ByteArrayEncoder[K], vSerializer: ByteArraySerializable[V])
  : MerklePatriciaTrie[K, V] = new MerklePatriciaTrie[K, V](None, source, hashFn)(kSerializer, vSerializer)

  def apply[K, V](rootHash: Array[Byte], source: NodeStorage, hashFn: HashFn)
    (implicit kSerializer: ByteArrayEncoder[K], vSerializer: ByteArraySerializable[V])
  : MerklePatriciaTrie[K, V] = {
    if (calculateEmptyRootHash(hashFn) sameElements rootHash) MerklePatriciaTrie(source, hashFn)
    else new MerklePatriciaTrie[K, V](Some(rootHash), source, hashFn)(kSerializer, vSerializer)
  }

  def calculateEmptyRootHash(hashFn: HashFn): Array[Byte] = hashFn(encodeRLP(Array.emptyByteArray))

  private def getNode(nodeId: Array[Byte], source: NodeStorage)(implicit nodeDec: RLPDecoder[Node]): Node = {
    val nodeEncoded =
      if (nodeId.length < 32) nodeId
      else source.get(ByteString(nodeId)).getOrElse(throw MPTException("Node not found, trie is inconsistent"))
    decodeRLP[Node](nodeEncoded)
  }

  private def matchingLength(a: Array[Byte], b: Array[Byte]): Int = a.zip(b).takeWhile(t => t._1 == t._2).length

  private def updateNodesInStorage(
    previousRootHash: Array[Byte],
    newRoot: Option[Node],
    toRemove: Seq[Node],
    toUpdate: Seq[Node],
    nodeStorage: NodeStorage): NodeStorage = {
    val rootCapped = newRoot.map(_.capped).getOrElse(Array.emptyByteArray)
    val toBeRemoved = toRemove.filter { node =>
      val nCapped = node.capped
      nCapped.length == 32 || (node.hash sameElements previousRootHash)
    }.map(n => ByteString(n.hash))
    val toBeUpdated = toUpdate.filter { n =>
      val nCapped = n.capped
      nCapped.length == 32 || (nCapped sameElements rootCapped)
    }.map(node => ByteString(node.hash) -> node.encode)
    nodeStorage.update(toBeRemoved, toBeUpdated)
  }

  private def getNextNode(extensionNode: ExtensionNode, nodeStorage: NodeStorage)(implicit nodeDec: RLPDecoder[Node]): Node =
    extensionNode.next match {
      case Right(node) => node
      case Left(hash) => MerklePatriciaTrie.getNode(hash, nodeStorage)
    }

  private def getChild(branchNode: BranchNode, pos: Int, nodeStorage: NodeStorage)(implicit nodeDec: RLPDecoder[Node]): Option[Node] =
    branchNode.children(pos) map {
      case Right(node) => node
      case Left(hash) => MerklePatriciaTrie.getNode(hash, nodeStorage)
    }

  private[mpt] implicit val nodeEnc = new RLPEncoder[Node] {
    override def encode(obj: Node): RLPEncodeable = obj match {
      case leaf: LeafNode => RLPList(HexPrefix.encode(nibbles = leaf.key, isLeaf = true), leaf.value)
      case extension: ExtensionNode =>
        RLPList(HexPrefix.encode(nibbles = extension.sharedKey, isLeaf = false), extension.next match {
          case Right(node) => this.encode(node)
          case Left(bytes) => bytes
        })
      case branch: BranchNode =>
        val toEncode: Seq[RLPEncodeable] = branch.children.map {
          case Some(Right(node)) => this.encode(node)
          case Some(Left(bytes)) => RLPValue(bytes)
          case None => RLPValue(Array.emptyByteArray)
        } :+ RLPValue(branch.terminator.getOrElse(Array.emptyByteArray))
        RLPList(toEncode: _*)
    }
  }

  implicit val defaultByteArraySerializable = new ByteArraySerializable[Array[Byte]] {
    override def toBytes(input: Array[Byte]): Array[Byte] = input

    override def fromBytes(bytes: Array[Byte]): Array[Byte] = bytes
  }
}

class MerklePatriciaTrie[K, V](private val rootHash: Option[Array[Byte]],
  val nodeStorage: NodeStorage,
  private val hashFn: HashFn)
  (implicit kSerializer: ByteArrayEncoder[K], vSerializer: ByteArraySerializable[V])
  extends SimpleMap[K, V, MerklePatriciaTrie[K, V]] {

  import MerklePatriciaTrie._

  /**
    * Implicits
    */
  private[mpt] implicit val nodeDec = new RLPDecoder[Node] {
    override def decode(rlp: RLPEncodeable): Node = rlp match {
      case RLPList(items@_*) if items.size == MerklePatriciaTrie.ListSize =>
        val parsedChildren: Seq[Option[Either[Array[Byte], Node]]] = items.init.map {
          case list: RLPList => Some(Right(this.decode(list)))
          case RLPValue(bytes) =>
            if (bytes.isEmpty) None
            else Some(Left(bytes))
        }
        val terminatorAsArray: Array[Byte] = items.last
        BranchNode(children = parsedChildren, terminator = if (terminatorAsArray.isEmpty) None else Some(terminatorAsArray), hashFn)
      case RLPList(items@_*) if items.size == MerklePatriciaTrie.PairSize =>
        val (key, isLeaf) = HexPrefix.decode(items.head)
        if (isLeaf) LeafNode(key, items.last, hashFn)
        else {
          val next = items.last match {
            case list: RLPList => Right(this.decode(list))
            case RLPValue(bytes) => Left(bytes)
          }
          ExtensionNode(key, next, hashFn)
        }
      case _ => throw new RuntimeException("Invalid Node")
    }
  }

  private lazy val EmptyTrieHash = hashFn(encodeRLP(Array.emptyByteArray))
  lazy val getRootHash: Array[Byte] = rootHash.getOrElse(EmptyTrieHash)

  /**
    * This function obtains the value asociated with the key passed, if there exists one.
    *
    * @param key
    * @return Option object with value if there exists one.
    * @throws MPTException if there is any inconsistency in how the trie is build.
    */
  def get(key: K): Option[V] = {
    rootHash flatMap { rootId =>
      val keyNibbles = HexPrefix.bytesToNibbles(bytes = kSerializer.toBytes(key))
      val rootNode = getNode(rootId, nodeStorage)
      get(rootNode, keyNibbles).map(bytes => vSerializer.fromBytes(bytes))
    }
  }

  /**
    * This function inserts a (key-value) pair into the trie. If the key is already asociated with another value it is updated.
    *
    * @param key
    * @param value
    * @return New trie with the (key-value) pair inserted.
    * @throws MPTException if there is any inconsistency in how the trie is build.
    */
  override def put(key: K, value: V): MerklePatriciaTrie[K, V] = {
    val keyNibbles = HexPrefix.bytesToNibbles(kSerializer.toBytes(key))
    rootHash map { rootId =>
      val root = getNode(rootId, nodeStorage)
      val NodeInsertResult(newRoot, nodesToRemoveFromStorage, nodesToUpdateInStorage) = put(root, keyNibbles, vSerializer.toBytes(value))
      val newRootHash = newRoot.hash
      val newSource = updateNodesInStorage(
        previousRootHash = getRootHash,
        newRoot = Some(newRoot),
        toRemove = nodesToRemoveFromStorage,
        toUpdate = nodesToUpdateInStorage,
        nodeStorage = nodeStorage)
      new MerklePatriciaTrie(Some(newRootHash), newSource, hashFn)(kSerializer, vSerializer)
    } getOrElse {
      val newRoot = LeafNode(keyNibbles, vSerializer.toBytes(value), hashFn)
      val newRootHash = newRoot.hash
      new MerklePatriciaTrie(Some(newRootHash),
        updateNodesInStorage(getRootHash, Some(newRoot), Seq(), Seq(newRoot), nodeStorage),
        hashFn)
    }
  }

  /**
    * This function deletes a (key-value) pair from the trie. If no (key-value) pair exists with the passed trie then there's no effect on it.
    *
    * @param key
    * @return New trie with the (key-value) pair associated with the key passed deleted from the trie.
    * @throws MPTException if there is any inconsistency in how the trie is build.
    */
  override def remove(key: K): MerklePatriciaTrie[K, V] = {
    rootHash map { rootId =>
      val keyNibbles = HexPrefix.bytesToNibbles(bytes = kSerializer.toBytes(key))
      val root = getNode(rootId, nodeStorage)
      remove(root, keyNibbles) match {
        case NodeRemoveResult(true, Some(newRoot), nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
          val newRootHash = newRoot.hash
          val afterDeletenodeStorage = updateNodesInStorage(
            previousRootHash = getRootHash,
            newRoot = Some(newRoot),
            toRemove = nodesToRemoveFromStorage,
            toUpdate = nodesToUpdateInStorage,
            nodeStorage = nodeStorage)
          new MerklePatriciaTrie(Some(newRootHash), afterDeletenodeStorage, hashFn)(kSerializer, vSerializer)
        case NodeRemoveResult(true, None, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
          val afterDeletenodeStorage = updateNodesInStorage(
            previousRootHash = getRootHash,
            newRoot = None,
            toRemove = nodesToRemoveFromStorage,
            toUpdate = nodesToUpdateInStorage,
            nodeStorage = nodeStorage)
          new MerklePatriciaTrie(None, afterDeletenodeStorage, hashFn)(kSerializer, vSerializer)
        case NodeRemoveResult(false, _, _, _) => this
      }
    } getOrElse {
      this
    }
  }

  /**
    * This function updates the KeyValueStore by deleting, updating and inserting new (key-value) pairs.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStore.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStore.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): MerklePatriciaTrie[K, V] = {
    val afterRemoval = toRemove.foldLeft(this) { (acc, key) => acc - key }
    toUpsert.foldLeft(afterRemoval) { (acc, item) => acc + item }
  }

  @tailrec
  private def get(node: Node, searchKey: Array[Byte]): Option[Array[Byte]] = node match {
    case LeafNode(key, value, _) =>
      if (key sameElements searchKey) Some(value) else None
    case extNode@ExtensionNode(sharedKey, _, _) =>
      val (commonKey, remainingKey) = searchKey.splitAt(sharedKey.length)
      if (searchKey.length >= sharedKey.length && (sharedKey sameElements commonKey)) {
        val nextNode = getNextNode(extNode, nodeStorage)
        get(nextNode, remainingKey)
      }
      else None
    case branch@BranchNode(_, terminator, _) =>
      if (searchKey.isEmpty) terminator
      else getChild(branch, searchKey(0), nodeStorage) match {
        case Some(child) => get(child, searchKey.slice(1, searchKey.length))
        case None => None
      }
  }

  private def put(node: Node, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = node match {
    case leafNode: LeafNode => putInLeafNode(leafNode, searchKey, value)
    case extensionNode: ExtensionNode => putInExtensionNode(extensionNode, searchKey, value)
    case branchNode: BranchNode => putInBranchNode(branchNode, searchKey, value)
  }

  private def putInLeafNode(node: LeafNode, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    val LeafNode(existingKey, storedValue, _) = node
    matchingLength(existingKey, searchKey) match {
      case ml if ml == existingKey.length && ml == searchKey.length =>
        // We are trying to insert a leaf node that has the same key as this one but different value so we need to
        // replace it
        val newLeafNode = LeafNode(existingKey, value, hashFn)
        NodeInsertResult(
          newNode = newLeafNode,
          toDeleteFromStorage = Seq(node),
          toUpdateInStorage = Seq(newLeafNode)
        )
      case 0 =>
        // There is no common prefix between the node which means that we need to replace this leaf node
        val (temporalBranchNode, maybeNewLeaf) =
          if (existingKey.isEmpty) // This node has no key so it should be stored as branch's value
            BranchNode.withValueOnly(storedValue, hashFn) -> None
          else {
            // The leaf should be put inside one of new branch nibbles
            val newLeafNode = LeafNode(existingKey.tail, storedValue, hashFn)
            BranchNode.withSingleChild(existingKey(0), newLeafNode, None, hashFn) -> Some(newLeafNode)
          }
        val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage, toUpdateInStorage) = put(temporalBranchNode, searchKey, value)
        NodeInsertResult(
          newNode = newBranchNode,
          toDeleteFromStorage = node +: toDeleteFromStorage,
          toUpdateInStorage = maybeNewLeaf.toList ++ toUpdateInStorage
        )
      case ml =>
        // Partially shared prefix, we replace the leaf with an extension and a branch node
        val (searchKeyPrefix, searchKeySuffix) = searchKey.splitAt(ml)
        val temporalNode =
          if (ml == existingKey.length) BranchNode.withValueOnly(storedValue, hashFn)
          else LeafNode(existingKey.drop(ml), storedValue, hashFn)
        val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage, toUpdateInStorage) = put(temporalNode, searchKeySuffix, value)
        val newExtNode = ExtensionNode(searchKeyPrefix, newBranchNode, hashFn)
        NodeInsertResult(
          newNode = newExtNode,
          toDeleteFromStorage = node +: toDeleteFromStorage,
          toUpdateInStorage = newExtNode +: toUpdateInStorage
        )
    }
  }

  private def putInExtensionNode(extensionNode: ExtensionNode, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    val ExtensionNode(sharedKey, next, _) = extensionNode
    matchingLength(sharedKey, searchKey) match {
      case 0 =>
        // There is no common prefix with the node which means we have to replace it for a branch node
        val sharedKeyHead = sharedKey(0)
        val (temporalBranchNode, maybeNewLeaf) = {
          // Direct extension, we just replace the extension with a branch
          if (sharedKey.length == 1) BranchNode.withSingleChild(sharedKeyHead, next, None, hashFn) -> None
          else {
            // The new branch node will have an extension that replaces current one
            val newExtNode = ExtensionNode(sharedKey.tail, next, hashFn)
            BranchNode.withSingleChild(sharedKeyHead, newExtNode, None, hashFn) -> Some(newExtNode)
          }
        }
        val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage, toUpdateInStorage) = put(temporalBranchNode, searchKey, value)
        NodeInsertResult(
          newNode = newBranchNode,
          toDeleteFromStorage = extensionNode +: toDeleteFromStorage,
          toUpdateInStorage = maybeNewLeaf.toList ++ toUpdateInStorage
        )
      case ml if ml == sharedKey.length =>
        // Current extension node's key is a prefix of the one being inserted, so we insert recursively on the extension's child
        val NodeInsertResult(newChild: BranchNode, toDeleteFromStorage, toUpdateInStorage) =
          put(getNextNode(extensionNode, nodeStorage), searchKey.drop(ml), value)
        val newExtNode = ExtensionNode(sharedKey, newChild, hashFn)
        NodeInsertResult(
          newNode = newExtNode,
          toDeleteFromStorage = extensionNode +: toDeleteFromStorage,
          toUpdateInStorage = newExtNode +: toUpdateInStorage
        )
      case ml =>
        // Partially shared prefix, we have to replace the node with an extension with the shared prefix
        val (sharedKeyPrefix, sharedKeySuffix) = sharedKey.splitAt(ml)
        val temporalExtensionNode = ExtensionNode(sharedKeySuffix, next, hashFn)
        val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage, toUpdateInStorage) = put(temporalExtensionNode, searchKey.drop(ml), value)
        val newExtNode = ExtensionNode(sharedKeyPrefix, newBranchNode, hashFn)
        NodeInsertResult(
          newNode = newExtNode,
          toDeleteFromStorage = extensionNode +: toDeleteFromStorage,
          toUpdateInStorage = newExtNode +: toUpdateInStorage
        )
    }
  }

  private def putInBranchNode(branchNode: BranchNode, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    val BranchNode(children, _, _) = branchNode
    if (searchKey.isEmpty) {
      // The key is empty, the branch node should now be a terminator node with the new value asociated with it
      val newBranchNode = BranchNode(children, Some(value), hashFn)
      NodeInsertResult(
        newNode = newBranchNode,
        toDeleteFromStorage = Seq(branchNode),
        toUpdateInStorage = Seq(newBranchNode)
      )
    }
    else {
      // Non empty key, we need to insert the value in the correct branch node's child
      val searchKeyHead: Int = searchKey(0)
      val searchKeyRemaining = searchKey.tail
      if (children(searchKeyHead).isDefined) {
        // The associated child is not empty, we recursively insert in that child
        val NodeInsertResult(changedChild, toDeleteFromStorage, toUpdateInStorage) =
          put(getChild(branchNode, searchKeyHead, nodeStorage).get, searchKeyRemaining, value)
        val newBranchNode = branchNode.updateChild(searchKeyHead, changedChild, hashFn)
        NodeInsertResult(
          newNode = newBranchNode,
          toDeleteFromStorage = branchNode +: toDeleteFromStorage,
          toUpdateInStorage = newBranchNode +: toUpdateInStorage
        )
      }
      else {
        // The associated child is empty, we just replace it with a leaf
        val newLeafNode = LeafNode(searchKeyRemaining, value, hashFn)
        val newBranchNode = branchNode.updateChild(searchKeyHead, newLeafNode, hashFn)
        NodeInsertResult(
          newNode = newBranchNode,
          toDeleteFromStorage = Seq(branchNode),
          toUpdateInStorage = Seq(newLeafNode, newBranchNode)
        )
      }
    }
  }

  private def remove(node: Node, searchKey: Array[Byte]): NodeRemoveResult = node match {
    case leafNode: LeafNode => removeFromLeafNode(leafNode, searchKey)
    case extensionNode: ExtensionNode => removeFromExtensionNode(extensionNode, searchKey)
    case branchNode: BranchNode => removeFromBranchNode(branchNode, searchKey)
  }

  private def removeFromBranchNode(node: BranchNode, searchKey: Array[Byte]): NodeRemoveResult = (node, searchKey.isEmpty) match {
    // They key matches a branch node but it's value doesn't match the key
    case (BranchNode(_, None, _), true) => NodeRemoveResult(hasChanged = false, newNode = None)
    // We want to delete Branch node value
    case (BranchNode(children, _, _), true) =>
      // We need to remove old node and fix it because we removed the value
      val fixedNode = fix(BranchNode(children, None, hashFn), nodeStorage, Seq())
      NodeRemoveResult(hasChanged = true, newNode = Some(fixedNode), toDeleteFromStorage = Seq(node), toUpdateInStorage = Seq(fixedNode))
    case (branchNode@BranchNode(children, optStoredValue, _), false) =>
      // We might be trying to remove a node that's inside one of the 16 mapped nibbles
      val searchKeyHead = searchKey(0)
      getChild(branchNode, searchKeyHead, nodeStorage) map { child =>
        // Child has been found so we try to remove it
        remove(child, searchKey.tail) match {
          case NodeRemoveResult(true, maybeNewChild, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
            // Something changed in a child so we need to fix
            val nodeToFix = maybeNewChild map { newChild =>
              branchNode.updateChild(searchKeyHead, newChild, hashFn)
            } getOrElse {
              BranchNode(children.updated(searchKeyHead, None), optStoredValue, hashFn)
            }
            val fixedNode = fix(nodeToFix, nodeStorage, nodesToUpdateInStorage)
            NodeRemoveResult(
              hasChanged = true,
              newNode = Some(fixedNode),
              toDeleteFromStorage = node +: nodesToRemoveFromStorage,
              toUpdateInStorage = fixedNode +: nodesToUpdateInStorage)
          // No removal made on children, so we return without any change
          case NodeRemoveResult(false, _, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
            NodeRemoveResult(
              hasChanged = false,
              newNode = None,
              toDeleteFromStorage = nodesToRemoveFromStorage,
              toUpdateInStorage = nodesToUpdateInStorage)
        }
      } getOrElse {
        // Child not found in this branch node, so key is not present
        NodeRemoveResult(hasChanged = false, newNode = None)
      }
  }

  private def removeFromLeafNode(leafNode: LeafNode, searchKey: Array[Byte]): NodeRemoveResult = {
    val LeafNode(existingKey, _, _) = leafNode
    if (existingKey sameElements searchKey) {
      // We found the node to delete
      NodeRemoveResult(hasChanged = true, newNode = None, toDeleteFromStorage = Seq(leafNode))
    }
    else NodeRemoveResult(hasChanged = false, newNode = None)
  }

  private def removeFromExtensionNode(extensionNode: ExtensionNode, searchKey: Array[Byte]): NodeRemoveResult = {
    val ExtensionNode(sharedKey, _, _) = extensionNode
    val cp = matchingLength(sharedKey, searchKey)
    if (cp == sharedKey.length) {
      // A child node of this extension is removed, so move forward
      remove(getNextNode(extensionNode, nodeStorage), searchKey.drop(cp)) match {
        case NodeRemoveResult(true, maybeNewChild, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
          // If we changed the child, we need to fix this extension node
          maybeNewChild map { newChild =>
            val toFix = ExtensionNode(sharedKey, newChild, hashFn)
            val fixedNode = fix(toFix, nodeStorage, nodesToUpdateInStorage)
            NodeRemoveResult(
              hasChanged = true,
              newNode = Some(fixedNode),
              toDeleteFromStorage = extensionNode +: nodesToRemoveFromStorage,
              toUpdateInStorage = fixedNode +: nodesToUpdateInStorage)
          } getOrElse {
            throw MPTException("A trie with newRoot extension should have at least 2 values stored")
          }
        case NodeRemoveResult(false, _, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
          NodeRemoveResult(
            hasChanged = false,
            newNode = None,
            toDeleteFromStorage = nodesToRemoveFromStorage,
            toUpdateInStorage = nodesToUpdateInStorage)
      }
    }
    else NodeRemoveResult(hasChanged = false, newNode = Some(extensionNode))
  }

  /**
    * Given a node which may be in an invalid state, fix it such that it is then in a valid state. Invalid state means:
    *   - Branch node where there is only a single entry;
    *   - Extension node followed by anything other than a Branch node.
    *
    * @param node         that may be in an invalid state.
    * @param nodeStorage  to obtain the nodes referenced in the node that may be in an invalid state.
    * @param notStoredYet to obtain the nodes referenced in the node that may be in an invalid state,
    *                     if they were not yet inserted into the nodeStorage.
    * @return fixed node.
    * @throws MPTException if there is any inconsistency in how the trie is build.
    */
  @tailrec
  private def fix(node: Node, nodeStorage: NodeStorage, notStoredYet: Seq[Node]): Node = node match {
    case BranchNode(children, optStoredValue, _) =>
      val usedIndexes = children.indices.foldLeft[Seq[Int]](Seq.empty) {
        (acc, i) =>
          if (children(i).isDefined) i +: acc else acc
      }
      (usedIndexes, optStoredValue) match {
        case (Nil, None) => throw MPTException("Branch with no subvalues")
        case (index :: Nil, None) =>
          val temporalExtNode = ExtensionNode(Array[Byte](index.toByte), children(index).get, hashFn)
          fix(temporalExtNode, nodeStorage, notStoredYet)
        case (Nil, Some(value)) => LeafNode(Array.emptyByteArray, value, hashFn)
        case _ => node
      }
    case extensionNode@ExtensionNode(sharedKey, _, _) =>
      val nextNode = extensionNode.next match {
        case Left(nextHash) =>
          // If the node is not in the extension node then it might be a node to be inserted at the end of this remove
          // so we search in this list too
          notStoredYet.find(n => n.hash sameElements nextHash).getOrElse(
            getNextNode(extensionNode, nodeStorage) // We search for the node in the db
          )
        case Right(nextNodeOnExt) => nextNodeOnExt
      }
      val newNode = nextNode match {
        // Compact Two extensions into one
        case ExtensionNode(subSharedKey, subNext, _) => ExtensionNode(sharedKey ++ subSharedKey, subNext, hashFn)
        // Compact the extension and the leaf into the same leaf node
        case LeafNode(subRemainingKey, subValue, _) => LeafNode(sharedKey ++ subRemainingKey, subValue, hashFn)
        // It's ok
        case _: BranchNode => node
      }
      newNode
    case _ => node
  }
}

