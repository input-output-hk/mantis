package io.iohk.ethereum.mpt

import akka.util.ByteString
import cats.syntax.option._
import io.iohk.ethereum.common.SimpleMap
import io.iohk.ethereum.db.storage.MptStorage
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.mpt
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => encodeRLP}
import org.bouncycastle.util.encoders.Hex
import io.iohk.ethereum.utils.ByteUtils.matchingLength

import scala.annotation.tailrec

object MerklePatriciaTrie {

  implicit val defaultByteArraySerializable = new ByteArraySerializable[Array[Byte]] {
    override def toBytes(input: Array[Byte]): Array[Byte] = input

    override def fromBytes(bytes: Array[Byte]): Array[Byte] = bytes
  }

  class MPTException(val message: String) extends RuntimeException(message)

  class MissingNodeException protected (val hash: ByteString, message: String) extends MPTException(message) {
    def this(hash: ByteString) = this(hash, s"Node not found ${Hex.toHexString(hash.toArray)}, trie is inconsistent")
  }

  class MissingRootNodeException(hash: ByteString)
      extends MissingNodeException(hash, s"Root node not found ${Hex.toHexString(hash.toArray)}")

  val EmptyEncoded = encodeRLP(Array.emptyByteArray)
  val EmptyRootHash: Array[Byte] = Node.hashFn(EmptyEncoded)

  private case class NodeInsertResult(newNode: MptNode, toDeleteFromStorage: List[MptNode] = Nil)

  private case class NodeRemoveResult(
      hasChanged: Boolean,
      newNode: Option[MptNode],
      toDeleteFromStorage: List[MptNode] = Nil
  )

  private[mpt] val PairSize: Byte = 2
  private[mpt] val ListSize: Byte = 17

  def apply[K, V](
      source: MptStorage
  )(implicit kSerializer: ByteArrayEncoder[K], vSerializer: ByteArraySerializable[V]): MerklePatriciaTrie[K, V] =
    new MerklePatriciaTrie[K, V](None, source)(kSerializer, vSerializer)

  def apply[K, V](rootHash: Array[Byte], source: MptStorage)(implicit
      kSerializer: ByteArrayEncoder[K],
      vSerializer: ByteArraySerializable[V]
  ): MerklePatriciaTrie[K, V] = {
    if (EmptyRootHash sameElements rootHash)
      MerklePatriciaTrie(source)
    else {
      new MerklePatriciaTrie[K, V](Some(mpt.HashNode(rootHash)), source)(kSerializer, vSerializer)
    }
  }
}

trait NodesKeyValueStorage extends SimpleMap[NodeHash, NodeEncoded, NodesKeyValueStorage] {
  def persist(): Unit
}

class MerklePatriciaTrie[K, V] private (private[mpt] val rootNode: Option[MptNode], val nodeStorage: MptStorage)(
    implicit
    kSerializer: ByteArrayEncoder[K],
    vSerializer: ByteArraySerializable[V]
) extends SimpleMap[K, V, MerklePatriciaTrie[K, V]] {

  import MerklePatriciaTrie._

  lazy val getRootHash: Array[Byte] = rootNode.map(_.hash).getOrElse(EmptyRootHash)

  /**
    * Get the value associated with the key passed, if there exists one.
    *
    * @param key
    * @return Option object with value if there exists one.
    * @throws io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException if there is any inconsistency in how the trie is build.
    */
  def get(key: K): Option[V] = {
    pathTraverse[Option[V]](None, mkKeyNibbles(key)) { case (_, node) =>
      node match {
        case LeafNode(_, value, _, _, _) =>
          Some(vSerializer.fromBytes(value.toArray[Byte]))

        case BranchNode(_, terminator, _, _, _) =>
          terminator.map(term => vSerializer.fromBytes(term.toArray[Byte]))

        case _ => None
      }
    }.flatten
  }

  /**
    * Get the proof associated with the key passed, if there exists one.
    *
    * @param key
    * @return Option object with proof if there exists one.
    * @throws io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException if there is any inconsistency in how the trie is build.
    */
  def getProof(key: K): Option[Vector[MptNode]] = {
    pathTraverse[Vector[MptNode]](Vector.empty, mkKeyNibbles(key)) { case (acc, node) =>
      node match {
        case nextNodeOnExt @ (_: BranchNode | _: ExtensionNode | _: LeafNode) => acc :+ nextNodeOnExt
        case _ => acc
      }
    }
  }

  def getProof(key: K): Option[Seq[V]] = {
    rootNode.flatMap { rootNode =>
      val keyNibbles: Array[Byte] = HexPrefix.bytesToNibbles(bytes = kSerializer.toBytes(key))
      getProof(rootNode, keyNibbles, List.empty.some)
        .map(each => each.map(bytes => vSerializer.fromBytes(bytes)))
    }
  }

//  @tailrec
  private def getProof(
      node: MptNode,
      searchKey: Array[Byte],
      soFar: Option[Seq[Array[Byte]]]
  ): Option[Seq[Array[Byte]]] =
    node match {
      case LeafNode(key, value, _, _, _) =>
//        if (key.toArray[Byte].sameElements(searchKey)) Some(value.toArray[Byte])
//        else None
        None
      case extNode @ ExtensionNode(sharedKey, _, _, _, _) =>
//        val (commonKey, remainingKey) = searchKey.splitAt(sharedKey.length)
//        if (searchKey.length >= sharedKey.length && (sharedKey sameElements commonKey))
//          getProof(extNode.next, remainingKey)
//        else None
        None
      case branch @ BranchNode(_, terminator, _, _, _) =>
        None
//        if (searchKey.isEmpty) terminator.map(_.toArray[Byte])
//        else getProof(branch.children(searchKey(0)), searchKey.slice(1, searchKey.length))
      case HashNode(bytes) =>
        None
//        getProof(nodeStorage.get(bytes), searchKey)
      case NullNode =>
        None
    }

  /**
    * Traverse given path from the root to value and accumulate data.
    * Only nodes which are significant for searching for value are taken into account.
    *
    * @param acc initial accumulator
    * @param searchKey search key
    * @param op accumulating operation
    * @tparam T accumulator type
    * @return accumulated data or None if key doesn't exist
    */
  private def pathTraverse[T](acc: T, searchKey: Array[Byte])(op: (T, MptNode) => T): Option[T] = {

    @tailrec
    def pathTraverse(acc: T, node: MptNode, searchKey: Array[Byte], op: (T, MptNode) => T): Option[T] = {
      node match {
        case LeafNode(key, _, _, _, _) =>
          if (key.toArray[Byte] sameElements searchKey) Some(op(acc, node)) else None

        case extNode @ ExtensionNode(sharedKey, _, _, _, _) =>
          val (commonKey, remainingKey) = searchKey.splitAt(sharedKey.length)
          if (searchKey.length >= sharedKey.length && (sharedKey.toArray[Byte] sameElements commonKey)) {
            pathTraverse(op(acc, node), extNode.next, remainingKey, op)
          } else None

        case branch: BranchNode =>
          if (searchKey.isEmpty) Some(op(acc, node))
          else
            pathTraverse(
              op(acc, node),
              branch.children(searchKey(0)),
              searchKey.slice(1, searchKey.length),
              op
            )

        case HashNode(bytes) =>
          pathTraverse(acc, getFromHash(bytes, nodeStorage), searchKey, op)

        case NullNode =>
          None
      }
    }

    rootNode match {
      case Some(root) =>
        pathTraverse(acc, root, searchKey, op)
      case None =>
        None
    }
  }

  private def getFromHash(nodeId: Array[Byte], source: MptStorage): MptNode = {
    val nodeEncoded = source.get(nodeId).encode
    MptTraversals
      .decodeNode(nodeEncoded)
      .withCachedHash(nodeId)
      .withCachedRlpEncoded(nodeEncoded)
  }

  private def mkKeyNibbles(key: K): Array[Byte] = HexPrefix.bytesToNibbles(kSerializer.toBytes(key))

  /**
    * This function inserts a (key-value) pair into the trie. If the key is already asociated with another value it is updated.
    *
    * @param key
    * @param value
    * @return New trie with the (key-value) pair inserted.
    * @throws io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException if there is any inconsistency in how the trie is build.
    */
  override def put(key: K, value: V): MerklePatriciaTrie[K, V] = {
    val keyNibbles = HexPrefix.bytesToNibbles(kSerializer.toBytes(key))
    rootNode map { root =>
      val NodeInsertResult(newRoot, nodesToRemoveFromStorage) = put(root, keyNibbles, vSerializer.toBytes(value))
      val newRootNode = nodeStorage.updateNodesInStorage(newRoot = Some(newRoot), toRemove = nodesToRemoveFromStorage)
      new MerklePatriciaTrie(newRootNode, nodeStorage)(kSerializer, vSerializer)
    } getOrElse {
      val newRoot = LeafNode(ByteString(keyNibbles), ByteString(vSerializer.toBytes(value)))
      val newRootNode = nodeStorage.updateNodesInStorage(Some(newRoot), Nil)
      new MerklePatriciaTrie(newRootNode, nodeStorage)
    }
  }

  /**
    * This function deletes a (key-value) pair from the trie. If no (key-value) pair exists with the passed trie then there's no effect on it.
    *
    * @param key
    * @return New trie with the (key-value) pair associated with the key passed deleted from the trie.
    * @throws io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException if there is any inconsistency in how the trie is build.
    */
  override def remove(key: K): MerklePatriciaTrie[K, V] = {
    rootNode map { root =>
      val keyNibbles = HexPrefix.bytesToNibbles(bytes = kSerializer.toBytes(key))
      remove(root, keyNibbles) match {
        case NodeRemoveResult(true, Some(newRoot), nodesToRemoveFromStorage) =>
          val newRootNode =
            nodeStorage.updateNodesInStorage(newRoot = Some(newRoot), toRemove = nodesToRemoveFromStorage)
          new MerklePatriciaTrie(newRootNode, nodeStorage)(kSerializer, vSerializer)
        case NodeRemoveResult(true, None, nodesToRemoveFromStorage) =>
          val newRootNode = nodeStorage.updateNodesInStorage(newRoot = None, toRemove = nodesToRemoveFromStorage)
          new MerklePatriciaTrie(None, nodeStorage)(kSerializer, vSerializer)
        case NodeRemoveResult(false, _, _) => this
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
  private def get(node: MptNode, searchKey: Array[Byte]): Option[Array[Byte]] = node match {
    case LeafNode(key, value, _, _, _) =>
      if (key.toArray[Byte] sameElements searchKey) Some(value.toArray[Byte]) else None
    case extNode @ ExtensionNode(sharedKey, _, _, _, _) =>
      val (commonKey, remainingKey) = searchKey.splitAt(sharedKey.length)
      if (searchKey.length >= sharedKey.length && (sharedKey sameElements commonKey)) {
        get(extNode.next, remainingKey)
      } else None
    case branch @ BranchNode(_, terminator, _, _, _) =>
      if (searchKey.isEmpty) terminator.map(_.toArray[Byte])
      else {
        get(branch.children(searchKey(0)), searchKey.slice(1, searchKey.length))
      }
    case HashNode(bytes) =>
      get(nodeStorage.get(bytes), searchKey)

    case NullNode =>
      None
  }

  private def put(node: MptNode, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = node match {
    case leafNode: LeafNode => putInLeafNode(leafNode, searchKey, value)
    case extensionNode: ExtensionNode => putInExtensionNode(extensionNode, searchKey, value)
    case branchNode: BranchNode => putInBranchNode(branchNode, searchKey, value)
    case HashNode(bytes) =>
      put(nodeStorage.get(bytes), searchKey, value)
    case _ => throw new MPTException("Cannot put node in NullNode")
  }

  private def putInLeafNode(node: LeafNode, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    val LeafNode(existingKey, storedValue, _, _, _) = node
    matchingLength(existingKey.toArray[Byte], searchKey) match {
      case ml if ml == existingKey.length && ml == searchKey.length =>
        // We are trying to insert a leaf node that has the same key as this one but different value so we need to
        // replace it
        val newLeafNode = LeafNode(existingKey, ByteString(value))
        NodeInsertResult(
          newNode = newLeafNode,
          toDeleteFromStorage = List(node)
        )
      case 0 =>
        // There is no common prefix between the node which means that we need to replace this leaf node
        val (temporalBranchNode, maybeNewLeaf) =
          if (existingKey.isEmpty) // This node has no key so it should be stored as branch's value
            BranchNode.withValueOnly(storedValue.toArray[Byte]) -> None
          else {
            // The leaf should be put inside one of new branch nibbles
            val newLeafNode = LeafNode(existingKey.tail, storedValue)
            BranchNode.withSingleChild(existingKey(0), newLeafNode, None) -> Some(newLeafNode)
          }
        val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage) = put(temporalBranchNode, searchKey, value)
        NodeInsertResult(
          newNode = newBranchNode,
          toDeleteFromStorage = node :: toDeleteFromStorage.filterNot(_ == temporalBranchNode)
        )
      case ml =>
        // Partially shared prefix, we replace the leaf with an extension and a branch node
        val (searchKeyPrefix, searchKeySuffix) = searchKey.splitAt(ml)
        val temporalNode =
          if (ml == existingKey.length) BranchNode.withValueOnly(storedValue.toArray[Byte])
          else LeafNode(existingKey.drop(ml), storedValue)
        val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage) = put(temporalNode, searchKeySuffix, value)
        val newExtNode = ExtensionNode(ByteString(searchKeyPrefix), newBranchNode)
        NodeInsertResult(
          newNode = newExtNode,
          toDeleteFromStorage = node :: toDeleteFromStorage.filterNot(_ == temporalNode)
        )
    }
  }

  private def putInExtensionNode(
      extensionNode: ExtensionNode,
      searchKey: Array[Byte],
      value: Array[Byte]
  ): NodeInsertResult = {
    val ExtensionNode(sharedKey, next, _, _, _) = extensionNode
    matchingLength(sharedKey.toArray[Byte], searchKey) match {
      case 0 =>
        // There is no common prefix with the node which means we have to replace it for a branch node
        val sharedKeyHead = sharedKey(0)
        val (temporalBranchNode, maybeNewExtNode) = {
          // Direct extension, we just replace the extension with a branch
          if (sharedKey.length == 1) BranchNode.withSingleChild(sharedKeyHead, next, None) -> None
          else {
            // The new branch node will have an extension that replaces current one
            val newExtNode = ExtensionNode(sharedKey.tail, next)
            BranchNode.withSingleChild(sharedKeyHead, newExtNode, None) -> Some(newExtNode)
          }
        }
        val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage) = put(temporalBranchNode, searchKey, value)
        NodeInsertResult(
          newNode = newBranchNode,
          toDeleteFromStorage = extensionNode :: toDeleteFromStorage.filterNot(_ == temporalBranchNode)
        )
      case ml if ml == sharedKey.length =>
        // Current extension node's key is a prefix of the one being inserted, so we insert recursively on the extension's child
        val NodeInsertResult(newChild: BranchNode, toDeleteFromStorage) =
          put(extensionNode.next, searchKey.drop(ml), value)
        val newExtNode = ExtensionNode(sharedKey, newChild)
        NodeInsertResult(
          newNode = newExtNode,
          toDeleteFromStorage = extensionNode :: toDeleteFromStorage
        )
      case ml =>
        // Partially shared prefix, we have to replace the node with an extension with the shared prefix
        val (sharedKeyPrefix, sharedKeySuffix) = sharedKey.splitAt(ml)
        val temporalExtensionNode = ExtensionNode(sharedKeySuffix, next)
        val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage) =
          put(temporalExtensionNode, searchKey.drop(ml), value)
        val newExtNode = ExtensionNode(sharedKeyPrefix, newBranchNode)
        NodeInsertResult(
          newNode = newExtNode,
          toDeleteFromStorage = extensionNode :: toDeleteFromStorage.filterNot(_ == temporalExtensionNode)
        )
    }
  }

  private def putInBranchNode(branchNode: BranchNode, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    val BranchNode(children, _, _, _, _) = branchNode
    if (searchKey.isEmpty) {
      // The key is empty, the branch node should now be a terminator node with the new value asociated with it
      val newBranchNode = BranchNode(children, Some(ByteString(value)))
      NodeInsertResult(
        newNode = newBranchNode,
        toDeleteFromStorage = List(branchNode)
      )
    } else {
      // Non empty key, we need to insert the value in the correct branch node's child
      val searchKeyHead: Int = searchKey(0)
      val searchKeyRemaining = searchKey.tail
      if (!children(searchKeyHead).isNull) {
        // The associated child is not empty, we recursively insert in that child
        val NodeInsertResult(changedChild, toDeleteFromStorage) =
          put(branchNode.children(searchKeyHead), searchKeyRemaining, value)
        val newBranchNode = branchNode.updateChild(searchKeyHead, changedChild)
        NodeInsertResult(
          newNode = newBranchNode,
          toDeleteFromStorage = branchNode :: toDeleteFromStorage
        )
      } else {
        // The associated child is empty, we just replace it with a leaf
        val newLeafNode = LeafNode(ByteString(searchKeyRemaining), ByteString(value))
        val newBranchNode = branchNode.updateChild(searchKeyHead, newLeafNode)
        NodeInsertResult(
          newNode = newBranchNode,
          toDeleteFromStorage = List(branchNode)
        )
      }
    }
  }

  private def remove(node: MptNode, searchKey: Array[Byte]): NodeRemoveResult = node match {
    case leafNode: LeafNode => removeFromLeafNode(leafNode, searchKey)
    case extensionNode: ExtensionNode => removeFromExtensionNode(extensionNode, searchKey)
    case branchNode: BranchNode => removeFromBranchNode(branchNode, searchKey)
    case HashNode(bytes) =>
      remove(nodeStorage.get(bytes), searchKey)
    case _ => throw new MPTException("Cannot delete node NullNode")
  }

  private def removeFromBranchNode(node: BranchNode, searchKey: Array[Byte]): NodeRemoveResult =
    (node, searchKey.isEmpty) match {
      // They key matches a branch node but it's value doesn't match the key
      case (BranchNode(_, None, _, _, _), true) => NodeRemoveResult(hasChanged = false, newNode = None)
      // We want to delete Branch node value
      case (BranchNode(children, _, _, _, _), true) =>
        // We need to remove old node and fix it because we removed the value
        val fixedNode = fix(BranchNode(children, None))
        NodeRemoveResult(hasChanged = true, newNode = Some(fixedNode), toDeleteFromStorage = List(node))
      case (branchNode @ BranchNode(children, optStoredValue, _, _, _), false) =>
        // We might be trying to remove a node that's inside one of the 16 mapped nibbles
        val searchKeyHead = searchKey(0)
        // Get Child will never return HashNode, it is match clause to satisfy compiler
        branchNode.children(searchKeyHead) match {
          case child @ (_: BranchNode | _: ExtensionNode | _: LeafNode | _: HashNode) =>
            // Child has been found so we try to remove it
            remove(child, searchKey.tail) match {
              case NodeRemoveResult(true, maybeNewChild, nodesToRemoveFromStorage) =>
                // Something changed in a child so we need to fix
                val nodeToFix = maybeNewChild map { newChild =>
                  branchNode.updateChild(searchKeyHead, newChild)
                } getOrElse {
                  BranchNode(children.updated(searchKeyHead, NullNode), optStoredValue)
                }
                val fixedNode = fix(nodeToFix)
                NodeRemoveResult(
                  hasChanged = true,
                  newNode = Some(fixedNode),
                  toDeleteFromStorage = node :: nodesToRemoveFromStorage
                )
              // No removal made on children, so we return without any change
              case NodeRemoveResult(false, _, nodesToRemoveFromStorage) =>
                NodeRemoveResult(hasChanged = false, newNode = None, toDeleteFromStorage = nodesToRemoveFromStorage)
            }
          case NullNode =>
            // Child not found in this branch node, so key is not present
            NodeRemoveResult(hasChanged = false, newNode = None)
        }
    }

  private def removeFromLeafNode(leafNode: LeafNode, searchKey: Array[Byte]): NodeRemoveResult = {
    val LeafNode(existingKey, _, _, _, _) = leafNode
    if (existingKey sameElements searchKey) {
      // We found the node to delete
      NodeRemoveResult(hasChanged = true, newNode = None, toDeleteFromStorage = List(leafNode))
    } else NodeRemoveResult(hasChanged = false, newNode = None)
  }

  private def removeFromExtensionNode(extensionNode: ExtensionNode, searchKey: Array[Byte]): NodeRemoveResult = {
    val ExtensionNode(sharedKey, _, _, _, _) = extensionNode
    val cp = matchingLength(sharedKey.toArray[Byte], searchKey)
    if (cp == sharedKey.length) {
      // A child node of this extension is removed, so move forward
      remove(extensionNode.next, searchKey.drop(cp)) match {
        case NodeRemoveResult(true, maybeNewChild, nodesToRemoveFromStorage) =>
          // If we changed the child, we need to fix this extension node
          maybeNewChild map { newChild =>
            val toFix = ExtensionNode(sharedKey, newChild)
            val fixedNode = fix(toFix)
            NodeRemoveResult(
              hasChanged = true,
              newNode = Some(fixedNode),
              toDeleteFromStorage = extensionNode :: nodesToRemoveFromStorage
            )
          } getOrElse {
            throw new MPTException("A trie with newRoot extension should have at least 2 values stored")
          }
        case NodeRemoveResult(false, _, nodesToRemoveFromStorage) =>
          NodeRemoveResult(hasChanged = false, newNode = None, toDeleteFromStorage = nodesToRemoveFromStorage)
      }
    } else NodeRemoveResult(hasChanged = false, newNode = Some(extensionNode))
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
    * @throws io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException if there is any inconsistency in how the trie is build.
    */
  @tailrec
  private def fix(node: MptNode): MptNode = node match {
    case BranchNode(children, optStoredValue, _, _, _) =>
      val usedIndexes = children.indices.foldLeft[Seq[Int]](Nil) { (acc, i) =>
        if (!children(i).isNull) i +: acc else acc
      }
      (usedIndexes, optStoredValue) match {
        case (Nil, None) => throw new MPTException("Branch with no subvalues")
        case (index :: Nil, None) =>
          val temporalExtNode = ExtensionNode(ByteString(index.toByte), children(index))
          fix(temporalExtNode)
        case (Nil, Some(value)) => LeafNode(ByteString.empty, value)
        case _ => node
      }
    case extensionNode @ ExtensionNode(sharedKey, _, _, _, _) =>
      val nextNode = extensionNode.next match {
        case HashNode(nextHash) =>
          // If the node is not in the extension node then it might be a node to be inserted at the end of this remove
          // so we search in this list too
          nodeStorage.get(nextHash) // We search for the node in the db
        case nextNodeOnExt @ (_: BranchNode | _: ExtensionNode | _: LeafNode | _: NullNode.type) => nextNodeOnExt
      }
      val newNode = nextNode match {
        // Compact Two extensions into one
        case ExtensionNode(subSharedKey, subNext, _, _, _) => ExtensionNode(sharedKey ++ subSharedKey, subNext)
        // Compact the extension and the leaf into the same leaf node
        case LeafNode(subRemainingKey, subValue, _, _, _) => LeafNode(sharedKey ++ subRemainingKey, subValue)
        // It's ok
        case _: BranchNode | _: HashNode | _: NullNode.type => node
      }
      newNode
    case _ => node
  }
}
