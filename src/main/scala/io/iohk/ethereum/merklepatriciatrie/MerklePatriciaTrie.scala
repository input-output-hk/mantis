package io.iohk.ethereum.merklepatriciatrie

import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicLong

import io.iohk.ethereum.merklepatriciatrie.MerklePatriciaTrie.HashFn
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{decode => decodeRLP, encode => encodeRLP, _}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object MerklePatriciaTrie {

  case class MPTException(message: String) extends RuntimeException(message)

  type HashFn = Array[Byte] => Array[Byte]

  private val PairSize: Byte = 2
  private[merklepatriciatrie] val ListSize: Byte = 17

  def apply[K, V](source: DataSource, hashFn: HashFn)
                 (implicit kSerializer: ByteArraySerializable[K], vSerializer: ByteArraySerializable[V])
  : MerklePatriciaTrie[K, V] = new MerklePatriciaTrie[K, V](None, source, hashFn)(kSerializer, vSerializer)

  def apply[K, V](rootHash: Array[Byte], source: DataSource, hashFn: HashFn)
                 (implicit kSerializer: ByteArraySerializable[K], vSerializer: ByteArraySerializable[V])
  : MerklePatriciaTrie[K, V] = new MerklePatriciaTrie[K, V](Some(rootHash), source, hashFn)(kSerializer, vSerializer)

  private def getNode(nodeId: Array[Byte], source: DataSource)(implicit nodeDec: RLPDecoder[Node]): Node = {
    val nodeEncoded = tryGetNode(nodeId, source)
    decodeRLP[Node](nodeEncoded)
  }

  private def tryGetNode[K <: ByteArraySerializable[K], V <: ByteArraySerializable[V]](key: Array[Byte],
                                                                                       source: DataSource): Array[Byte] =
    if (key.length < 32) key
    else source.get(key).getOrElse(throw MPTException("Node not found, trie is inconsistent"))

  private def matchingLength(bytes1: Array[Byte], bytes2: Array[Byte]): Int = bytes1.zip(bytes2).takeWhile(t => t._1 == t._2).length

  private def updateNodesInStorage(previousRootHash: Array[Byte], newRootHash: Array[Byte], newRoot: Option[Node],
                                   toRemove: Seq[Node], toUpdate: Seq[Node], dataSource: DataSource, hashFn: HashFn): DataSource = {
    val rootCapped = newRoot.map(_.capped(hashFn)).getOrElse(Array.emptyByteArray)
    val toBeRemoved = toRemove.filter{node =>
      val nCapped = node.capped(hashFn)
      nCapped.length == 32 || (node.hash(hashFn) sameElements previousRootHash) }.map(_.hash(hashFn))
    val toBeUpdated = toUpdate.filter { n =>
      val nCapped = n.capped(hashFn)
      nCapped.length == 32 || (nCapped sameElements rootCapped)
    }.map(node => node.hash(hashFn) -> node.encode)
    dataSource.update(newRootHash, toBeRemoved, toBeUpdated)
  }

  private def getNextNode(extensionNode: ExtensionNode, dataSource: DataSource)(implicit nodeDec: RLPDecoder[Node]): Node =
    extensionNode.next match {
      case Right(node) => node
      case Left(hash) => MerklePatriciaTrie.getNode(hash, dataSource)
    }

  private def getChild(branchNode: BranchNode, pos: Int, dataSource: DataSource)(implicit nodeDec: RLPDecoder[Node]): Option[Node] =
    branchNode.children(pos) match {
      case Some(Right(node)) => Some(node)
      case Some(Left(hash)) => Some(MerklePatriciaTrie.getNode(hash, dataSource))
      case None => None
    }

  implicit val defaultByteArraySerializable = new ByteArraySerializable[Array[Byte]] {
    override def toBytes(input: Array[Byte]): Array[Byte] = input

    override def fromBytes(bytes: Array[Byte]): Array[Byte] = bytes
  }

  /**
    * Implicits
    */
  private[merklepatriciatrie] implicit val nodeEncDec = new RLPDecoder[Node] with RLPEncoder[Node] {
    override def encode(obj: Node): RLPEncodeable = obj match {
      case leaf: LeafNode => RLPList(HexPrefix.encode(nibbles = leaf.key, t = true), leaf.value)
      case extension: ExtensionNode =>
        RLPList(HexPrefix.encode(nibbles = extension.sharedKey, t = false), extension.next match {
          case Right(node) => this.encode(node)
          case Left(bytes) => bytes
        })
      case branch: BranchNode =>
        val toEncode: Seq[RLPEncodeable] = branch.children.map {
          case Some(Right(node)) => this.encode(node)
          case Some(Left(bytes)) => RLPValue(bytes)
          case _ => RLPValue(Array.emptyByteArray)
        } :+ RLPValue(branch.terminator.getOrElse(Array.emptyByteArray))
        RLPList(toEncode: _*)
    }

    override def decode(rlp: RLPEncodeable): Node = rlp match {
      case RLPList(items@_*) if items.size == MerklePatriciaTrie.ListSize =>
        val parsedChildren: Seq[Option[Either[Array[Byte], Node]]] = items.init.map {
          case list: RLPList => Some(Right(this.decode(list)))
          case RLPValue(bytes) =>
            if (bytes.isEmpty) None
            else Some(Left(bytes))
        }
        val terminatorAsArray: Array[Byte] = items.last
        BranchNode(children = parsedChildren, terminator = if (terminatorAsArray.isEmpty) None else Some(terminatorAsArray))
      case RLPList(items@_*) if items.size == MerklePatriciaTrie.PairSize =>
        val (key, isLeaf) = HexPrefix.decode(items.head)
        if (isLeaf) LeafNode(key, items.last)
        else {
          val next = items.last match {
            case list: RLPList => Right(this.decode(list))
            case RLPValue(bytes) => Left(bytes)
          }
          ExtensionNode(key, next)
        }
      case _ => throw new RuntimeException("Invalid Node")
    }
  }
}

class MerklePatriciaTrie[K, V](private val rootHash: Option[Array[Byte]],
                               val dataSource: DataSource,
                               private val hashFn: HashFn)
                              (implicit kSerializer: ByteArraySerializable[K], vSerializer: ByteArraySerializable[V]) {

  import MerklePatriciaTrie._

  private lazy val EmptyTrieHash = hashFn(encodeRLP(Array.emptyByteArray))
  lazy val getRootHash = rootHash match {
    case Some(root) => root
    case None => EmptyTrieHash
  }

  /**
    * This function obtains the value asociated with the key passed, if there exists one.
    *
    * @param key
    * @return Option object with value if there exists one.
    * @throws MPTException if there is any inconsistency in how the trie is build.
    */
  def get(key: K): Option[V] = {
    rootHash match {
      case Some(rootId) =>
        val keyNibbles = HexPrefix.bytesToNibbles(bytes = kSerializer.toBytes(key))
        val rootNode = getNode(rootId, dataSource)
        get(rootNode, keyNibbles).map(bytes => vSerializer.fromBytes(bytes))
      case None => None
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
  def put(key: K, value: V): MerklePatriciaTrie[K, V] = {
    val keyNibbles = HexPrefix.bytesToNibbles(bytes = kSerializer.toBytes(key))
    rootHash match {
      case Some(rootId) =>
        val root = getNode(rootId, dataSource)
        val NodeInsertResult(newRoot, nodesToRemoveFromStorage, nodesToUpdateInStorage) = put(root, keyNibbles, vSerializer.toBytes(value))
        val newRootHash = newRoot.hash(hashFn)
        val newSource = updateNodesInStorage(
          previousRootHash = getRootHash,
          newRootHash = newRootHash,
          newRoot = Some(newRoot),
          toRemove = nodesToRemoveFromStorage,
          toUpdate = nodesToUpdateInStorage,
          dataSource = dataSource,
          hashFn = hashFn)
        new MerklePatriciaTrie(Some(newRootHash), newSource, hashFn)
      case None =>
        val newRoot = LeafNode(keyNibbles, vSerializer.toBytes(value))
        val newRootHash = newRoot.hash(hashFn)
        new MerklePatriciaTrie(Some(newRootHash),
          updateNodesInStorage(getRootHash, newRootHash, Some(newRoot), Seq(), Seq(newRoot), dataSource, hashFn),
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
  def remove(key: K): MerklePatriciaTrie[K, V] = {
    rootHash match {
      case Some(rootId) =>
        val keyNibbles = HexPrefix.bytesToNibbles(bytes = kSerializer.toBytes(key))
        val root = getNode(rootId, dataSource)
        remove(root, keyNibbles) match {
          case NodeRemoveResult(true, Some(newRoot), nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
            val newRootHash = newRoot.hash(hashFn)
            val afterDeleteDataSource = updateNodesInStorage(
              previousRootHash = getRootHash,
              newRootHash = newRootHash,
              newRoot = Some(newRoot),
              toRemove = nodesToRemoveFromStorage,
              toUpdate = nodesToUpdateInStorage,
              dataSource = dataSource,
              hashFn = hashFn)
            new MerklePatriciaTrie(Some(newRootHash), afterDeleteDataSource, hashFn)
          case NodeRemoveResult(true, None, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
            val afterDeleteDataSource = updateNodesInStorage(
              previousRootHash = getRootHash,
              newRootHash = EmptyTrieHash,
              newRoot = None,
              toRemove = nodesToRemoveFromStorage,
              toUpdate = nodesToUpdateInStorage,
              dataSource = dataSource,
              hashFn = hashFn)
            new MerklePatriciaTrie(None, afterDeleteDataSource, hashFn)
          case NodeRemoveResult(false, _, _, _) => this
        }
      case None => this
    }
  }

  @tailrec
  private def get(node: Node, searchKey: Array[Byte]): Option[Array[Byte]] = node match {
    case LeafNode(key, value) =>
      if (key sameElements searchKey) Some(value) else None
    case extNode@ExtensionNode(sharedKey, next) =>
      val (commonKey, remainingKey) = searchKey.splitAt(sharedKey.length)
      if (searchKey.length >= sharedKey.length && (sharedKey sameElements commonKey)) {
        val nextNode = getNextNode(extNode, dataSource)
        get(nextNode, remainingKey)
      }
      else None
    case branch@BranchNode(children, terminator) =>
      if (searchKey.isEmpty) terminator
      else getChild(branch, searchKey(0), dataSource) match {
        case Some(child) => get(child, searchKey.slice(1, searchKey.length))
        case None => None
      }
  }

  private def put(node: Node, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    node match {
      case LeafNode(existingKey, storedValue) =>
        matchingLength(existingKey, searchKey) match {
          case ml if ml == existingKey.length && ml == searchKey.length =>
            // We are trying to insert a leaf node that has the same key as this one but different value so we need to
            // replace it
            val newLeafNode = LeafNode(existingKey, value)
            NodeInsertResult(
              newNode = newLeafNode,
              toDeleteFromStorage = Seq(node),
              toUpdateInStorage = Seq(newLeafNode)
            )
          case 0 =>
            // There is no common prefix between the node which means that we need to replace this leaf node
            val (temporalBranchNode, maybeNewLeaf) =
              if (existingKey.isEmpty) // This node has no key so it should be stored as branch's value
                BranchNode.withValueOnly(storedValue) -> None
              else {
                // The leaf should be put inside one of new branch nibbles
                val newLeafNode = LeafNode(existingKey.tail, storedValue)
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
              if (ml == existingKey.length) BranchNode.withValueOnly(storedValue)
              else LeafNode(existingKey.drop(ml), storedValue)
            val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage, toUpdateInStorage) = put(temporalNode, searchKeySuffix, value)
            val newExtNode = ExtensionNode(searchKeyPrefix, newBranchNode, hashFn)
            NodeInsertResult(
              newNode = newExtNode,
              toDeleteFromStorage = node +: toDeleteFromStorage,
              toUpdateInStorage = newExtNode +: toUpdateInStorage
            )
        }
      case extensionNode@ExtensionNode(sharedKey, next) =>
        matchingLength(sharedKey, searchKey) match {
          case 0 =>
            // There is no common prefix with the node which means we have to replace it for a branch node
            val sharedKeyHead = sharedKey(0)
            val (temporalBranchNode, maybeNewLeaf) = {
              // Direct extension, we just replace the extension with a branch
              if (sharedKey.length == 1) BranchNode.withSingleChild(sharedKeyHead, next, None) -> None
              else {
                // The new branch node will have an extension that replaces current one
                val newExtNode = ExtensionNode(sharedKey.tail, next)
                BranchNode.withSingleChild(sharedKeyHead, newExtNode, None, hashFn) -> Some(newExtNode)
              }
            }
            val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage, toUpdateInStorage) = put(temporalBranchNode, searchKey, value)
            NodeInsertResult(
              newNode = newBranchNode,
              toDeleteFromStorage = node +: toDeleteFromStorage,
              toUpdateInStorage = maybeNewLeaf.toList ++ toUpdateInStorage
            )
          case ml if ml == sharedKey.length =>
            // Current extension node's key is a prefix of the one being inserted, so we insert recursively on the extension's child
            val NodeInsertResult(newChild: BranchNode, toDeleteFromStorage, toUpdateInStorage) =
              put(getNextNode(extensionNode, dataSource), searchKey.drop(ml), value)
            val newExtNode = ExtensionNode(sharedKey, newChild, hashFn)
            NodeInsertResult(
              newNode = newExtNode,
              toDeleteFromStorage = node +: toDeleteFromStorage,
              toUpdateInStorage = newExtNode +: toUpdateInStorage
            )
          case ml =>
            // Partially shared prefix, we have to replace the node with an extension with the shared prefix
            val (sharedKeyPrefix, sharedKeySuffix) = sharedKey.splitAt(ml)
            val temporalExtensionNode = ExtensionNode(sharedKeySuffix, next)
            val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage, toUpdateInStorage) = put(temporalExtensionNode, searchKey.drop(ml), value)
            val newExtNode = ExtensionNode(sharedKeyPrefix, newBranchNode, hashFn)
            NodeInsertResult(
              newNode = newExtNode,
              toDeleteFromStorage = node +: toDeleteFromStorage,
              toUpdateInStorage = newExtNode +: toUpdateInStorage
            )
        }
      case branchNode@BranchNode(children, optStoredValue) =>
        if (searchKey.isEmpty) {
          // The key is empty, the branch node should now be a terminator node with the new value asociated with it
          val newBranchNode = BranchNode(children, Some(value))
          NodeInsertResult(
            newNode = newBranchNode,
            toDeleteFromStorage = Seq(node),
            toUpdateInStorage = Seq(newBranchNode)
          )
        }
        else {
          // Non empty key, we need to insert the value in the correct branch node's child
          val searchKeyHead: Int = searchKey(0)
          val searchKeyRemaining = searchKey.tail
          children(searchKeyHead) match {
            case Some(_) =>
              // The associated child is not empty, we recursively insert in that child
              val NodeInsertResult(changedChild, toDeleteFromStorage, toUpdateInStorage) =
                put(getChild(branchNode, searchKeyHead, dataSource).get, searchKeyRemaining, value)
              val newBranchNode = branchNode.updateChild(searchKeyHead, changedChild, hashFn)
              NodeInsertResult(
                newNode = newBranchNode,
                toDeleteFromStorage = node +: toDeleteFromStorage,
                toUpdateInStorage = newBranchNode +: toUpdateInStorage
              )
            case None =>
              // The associated child is empty, we just replace it with a leaf
              val newLeafNode = LeafNode(searchKeyRemaining, value)
              val newBranchNode = branchNode.updateChild(searchKeyHead, newLeafNode, hashFn)
              NodeInsertResult(
                newNode = newBranchNode,
                toDeleteFromStorage = Seq(node),
                toUpdateInStorage = Seq(newLeafNode, newBranchNode)
              )
          }
        }
    }
  }

  private def remove(node: Node, searchKey: Array[Byte]): NodeRemoveResult = (node, searchKey.isEmpty) match {
    // They key matches a branch node but it's value doesn't match the key
    case (BranchNode(children, None), true) => NodeRemoveResult(hasChanged = false, maybeNewChild = None)
    // We want to delete Branch node value
    case (BranchNode(children, _), true) =>
      // We need to remove old node and fix it because we removed the value
      val fixedNode = fix(BranchNode(children, None), dataSource, Seq(), hashFn)
      NodeRemoveResult(hasChanged = true, maybeNewChild = Some(fixedNode), toDeleteFromStorage = Seq(node), toUpdateInStorage = Seq(fixedNode))
    case (branchNode@BranchNode(children, optStoredValue), false) =>
      // We might be trying to remove a node that's inside one of the 16 mapped nibbles
      val searchKeyHead = searchKey(0)
      getChild(branchNode, searchKeyHead, dataSource) match {
        // Child has been found so we try to remove it
        case Some(child) =>
          remove(child, searchKey.tail) match {
            case NodeRemoveResult(true, maybeNewChild, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
              // Something changed in a child so we need to fix
              val nodeToFix = maybeNewChild match {
                case Some(newChild) => branchNode.updateChild(searchKeyHead, newChild, hashFn)
                case None => BranchNode(children.updated(searchKeyHead, None), optStoredValue)
              }
              val fixedNode = fix(nodeToFix, dataSource, nodesToUpdateInStorage, hashFn)
              NodeRemoveResult(
                hasChanged = true,
                maybeNewChild = Some(fixedNode),
                toDeleteFromStorage = node +: nodesToRemoveFromStorage,
                toUpdateInStorage = fixedNode +: nodesToUpdateInStorage)
            // No removal made on children, so we return without any change
            case NodeRemoveResult(false, _, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
              NodeRemoveResult(
                hasChanged = false,
                maybeNewChild = None,
                toDeleteFromStorage = nodesToRemoveFromStorage,
                toUpdateInStorage = nodesToUpdateInStorage)
          }
        // Child not found in this branch node, so key is not present
        case None => NodeRemoveResult(hasChanged = false, maybeNewChild = None)
      }
    case (LeafNode(existingKey, storedValue), _) =>
      if (existingKey sameElements searchKey) {
        // We found the node to delete
        NodeRemoveResult(hasChanged = true, maybeNewChild = None, toDeleteFromStorage = Seq(node))
      }
      else NodeRemoveResult(hasChanged = false, maybeNewChild = None)
    case (extensionNode@ExtensionNode(sharedKey, next), _) =>
      val cp = matchingLength(sharedKey, searchKey)
      if (cp == sharedKey.length) {
        // A child node of this extension is removed, so move forward
        remove(getNextNode(extensionNode, dataSource), searchKey.drop(cp)) match {
          case NodeRemoveResult(true, maybeNewChild, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
            // If we changed the child, we need to fix this extension node
            maybeNewChild match {
              case Some(newChild) =>
                val toFix = ExtensionNode(sharedKey, newChild, hashFn)
                val fixedNode = fix(toFix, dataSource, nodesToUpdateInStorage, hashFn)
                NodeRemoveResult(
                  hasChanged = true,
                  maybeNewChild = Some(fixedNode),
                  toDeleteFromStorage = node +: nodesToRemoveFromStorage,
                  toUpdateInStorage = fixedNode +: nodesToUpdateInStorage)
              case None => throw MPTException("A trie with newRoot extension should have at least 2 values stored")
            }
          case NodeRemoveResult(false, _, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
            NodeRemoveResult(
              hasChanged = false,
              maybeNewChild = None,
              toDeleteFromStorage = nodesToRemoveFromStorage,
              toUpdateInStorage = nodesToUpdateInStorage)
        }
      }
      else NodeRemoveResult(hasChanged = false, maybeNewChild = Some(node))
  }

  /**
    * Given a node which may be in an invalid state, fix it such that it is then in a valid state. Invalid state means:
    *   - Branch node where there is only a single entry;
    *   - Extension node followed by anything other than a Branch node.
    *
    * @param node that may be in an invalid state.
    * @param dataSource to obtain the nodes referenced in the node that may be in an invalid state.
    * @param notStoredYet to obtain the nodes referenced in the node that may be in an invalid state,
    *                     if they were not yet inserted into the dataSource.
    * @param hashFn
    * @return fixed node.
    * @throws MPTException if there is any inconsistency in how the trie is build.
    */
  @tailrec
  private def fix(node: Node, dataSource: DataSource, notStoredYet: Seq[Node], hashFn: HashFn): Node = node match {
    case BranchNode(children, optStoredValue) =>
      val usedIndexes = children.indices.foldLeft[Seq[Int]](Seq.empty) { (acc, i) =>
        if (children(i).isDefined) acc :+ i else acc
      }
      (usedIndexes, optStoredValue) match {
        case (Nil, None) => throw MPTException("Branch with no subvalues")
        case (index :: Nil, None) =>
          val temporalExtNode = ExtensionNode(Array[Byte](index.toByte), children(index).get)
          fix(temporalExtNode, dataSource, notStoredYet, hashFn)
        case (Nil, Some(value)) => LeafNode(Array.emptyByteArray, value)
        case _ => node
      }
    case extensionNode@ExtensionNode(sharedKey, next) =>
      val nextNode = extensionNode.next match {
        case Left(nextHash) =>
          // If the node is not in the extension node then it might be a node to be inserted at the end of this remove
          // so we search in this list too
          notStoredYet.find(n => n.hash(hashFn) sameElements nextHash) match {
            case Some(nextNodeOnList) => nextNodeOnList
            case None => getNextNode(extensionNode, dataSource) // We search for the node in the db
          }
        case Right(nextNodeOnExt) => nextNodeOnExt
      }
      val newNode = nextNode match {
        // Compact Two extensions into one
        case ExtensionNode(subSharedKey, subNext) => ExtensionNode(sharedKey ++ subSharedKey, subNext)
        // Compact the extension and the leaf into the same leaf node
        case LeafNode(subRemainingKey, subValue) => LeafNode(sharedKey ++ subRemainingKey, subValue)
        // It's ok
        case BranchNode(subChildren, subOptValue) => node
      }
      newNode
    case _ => node
  }
}

trait ByteArraySerializable[T] {
  def toBytes(input: T): Array[Byte]

  def fromBytes(bytes: Array[Byte]): T
}

trait DataSource {

  type Key = Array[Byte]
  type Value = Array[Byte]

  def get(key: Key): Option[Value]

  def update(rootHash: Array[Byte], toRemove: Seq[Key], toUpdate: Seq[(Key, Value)]): DataSource
}

/**
  * Trie elements
  */
private sealed trait Node {

  import MerklePatriciaTrie._

  private var hashedCache: Option[Array[Byte]] = None

  lazy val encode: Array[Byte] = encodeRLP[Node](this)

  def hash(hashFn: HashFn): Array[Byte] = hashedCache.getOrElse {
    val hashed = hashFn(encode)
    hashedCache = Some(hashed)
    hashed
  }

  def capped(hashFn: HashFn): Array[Byte] = {
    val encoded = encode
    if (encoded.length < 32) encoded else hash(hashFn)
  }
}

private case class LeafNode(key: Array[Byte], value: Array[Byte]) extends Node

private case class ExtensionNode(sharedKey: Array[Byte], next: Either[Array[Byte], Node]) extends Node

private object ExtensionNode {
  def apply(sharedKey: Array[Byte], next: Node, hashFn: HashFn): ExtensionNode = {
    val nextCapped = next.capped(hashFn)
    ExtensionNode(sharedKey, if (nextCapped.length == 32) Left(nextCapped) else Right(next))
  }
}

private case class BranchNode(children: Seq[Option[Either[Array[Byte], Node]]], terminator: Option[Array[Byte]]) extends Node {
  def updateChild(childIndex: Int, childNode: Node, hashFn: HashFn): BranchNode = {
    val childCapped = childNode.capped(hashFn)
    BranchNode(children.updated(childIndex, Some(if (childCapped.length == 32) Left(childCapped) else Right(childNode))), terminator)
  }
}

private object BranchNode {
  private val emptyChildren: Seq[Option[Either[Array[Byte], Node]]] = Array.fill(MerklePatriciaTrie.ListSize - 1)(None)

  def withValueOnly(terminator: Array[Byte]): BranchNode =
    BranchNode(emptyChildren, Some(terminator))

  def withSingleChild(position: Byte, child: Node, terminator: Option[Array[Byte]], hashFn: HashFn): BranchNode = {
    val childCapped = child.capped(hashFn)
    BranchNode(emptyChildren.updated(position, Some(if (childCapped.length == 32) Left(childCapped) else Right(child))), terminator)
  }

  def withSingleChild(position: Byte, child: Either[Array[Byte], Node], terminator: Option[Array[Byte]]): BranchNode =
    BranchNode(emptyChildren.updated(position, Some(child)), terminator)
}

private case class NodeInsertResult(newNode: Node,
                                    toDeleteFromStorage: Seq[Node] = Seq(),
                                    toUpdateInStorage: Seq[Node] = Seq())

private case class NodeRemoveResult(hasChanged: Boolean, maybeNewChild: Option[Node],
                                    toDeleteFromStorage: Seq[Node] = Seq(),
                                    toUpdateInStorage: Seq[Node] = Seq())

