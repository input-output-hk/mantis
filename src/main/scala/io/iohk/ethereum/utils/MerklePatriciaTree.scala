package io.iohk.ethereum.utils

import akka.util.ByteString
import io.iohk.ethereum.utils.MerklePatriciaTree.HashFn
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => encodeRLP, decode => decodeRLP, _}

import scala.annotation.tailrec

object MerklePatriciaTree {
  type HashFn = Array[Byte] => Array[Byte]

  val PairSize: Byte = 2
  val ListSize: Byte = 17

  def apply[K, V](source: DataSource, hashFn: HashFn)
                 (implicit kSerializer: ByteArraySerializable[K], vSerializer: ByteArraySerializable[V])
  : MerklePatriciaTree[K, V] = MerklePatriciaTree[K, V](None, source, hashFn)(kSerializer, vSerializer)

  private[utils] def getNode(nodeId: Array[Byte], source: DataSource)(implicit nodeDec: RLPDecoder[Node]): Option[Node] =
    tryGetNode(nodeId, source).flatMap {
      arr: Array[Byte] => Some(decodeRLP[Node](arr))
    }

  private def tryGetNode[K <: ByteArraySerializable[K], V <: ByteArraySerializable[V]]
  (key: Array[Byte], source: DataSource): Option[Array[Byte]] = if (key.length < 32) Some(key) else source.get(key)

  private def matchingLength(bytes1: Array[Byte], bytes2: Array[Byte]): Int = bytes1.zip(bytes2).takeWhile(t => t._1 == t._2).length

  private def updateNodeInStorage(node: Node, source: DataSource, hashFn: HashFn): DataSource = source.update(node.hash(hashFn), node.encode)

  private def replaceRoot(rootHash: Array[Byte], newRoot: Node, source: DataSource, hashFn: HashFn): DataSource =
    source.update(Seq(rootHash), Seq(newRoot.hash(hashFn) -> newRoot.encode))

  private def updateNodesInStorage(toRemove: Seq[Node], toUpdate: Seq[Node], dataSource: DataSource, hashFn: HashFn): DataSource = {
    val toBeRemoved = toRemove.map(node => node.capped(hashFn)).filter(_.length == 32)
    val toBeUpdated = toUpdate.filter(n => n.capped(hashFn).length == 32).map(node => node.hash(hashFn) -> node.encode)
    dataSource.update(toBeRemoved, toBeUpdated)
  }

  private[utils] def getNextNode(extensionNode: ExtensionNode, dataSource: DataSource)(implicit nodeDec: RLPDecoder[Node]): Option[Node] = extensionNode.next match {
    case Right(node) => Some(node)
    case Left(hash) => MerklePatriciaTree.getNode(hash, dataSource)
  }

  private[utils] def getNextNodeId(extensionNode: ExtensionNode, hashFn: HashFn): Array[Byte] = extensionNode.next match {
    case Right(node) => node.capped(hashFn)
    case Left(hash) => hash
  }

  private[utils] def getChild(branchNode: BranchNode, pos: Int, dataSource: DataSource)(implicit nodeDec: RLPDecoder[Node]): Option[Node] = branchNode.children(pos) match {
    case Some(Right(node)) => Some(node)
    case Some(Left(hash)) => MerklePatriciaTree.getNode(hash, dataSource)
    case None => None
  }

  private[utils] def getChildId(branchNode: BranchNode, pos: Int, hashFn: HashFn): Option[Array[Byte]] = branchNode.children(pos) match {
    case Some(Right(node)) => Some(node.capped(hashFn))
    case Some(Left(hash)) => Some(hash)
    case None => None
  }

  implicit val defaultByteArraySerializable = new ByteArraySerializable[Array[Byte]] {
    override def toBytes(input: Array[Byte]): Array[Byte] = input

    override def fromBytes(bytes: Array[Byte]): Array[Byte] = bytes
  }

  /**
    * Implicits
    */

  // FIXME Improve this visibility
  private[utils] implicit val nodeEncDec = new RLPDecoder[Node] with RLPEncoder[Node] {
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
      case RLPList(items@_*) if items.size == MerklePatriciaTree.ListSize =>
        val parsedChildren: Seq[Option[Either[Array[Byte], Node]]] = items.init.map {
          case list: RLPList => Some(Right(this.decode(list)))
          case RLPValue(bytes) =>
            if (bytes.isEmpty) None
            else Some(Left(bytes))
        }
        val terminatorAsArray: Array[Byte] = items.last
        BranchNode(children = parsedChildren, terminator = if (terminatorAsArray.isEmpty) None else Some(terminatorAsArray))
      case RLPList(items@_*) if items.size == MerklePatriciaTree.PairSize =>
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

case class MerklePatriciaTree[K, V](rootHash: Option[Array[Byte]], dataSource: DataSource, hashFn: HashFn)
                                   (implicit kSerializer: ByteArraySerializable[K], vSerializer: ByteArraySerializable[V]) {

  import MerklePatriciaTree._

  def get(key: K): Option[V] = {
    rootHash match {
      case Some(rootId) =>
        val keyNibbles = HexPrefix.bytesToNibbles(bytes = kSerializer.toBytes(key))
        get(rootId, keyNibbles).map(bytes => vSerializer.fromBytes(bytes))
      case None => None
    }
  }

  def put(key: K, value: V): MerklePatriciaTree[K, V] = {
    val keyNibbles = HexPrefix.bytesToNibbles(bytes = kSerializer.toBytes(key))
    rootHash match {
      case Some(rootId) =>
        val NodeInsertResult(newRoot, newSource) = put(rootId, keyNibbles, vSerializer.toBytes(value))
        MerklePatriciaTree(Some(newRoot.hash(hashFn)),
          replaceRoot(rootId, newRoot, newSource, hashFn),
          hashFn)
      case None =>
        val root = LeafNode(keyNibbles, vSerializer.toBytes(value))
        MerklePatriciaTree(Some(root.hash(hashFn)),
          updateNodeInStorage(root, dataSource, hashFn),
          hashFn)
    }
  }

  def remove(key: K): MerklePatriciaTree[K, V] = {
    rootHash match {
      case Some(rootId) =>
        val keyNibbles = HexPrefix.bytesToNibbles(bytes = kSerializer.toBytes(key))
        remove(rootId, keyNibbles) match {
          case NodeRemoveResult(true, Some(newRoot), afterDeleteDataSource) =>
            MerklePatriciaTree(Some(newRoot.hash(hashFn)), replaceRoot(rootId, newRoot, afterDeleteDataSource, hashFn), hashFn)
          case NodeRemoveResult(true, None, newDataSource) =>
            MerklePatriciaTree(None, newDataSource, hashFn)
          case NodeRemoveResult(false, _, _) => this
        }
      case None => this
    }
  }

  /* Private functions */
  @tailrec
  private def get(nodeId: Array[Byte], searchKey: Array[Byte]): Option[Array[Byte]] = {
    val optNode = getNode(nodeId, dataSource)
    optNode match {
      case Some(node) => node match {
        case LeafNode(key, value) =>
          if (key sameElements searchKey) Some(value) else None
        case extNode@ExtensionNode(sharedKey, next) =>
          val (commonKey, remainingKey) = searchKey.splitAt(sharedKey.length)
          if (searchKey.length >= sharedKey.length && (sharedKey sameElements commonKey))
            get(getNextNodeId(extNode, hashFn), remainingKey)
          else None
        case branch@BranchNode(children, terminator) =>
          if (searchKey.isEmpty) terminator
          else getChildId(branch, searchKey(0), hashFn) match {
            case Some(childId) => get(childId, searchKey.slice(1, searchKey.length))
            case None => None
          }
      }
      case None => None
    }
  }

  private def put(nodeId: Array[Byte], searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    getNode(nodeId, dataSource) match {
      case Some(node) => put(node, searchKey, value)
      case None => throw new Exception("Node not found")
    }
  }

  private def put(node: Node, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    node match {
      case LeafNode(existingKey, storedValue) =>
        matchingLength(existingKey, searchKey) match {
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
            val NodeInsertResult(newBranchNode, dataSourceAfterPut) = put(temporalBranchNode, searchKey, value)
            NodeInsertResult(
              newNode = newBranchNode,
              dataSource = updateNodesInStorage(Seq(node), Seq(newBranchNode) ++ maybeNewLeaf.toList, dataSourceAfterPut, hashFn)
            )
          case ml if ml == existingKey.length && ml == searchKey.length =>
            // We are trying to insert a leaf node that has the same key as this one but different value so we need to
            // replace it
            val newLeafNode = LeafNode(existingKey, value)
            NodeInsertResult(
              newNode = newLeafNode,
              dataSource = updateNodesInStorage(Seq(node), Seq(newLeafNode), dataSource, hashFn)
            )
          case ml if ml == existingKey.length =>
            // Current leaf's key is exactly the common prefix so we need to replace this node with an extension
            // and a branch to continue inserting our key
            val (searchKeyPrefix, searchKeySuffix) = searchKey.splitAt(ml)
            val temporalBranchNode = BranchNode.withValueOnly(storedValue)
            val NodeInsertResult(newBranchNode: BranchNode, dataSourceAfterPut) = put(temporalBranchNode, searchKeySuffix, value)
            val newExtNode = ExtensionNode(searchKeyPrefix, newBranchNode, hashFn)
            NodeInsertResult(
              newNode = newExtNode,
              dataSource = updateNodesInStorage(Seq(node), Seq(newBranchNode, newExtNode), dataSourceAfterPut, hashFn)
            )
          case ml =>
            // Partially shared prefix, we replace the leaf with an extension and a branch node
            val (searchKeyPrefix, searchKeySuffix) = searchKey.splitAt(ml)
            val temporalLeafNode = LeafNode(existingKey.drop(ml), storedValue)
            val NodeInsertResult(newBranchNode: BranchNode, dataSourceAfterPut) = put(temporalLeafNode, searchKeySuffix, value)
            val newExtNode = ExtensionNode(searchKeyPrefix, newBranchNode, hashFn)
            NodeInsertResult(
              newNode = newExtNode,
              dataSource = updateNodesInStorage(Seq(node), Seq(newBranchNode, newExtNode), dataSourceAfterPut, hashFn)
            )
        }
      case extensionNode@ExtensionNode(sharedKey, next) =>
        matchingLength(sharedKey, searchKey) match {
          case 0 =>
            // There is no common prefix with the node which means we have to replace it for a branch node
            val sharedKeyHead = sharedKey(0)
            val (temporalBranchNode, maybeNewLeaf) = {
              // Direct extension, we just replace the extension with a branch
              if (sharedKey.length == 1){
                BranchNode.withSingleChild(sharedKeyHead, next, None) -> None
              }
              else {
                // The new branch node will have an extension that replaces current one
                val newExtNode = ExtensionNode(sharedKey.tail, next)
                BranchNode.withSingleChild(sharedKeyHead, newExtNode, None, hashFn) -> Some(newExtNode)
              }
            }
            val NodeInsertResult(newBranchNode, dataSourceAfterPut) = put(temporalBranchNode, searchKey, value)
            NodeInsertResult(
              newNode = newBranchNode,
              dataSource = updateNodesInStorage(Seq(node), Seq(newBranchNode) ++ maybeNewLeaf.toList, dataSourceAfterPut, hashFn)
            )
          case ml if ml == sharedKey.length =>
            // Current extension node's key is a prefix of the one being inserted, so we insert recursively on the extension's child
            val NodeInsertResult(newChild: BranchNode, dataSourceAfterPut) = put(getNextNodeId(extensionNode, hashFn), searchKey.drop(ml), value)
            val newExtNode = ExtensionNode(sharedKey, newChild, hashFn)
            NodeInsertResult(
              newNode = newExtNode,
              dataSource = updateNodesInStorage(Seq(node), Seq(newExtNode), dataSourceAfterPut, hashFn)
            )
          case ml =>
            // Partially shared prefix, we have to replace the node with an extension with the shared prefix
            val (sharedKeyPrefix, sharedKeySuffix) = sharedKey.splitAt(ml)
            val temporalExtensionNode = ExtensionNode(sharedKeySuffix, next)
            val NodeInsertResult(newBranchNode: BranchNode, dataSourceAfterPut) = put(temporalExtensionNode, searchKey.drop(ml), value)
            val newExtNode = ExtensionNode(sharedKeyPrefix, newBranchNode, hashFn)
            NodeInsertResult(
              newNode = newExtNode,
              dataSource = updateNodesInStorage(Seq(node), Seq(newBranchNode, newExtNode), dataSourceAfterPut, hashFn)
            )
        }
      case branchNode@BranchNode(children, optStoredValue) =>
        if (searchKey.isEmpty) {
          // The key is empty, the branch node should now be a terminator node with the new value asociated with it
          val newBranchNode = BranchNode(children, Some(value))
          NodeInsertResult(
            newNode = newBranchNode,
            dataSource = updateNodesInStorage(Seq(node), Seq(newBranchNode), dataSource, hashFn)
          )
        }
        else {
          // Non empty key, we need to insert the value in the correct branch node's child
          val searchKeyHead: Int = searchKey(0)
          val searchKeyRemaining = searchKey.tail
          children(searchKeyHead) match {
            case Some(_) =>
              // The associated child is not empty, we recursively insert in that child
              val NodeInsertResult(changedChild, dataSourceAfterPut) = put(getChildId(branchNode, searchKeyHead, hashFn).get, searchKeyRemaining, value)
              val newBranchNode = branchNode.updateChild(searchKeyHead, changedChild, hashFn)
              NodeInsertResult(
                newNode = newBranchNode,
                dataSource = updateNodesInStorage(Seq(node), Seq(newBranchNode), dataSourceAfterPut, hashFn)
              )
            case None =>
              // The associated child is empty, we just replace it with a leaf
              val newLeafNode = LeafNode(searchKeyRemaining, value)
              val newBranchNode = branchNode.updateChild(searchKeyHead, newLeafNode, hashFn)
              NodeInsertResult(
                newNode = newBranchNode,
                dataSource = updateNodesInStorage(Seq(node), Seq(newLeafNode, newBranchNode), dataSource, hashFn)
              )
          }
        }
    }
  }

  private def remove(nodeId: Array[Byte], searchKey: Array[Byte]): NodeRemoveResult = {
    getNode(nodeId, dataSource) match {
      case Some(node) => remove(node, searchKey)
      case None => throw new Exception("Node not found")
    }
  }

  private def remove(node: Node, searchKey: Array[Byte]): NodeRemoveResult = (node, searchKey.isEmpty) match {
    // They key matches a branch node but it's value doesn't match the key
    case (BranchNode(children, None), true) => NodeRemoveResult(hasChanged = false, maybeNewChild = None, dataSource = dataSource)
    // We want to delete Branch node value
    case (BranchNode(children, _), true) =>
      // We need to remove old node and fix it because we removed the value
      val fixedNode = fix(BranchNode(children, None), dataSource)
      val afterFixedSave = updateNodesInStorage(Seq(node), Seq(fixedNode), dataSource, hashFn)
      NodeRemoveResult(hasChanged = true, maybeNewChild = Some(fixedNode), dataSource = afterFixedSave)
    case (branchNode@BranchNode(children, optStoredValue), false) =>
      // We might be trying to remove a node that's inside one of the 16 mapped nibbles
      val searchKeyHead = searchKey(0)
      getChildId(branchNode, searchKeyHead, hashFn) match {
        // Child has been found so we try to remove it
        case Some(child) =>
          remove(child, searchKey.tail) match {
            case NodeRemoveResult(true, maybeNewChild, afterChildRemovalDataSource) =>
              // Something changed in a child so we need to fix
              val nodeToFix = maybeNewChild match {
                case Some(newChild) => branchNode.updateChild(searchKeyHead, newChild, hashFn)
                case None => BranchNode(children.updated(searchKeyHead, None), optStoredValue)
              }
              val fixedNode = fix(nodeToFix, afterChildRemovalDataSource)
              val afterFixedSave = updateNodesInStorage(Seq(node), Seq(fixedNode), afterChildRemovalDataSource, hashFn)
              NodeRemoveResult(hasChanged = true, maybeNewChild = Some(fixedNode), dataSource = afterFixedSave)
            // No removal made on children, so we return without any change
            case NodeRemoveResult(false, _, _) => NodeRemoveResult(hasChanged = false, maybeNewChild = None, dataSource = dataSource)
          }
        // Child not found in this branch node, so key is not present
        case None => NodeRemoveResult(hasChanged = false, maybeNewChild = None, dataSource)
      }
    case (LeafNode(existingKey, storedValue), _) =>
      if (existingKey sameElements searchKey) {
        // We found the node to delete
        val newDataSource = updateNodesInStorage(Seq(node), Seq(), dataSource, hashFn)
        NodeRemoveResult(hasChanged = true, maybeNewChild = None, dataSource = newDataSource)
      }
      else NodeRemoveResult(hasChanged = false, maybeNewChild = None, dataSource = dataSource)
    case (extensionNode@ExtensionNode(sharedKey, next), _) =>
      val cp = matchingLength(sharedKey, searchKey)
      if (cp == sharedKey.length) {
        // A child node of this extension is removed, so move forward
        remove(getNextNodeId(extensionNode, hashFn), searchKey.drop(cp)) match {
          case NodeRemoveResult(true, maybeNewChild, afterChildRemovalDataSource) =>
            // If we changed the child, we need to fix this extension node
            maybeNewChild match {
              case Some(newChild) =>
                val toFix = ExtensionNode(sharedKey, newChild, hashFn)
                val fixedNode = fix(toFix, afterChildRemovalDataSource)
                val afterFixedInsertion = updateNodesInStorage(Seq(node), Seq(fixedNode), afterChildRemovalDataSource, hashFn)
                NodeRemoveResult(hasChanged = true, maybeNewChild = Some(fixedNode), dataSource = afterFixedInsertion)
              case None => throw new Exception("A trie with root extension should have at least 2 values stored")
            }
          case NodeRemoveResult(false, _, _) => NodeRemoveResult(hasChanged = false, maybeNewChild = None, dataSource = dataSource)
        }
      }
      else NodeRemoveResult(hasChanged = false, maybeNewChild = Some(node), dataSource = dataSource)
  }

  /**
    * Given a node which may be in an _invalid state_, fix it such that it is then in a valid state.
    *
    * _invalid state_ means:
    *   - Branch node where there is only a single entry;
    *   - Extension node followed by anything other than a Branch node.
    *
    */
  @tailrec
  private def fix(node: Node, dataSource: DataSource): Node = node match {
    case BranchNode(children, optStoredValue) =>
      val usedIndexes = children.indices.foldLeft[Seq[Int]](Seq.empty) { (acc, i) =>
        if (children(i).isDefined) acc :+ i else acc
      }
      (usedIndexes, optStoredValue) match {
        case (Nil, None) => throw new Exception("Branch with no subvalues")
        case (index :: Nil, None) =>
          val temporalExtNode = ExtensionNode(Array[Byte](index.toByte), children(index).get)
          fix(temporalExtNode, dataSource)
        case (Nil, Some(value)) => LeafNode(Array.emptyByteArray, value)
        case _ => node
      }
    case extensionNode@ExtensionNode(sharedKey, next) =>
      getNextNode(extensionNode, dataSource) match {
        case Some(nextNode) =>
          val newNode = nextNode match {
            // Compact Two extensions into one
            case ExtensionNode(subSharedKey, subNext) => ExtensionNode(sharedKey ++ subSharedKey, subNext)
            // Compact the extension and the leaf into the same leaf node
            case LeafNode(subRemainingKey, subValue) => LeafNode(sharedKey ++ subRemainingKey, subValue)
            // It's ok
            case BranchNode(subChildren, subOptValue) => node
          }
          newNode
        case None => throw new Exception("Node not found, can't apply fix")
      }
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

  def update(key: Key, value: Value): DataSource

  def update(toInsert: Seq[(Key, Value)]): DataSource

  def remove(toRemove: Key): DataSource

  def update(toRemove: Seq[Key], toUpdate: Seq[(Key, Value)]): DataSource
}

/**
  * Trie elements
  */
private sealed trait Node {

  import MerklePatriciaTree._

  private[this] var encodedCache: Option[Array[Byte]] = None
  private[this] var hashedCache: Option[Array[Byte]] = None

  def encode: Array[Byte] = encodedCache.getOrElse {
    val encoded = encodeRLP[Node](this)
    encodedCache = Some(encoded)
    encoded
  }

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

private object ExtensionNode{
  def apply(sharedKey: Array[Byte], next: Node, hashFn: HashFn): ExtensionNode = {
    val nextCapped = next.capped(hashFn)
    ExtensionNode(sharedKey, if(nextCapped.length == 32) Left(nextCapped) else Right(next))
  }
}

private case class BranchNode(children: Seq[Option[Either[Array[Byte], Node]]], terminator: Option[Array[Byte]]) extends Node{
  def updateChild(childIndex: Int, childNode: Node, hashFn: HashFn): BranchNode = {
    val childCapped = childNode.capped(hashFn)
    BranchNode(children.updated(childIndex, Some(if(childCapped.length == 32) Left(childCapped) else Right(childNode))), terminator)
  }
}

private object BranchNode {
  private val emptyChildren: Seq[Option[Either[Array[Byte], Node]]] = Array.fill(MerklePatriciaTree.ListSize - 1)(None)

  def withValueOnly(terminator: Array[Byte]): BranchNode =
    BranchNode(emptyChildren, Some(terminator))

  def withSingleChild(position: Byte, child: Node, terminator: Option[Array[Byte]], hashFn: HashFn): BranchNode = {
    val childCapped = child.capped(hashFn)
    BranchNode(emptyChildren.updated(position, Some(if(childCapped.length == 32) Left(childCapped) else Right(child))), terminator)
  }

  def withSingleChild(position: Byte, child: Either[Array[Byte], Node], terminator: Option[Array[Byte]]): BranchNode =
    BranchNode(emptyChildren.updated(position, Some(child)), terminator)
}

private case class NodeInsertResult(newNode: Node, dataSource: DataSource)

private case class NodeRemoveResult(hasChanged: Boolean, maybeNewChild: Option[Node], dataSource: DataSource)

