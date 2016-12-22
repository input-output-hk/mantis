package io.iohk.ethereum.utils

import akka.util.ByteString
import io.iohk.ethereum.utils.MerklePatriciaTree.HashFn
import io.iohk.ethereum.utils.RLPImplicits._

import scala.annotation.tailrec

object MerklePatriciaTree {
  type HashFn = Array[Byte] => Array[Byte]
  //FIXME: Start using them
  type KeyType = ByteString
  type NodeId = Array[Byte]

  val PairSize: Byte = 2
  val ListSize: Byte = 17

  def apply[K, V](source: DataSource, hashFn: HashFn)
                 (implicit kSerializer: ByteArraySerializable[K], vSerializer: ByteArraySerializable[V])
  : MerklePatriciaTree[K, V] = MerklePatriciaTree[K, V](None, source, hashFn)(kSerializer, vSerializer)

  private def getNode(nodeId: Array[Byte], source: DataSource)(implicit nodeDec: RLPDecoder[Node]): Option[Node] =
    tryGetNode(nodeId, source).flatMap {
      arr: Array[Byte] => Some(RLP.decode[Node](arr))
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

  implicit val defaultByteArraySerializable = new ByteArraySerializable[Array[Byte]] {
    override def toBytes(input: Array[Byte]): Array[Byte] = input

    override def fromBytes(bytes: Array[Byte]): Array[Byte] = bytes
  }

  /**
    * Implicits
    */
  private implicit val branchNodeEncDec = new RLPEncoder[BranchNode] with RLPDecoder[BranchNode] {
    override def encode(obj: BranchNode): RLPEncodeable = new RLPList {
      override def items: Seq[RLPEncodeable] = (obj.terminator +: obj.children).map { item: Option[Array[Byte]] =>
        new RLPValue {
          override def bytes: ByteString = ByteString(item.getOrElse(Array[Byte]()))
        }
      }
    }

    override def decode(rlp: RLPEncodeable): BranchNode = rlp match {
      case l: RLPList =>
        val parsed: Seq[Option[Array[Byte]]] = l.items.map { i =>
          val asArray: Array[Byte] = i
          if (asArray.isEmpty) None else Some(asArray)
        }
        BranchNode(children = parsed.tail, terminator = parsed.head)
      case _ => throw new RuntimeException("Invalid Branch Node")
    }

    implicit def nodeToEnc(branchNode: BranchNode): RLPEncodeable = encode(branchNode)
  }

  private implicit val extensionNodeEncDec = new RLPEncoder[ExtensionNode] with RLPDecoder[ExtensionNode] {
    override def encode(obj: ExtensionNode): RLPEncodeable = new RLPList {
      override def items: Seq[RLPEncodeable] = Seq(HexPrefix.encode(nibbles = obj.sharedKey, t = false), obj.next)
    }

    override def decode(rlp: RLPEncodeable): ExtensionNode = rlp match {
      case l: RLPList => ExtensionNode(l.items.head, l.items(1))
      case _ => throw new RuntimeException("Invalid Extension Node")
    }

    implicit def nodeToEnc(extensionNode: ExtensionNode): RLPEncodeable = encode(extensionNode)
  }

  private implicit val leafNodeEncDec = new RLPEncoder[LeafNode] with RLPDecoder[LeafNode] {
    override def encode(obj: LeafNode): RLPEncodeable = new RLPList {
      override def items: Seq[RLPEncodeable] = Seq(HexPrefix.encode(nibbles = obj.key, t = true), obj.value)
    }

    override def decode(rlp: RLPEncodeable): LeafNode = rlp match {
      case l: RLPList => LeafNode(l.items.head, l.items(1))
      case _ => throw new RuntimeException("Invalid Extension Node")
    }

    implicit def nodeToEnc(leafNode: LeafNode): RLPEncodeable = encode(leafNode)
  }

  // FIXME Improve this visibility
  private[utils] implicit val nodeEncoder = new RLPEncoder[Node] with RLPDecoder[Node] {
    override def encode(obj: Node): RLPEncodeable = obj match {
      case l: LeafNode => l
      case e: ExtensionNode => e
      case b: BranchNode => b
    }

    override def decode(rlp: RLPEncodeable): Node = rlp match {
      case l: RLPList if l.items.size == MerklePatriciaTree.ListSize => l: BranchNode
      case l: RLPList if l.items.size == MerklePatriciaTree.PairSize =>
        val (key, isLeaf) = HexPrefix.decode(l.items.head)
        if (isLeaf) LeafNode(key, l.items(1))
        else ExtensionNode(key, l.items(1))
      case _ => throw new RuntimeException("Invalid Node")
    }
  }

  private implicit def encToNode(rLPEncodeable: RLPList): BranchNode = branchNodeEncDec.decode(rLPEncodeable)
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
        case ExtensionNode(sharedKey, next) =>
          val (commonKey, remainingKey) = searchKey.splitAt(sharedKey.length)
          if (searchKey.length >= sharedKey.length && (sharedKey sameElements commonKey)) get(next, remainingKey)
          else None
        case BranchNode(children, terminator) =>
          if (searchKey.isEmpty) terminator
          else children(searchKey(0)) match {
            case Some(childNodeId) =>
              val remainingKey = searchKey.slice(1, searchKey.length)
              get(childNodeId, remainingKey)
            case None => None
          }
      }
      case None => None
    }
  }

  private def put(nodeId: Array[Byte], searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    getNode(nodeId, dataSource) match{
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
                BranchNode.withSingleChild(existingKey(0), newLeafNode.capped(hashFn), None) -> Some(newLeafNode)
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
            val NodeInsertResult(newBranchNode, dataSourceAfterPut) = put(temporalBranchNode, searchKeySuffix, value)
            val newExtNode = ExtensionNode(searchKeyPrefix, newBranchNode.capped(hashFn))
            NodeInsertResult(
              newNode = newExtNode,
              dataSource = updateNodesInStorage(Seq(node), Seq(newBranchNode, newExtNode), dataSourceAfterPut, hashFn)
            )
          case ml =>
            // Partially shared prefix, we replace the leaf with an extension and a branch node
            val (searchKeyPrefix, searchKeySuffix) = searchKey.splitAt(ml)
            val temporalLeafNode = LeafNode(existingKey.drop(ml), storedValue)
            val NodeInsertResult(newBranchNode: BranchNode, dataSourceAfterPut) = put(temporalLeafNode, searchKeySuffix, value)
            val newExtNode = ExtensionNode(searchKeyPrefix, newBranchNode.capped(hashFn))
            NodeInsertResult(
              newNode = newExtNode,
              dataSource = updateNodesInStorage(Seq(node), Seq(newBranchNode, newExtNode), dataSourceAfterPut, hashFn)
            )
        }
      case ExtensionNode(sharedKey, next) =>
        matchingLength(sharedKey, searchKey) match {
          case 0 =>
            // There is no common prefix with the node which means we have to replace it for a branch node
            val sharedKeyHead = sharedKey(0)
            val (newChildEnc, maybeNewLeaf) =
              if (sharedKey.length == 1) next -> None // Direct extension, we just replace the extension with a branch
              else {
                // The new branch node will have an extension that replaces current one
                val newExtNode = ExtensionNode(sharedKey.tail, next)
                newExtNode.capped(hashFn) -> Some(newExtNode)
              }
            val temporalBranchNode = BranchNode.withSingleChild(sharedKeyHead, newChildEnc, None)
            val NodeInsertResult(newBranchNode, dataSourceAfterPut) = put(temporalBranchNode, searchKey, value)
            NodeInsertResult(
              newNode = newBranchNode,
              dataSource = updateNodesInStorage(Seq(node), Seq(newBranchNode) ++ maybeNewLeaf.toList, dataSourceAfterPut, hashFn)
            )
          case ml if ml == sharedKey.length =>
            // Current extension node's key is a prefix of the one being inserted, so we insert recursively on the extension's child
            val NodeInsertResult(newChild, dataSourceAfterPut) = put(next, searchKey.drop(ml), value)
            val newExtNode = ExtensionNode(sharedKey, newChild.capped(hashFn))
            NodeInsertResult(
              newNode = newExtNode,
              dataSource = updateNodesInStorage(Seq(node), Seq(newExtNode), dataSourceAfterPut, hashFn)
            )
          case ml =>
            // Partially shared prefix, we have to replace the node with an extension with the shared prefix
            val (sharedKeyPrefix, sharedKeySuffix) = sharedKey.splitAt(ml)
            val temporalExtensionNode = ExtensionNode(sharedKeySuffix, next)
            val NodeInsertResult(newBranchNode, dataSourceAfterPut) = put(temporalExtensionNode, searchKey.drop(ml), value)
            val newExtNode = ExtensionNode(sharedKeyPrefix, newBranchNode.capped(hashFn))
            NodeInsertResult(
              newNode = newExtNode,
              dataSource = updateNodesInStorage(Seq(node), Seq(newBranchNode, newExtNode), dataSourceAfterPut, hashFn)
            )
        }
      case BranchNode(children, optStoredValue) =>
        if (searchKey.isEmpty) {
          // The key is empty, the branch node should now be a terminator node with the new value asociated with it
          val newBranchNode = BranchNode(children, Some(value))
          NodeInsertResult(
            newNode = newBranchNode,
            dataSource = updateNodesInStorage(Seq(node), Seq(newBranchNode), dataSource, hashFn)
          )
        }
        else { // Non empty key, we need to insert the value in the correct branch node's child
          val searchKeyHead: Int = searchKey(0)
          val searchKeyRemaining = searchKey.tail
          children(searchKeyHead) match {
            case Some(child) =>
              // The associated child is not empty, we recursively insert in that child
              val NodeInsertResult(changedChild, dataSourceAfterPut) = put(child, searchKeyRemaining, value)
              val newBranchNode = BranchNode(children.updated(searchKeyHead, Some(changedChild.capped(hashFn))), optStoredValue)
              NodeInsertResult(
                newNode = newBranchNode,
                dataSource = updateNodesInStorage(Seq(node), Seq(newBranchNode), dataSourceAfterPut, hashFn)
              )
            case None =>
              // The associated child is empty, we just replace it with a leaf
              val newLeafNode = LeafNode(searchKeyRemaining, value)
              val newBranchNode = BranchNode(children.updated(searchKeyHead, Some(newLeafNode.capped(hashFn))), optStoredValue)
              NodeInsertResult(
                newNode = newBranchNode,
                dataSource = updateNodesInStorage(Seq(node), Seq(newLeafNode, newBranchNode), dataSource, hashFn)
              )
          }
        }
    }
  }

  private def remove(nodeId: Array[Byte], searchKey: Array[Byte]): NodeRemoveResult = {
    getNode(nodeId, dataSource) match{
      case Some(node) => remove(node, searchKey)
      case None => throw new Exception("Node not found")
    }
  }

  private def remove(node: Node, searchKey: Array[Byte]): NodeRemoveResult = (node, searchKey.isEmpty) match {
    // They key matches a branch node but it's value doesn't match the key
    case (BranchNode(children, None), true) => NodeRemoveResult(hasChanged = false, maybeNewChild = None, dataSource = dataSource)
    // We want to delete Branch node value
    case (BranchNode(children, _), true) => {
      // We need to remove old node and fix it because we removed the value
      val fixedNode = fix(BranchNode(children, None), dataSource)
      val afterFixedSave = updateNodesInStorage(Seq(node), Seq(fixedNode), dataSource, hashFn)
      NodeRemoveResult(hasChanged = true, maybeNewChild = Some(fixedNode), dataSource = afterFixedSave)
    }
    case (BranchNode(children, optStoredValue), false) =>
      // We might be trying to remove a node that's inside one of the 16 mapped nibbles
      val searchKeyHead = searchKey(0)
      children(searchKeyHead) match {
        // Child has been found so we try to remove it
        case Some(child) =>
          remove(child, searchKey.tail) match {
            case NodeRemoveResult(true, maybeNewChild, afterChildRemovalDataSource) => {
              // Something changed in a child so we need to fix
              val nodeToFix = maybeNewChild match {
                case Some(newChild) => BranchNode(children.updated(searchKeyHead, Some(newChild.capped(hashFn))), optStoredValue)
                case None => BranchNode(children.updated(searchKeyHead, None), optStoredValue)
              }
              val fixedNode = fix(nodeToFix, afterChildRemovalDataSource)
              val afterFixedSave = updateNodesInStorage(Seq(node), Seq(fixedNode), afterChildRemovalDataSource, hashFn)
              NodeRemoveResult(hasChanged = true, maybeNewChild = Some(fixedNode), dataSource = afterFixedSave)
            }
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
    case (ExtensionNode(sharedKey, next), _) =>
      val cp = matchingLength(sharedKey, searchKey)
      if (cp == sharedKey.length) {
        // A child node of this extension is removed, so move forward
        remove(next, searchKey.drop(cp)) match {
          case NodeRemoveResult(true, maybeNewChild, afterChildRemovalDataSource) => {
            // If we changed the child, we need to fix this extension node
            val toFix = if (maybeNewChild.isEmpty) ExtensionNode(sharedKey, Array.emptyByteArray) // FIXME Is an empty byte array a valid value?
            else ExtensionNode(sharedKey, maybeNewChild.get.capped(hashFn))
            val fixedNode = fix(toFix, afterChildRemovalDataSource)
            val afterFixedInsertion = updateNodesInStorage(Seq(node), Seq(fixedNode), afterChildRemovalDataSource, hashFn)
            NodeRemoveResult(hasChanged = true, maybeNewChild = Some(fixedNode), dataSource = afterFixedInsertion)
          }
          case NodeRemoveResult(false, _, _) => NodeRemoveResult(hasChanged = false, maybeNewChild = None, dataSource = dataSource)
        }
      } else NodeRemoveResult(hasChanged = false, maybeNewChild = Some(node), dataSource = dataSource)
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
        //case (NoIndex(), None) => Failure(new Exception("Branch with no subvalues")) FIXME Is this case valid?
        case (index :: Nil, None) =>
          val temporalExtNode = ExtensionNode(Array[Byte](index.toByte), children(index).get)
          fix(temporalExtNode, dataSource)
        case (Nil, Some(value)) => LeafNode(Array.emptyByteArray, value)
        case _ => node
      }
    case ExtensionNode(sharedKey, next) =>
      getNode(next, dataSource) match{
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
    val encoded = RLP.encode[Node](this)
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

private case class ExtensionNode(sharedKey: Array[Byte], next: Array[Byte]) extends Node

private case class BranchNode(children: Seq[Option[Array[Byte]]], terminator: Option[Array[Byte]]) extends Node

private object BranchNode {
  private val emptyChildren: Seq[Option[Array[Byte]]] = Array.fill(MerklePatriciaTree.ListSize - 1)(None)

  def withValueOnly(terminator: Array[Byte]): BranchNode = new BranchNode(emptyChildren, Some(terminator))

  def withSingleChild(position: Byte, child: Array[Byte], terminator: Option[Array[Byte]]): BranchNode =
    new BranchNode(emptyChildren.updated(position, Some(child)), terminator)
}

private case class NodeInsertResult(newNode: Node, dataSource: DataSource)

private case class NodeRemoveResult(hasChanged: Boolean, maybeNewChild: Option[Node], dataSource: DataSource)

