package io.iohk.ethereum.mpt

import akka.util.ByteString
import io.iohk.ethereum.db.storage.MptStorage
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException
import io.iohk.ethereum.mpt.MptVisitors._
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPList, RLPValue, rawDecode}
import io.iohk.ethereum.rlp.RLPImplicitConversions._


// scalastyle:off
object MptTraversals {

  def collapseTrie(node: MptNode): (HashNode, List[(ByteString, Array[Byte])]) = {
    val nodeCapper = new NodeCapper(withUpdates = true)
    val nodeEncoded = encodeNode(node, Some(nodeCapper))
    val rootHash = ByteString(Node.hashFn(nodeEncoded))
    (HashNode(rootHash.toArray[Byte]), (rootHash, nodeEncoded) :: nodeCapper.getNodesToUpdate)
  }

  def parseTrieIntoMemory(rootNode: MptNode, source: MptStorage): MptNode = {
    dispatch(rootNode, new MptConstructionVisitor(source))
  }

  def encodeNode(node: MptNode, nodeCapper: Option[NodeCapper] = None): Array[Byte] = {
    val nodeEncoded = encode(node, nodeCapper)
    io.iohk.ethereum.rlp.encode(nodeEncoded)
  }

  def encode(node: MptNode, nodeCapper: Option[NodeCapper] = None): RLPEncodeable = {
    val nodeCap = nodeCapper.fold(new NodeCapper(withUpdates = false))(capper => capper)
    dispatch(node, new RlpHashingVisitor(new RlpEncVisitor, 0, nodeCap))
  }

  def decodeNode(nodeEncoded: NodeEncoded): MptNode = {
    parseMpt(decodeNodeRlp(nodeEncoded))
  }

  def decodeNodeRlp(nodeEncoded: NodeEncoded): RLPEncodeable = {
    rawDecode(nodeEncoded)
  }

  private def parseMpt(nodeEncoded: RLPEncodeable): MptNode = nodeEncoded match {
    case list@RLPList(items@_*) if items.size == MerklePatriciaTrie.ListSize =>
      var i = 0
      val children = new Array[MptNode](BranchNode.numberOfChildren)
      while (i < BranchNode.numberOfChildren) {
        children(i) =  parseMpt(items(i))
        i = i + 1
      }
      val terminatorAsArray: ByteString = items.last
      BranchNode(children = children, terminator = if (terminatorAsArray.isEmpty) None else Some(terminatorAsArray), parsedRlp = Some(list))

    case list@RLPList(items@_*) if items.size == MerklePatriciaTrie.PairSize =>
      val (key, isLeaf) = HexPrefix.decode(items.head)
      if (isLeaf)
        LeafNode(ByteString(key), items.last, parsedRlp = Some(list))
      else {
        ExtensionNode(ByteString(key), parseMpt(items.last), parsedRlp = Some(list))
      }

    case RLPValue(bytes) if bytes.length == MptNode.MaxEncodedNodeLength =>
      HashNode(bytes)

    case RLPValue(bytes) if bytes.isEmpty =>
      NullNode

    case _ => throw new MPTException("Invalid Node")
  }

  private def dispatch[T](input: MptNode, visitor: MptVisitor[T]): T = {
    input match{
      case leaf: LeafNode =>
        visitor.visitLeaf(leaf)
      case branch : BranchNode =>
        val branchVisitor = visitor.visitBranch(branch)
        var i = 0
        while (i < BranchNode.numberOfChildren) {
          val subVisitor = branchVisitor.visitChild()
          branchVisitor.visitChild(dispatch(branch.children(i), subVisitor))
          i = i + 1
        }
        branchVisitor.visitTerminator(branch.terminator)
        branchVisitor.done()

      case extension: ExtensionNode =>
        val extensionVisitor = visitor.visitExtension(extension)
        val subVisitor = extensionVisitor.visitNext()
        extensionVisitor.visitNext(dispatch(extension.next, subVisitor))
        extensionVisitor.done()

      case hashNode: HashNode =>
        val vistResult = visitor.visitHash(hashNode)
        vistResult.next(visitor)(dispatch)

      case nullNode: NullNode.type =>
        visitor.visitNull()
    }
  }


  sealed abstract class ParsedNode{
    val parsedRlp: RLPEncodeable
  }
  final case class NodeStoredInParent(parsedRlp: RLPEncodeable) extends ParsedNode
  final case class AlreadyExistingNode(parsedRlp: RLPEncodeable) extends ParsedNode
  final case class ExistingHashNode(parsedRlp: RLPEncodeable, hash: ByteString) extends ParsedNode
  final case class NewNode(parsedRlp: RLPEncodeable, hash: ByteString, nodeEncoded: NodeEncoded, children: List[NodeData]) extends ParsedNode
  case object ParsedNull extends ParsedNode {
    override val parsedRlp: RLPEncodeable = NullNode.parsedRlp.get
  }



  final case class NodeToUpdate(nodeHash: NodeHash, nodeEncoded: NodeEncoded)

  sealed abstract class NodeData {
    val hash: ByteString
  }
  final case class PartialNode(hash: ByteString) extends NodeData
  final case class FullNode(hash: ByteString, nodeEncoded: NodeEncoded, children: List[NodeData]) extends NodeData


  def filterNulls(parsedNode: List[ParsedNode]) = {
    parsedNode.flatMap{
      case NewNode(parsedRlp, hash, nodeEncoded, children) => Some(FullNode(hash, nodeEncoded, children))
      case ExistingHashNode(parsedRlp, hash) => Some(PartialNode(hash))
      case _ => None
    }
  }

  def getCached(node: MptNode): (NodeData, List[NodeToUpdate]) = {
    val handler = new ParsedNodeHandler
    val parsed = dispatch(node, new MptCachingVisitor(new RlpEncVisitor, handler, 0)).asInstanceOf[NewNode]
    (FullNode(parsed.hash, parsed.nodeEncoded, parsed.children), handler.getNodesToUpdate)
  }
  class ParsedNodeHandler{
    private var nodesToUpdate = List.empty[NodeToUpdate]

    def capNode(parsedNode: RLPEncodeable, depth: Int, children: List[ParsedNode]): ParsedNode = {
      if (depth > 0)
        capNode(parsedNode, children)
      else {
        val enc =  io.iohk.ethereum.rlp.encode(parsedNode)
        val hash = ByteString(Node.hashFn(enc))
        val newNode = NewNode(parsedNode, hash, enc, filterNulls(children))
        nodesToUpdate = NodeToUpdate(hash, enc) :: nodesToUpdate
        newNode
      }
    }

    private def capNode(nodeEncoded: RLPEncodeable, children: List[ParsedNode]): ParsedNode = {
      val asArray = io.iohk.ethereum.rlp.encode(nodeEncoded)
      if (asArray.length < MptNode.MaxEncodedNodeLength)
        NodeStoredInParent(nodeEncoded)
      else {
        val hash = ByteString(Node.hashFn(asArray))
        val newNode = NewNode(RLPValue(hash.toArray[Byte]), hash, asArray, filterNulls(children))
        nodesToUpdate = NodeToUpdate(hash, asArray) :: nodesToUpdate
        newNode
      }
    }

    def getNodesToUpdate: List[NodeToUpdate] = nodesToUpdate
  }

  class MptCachingVisitor(downstream: MptVisitor[RLPEncodeable], parsedNodeHandler: ParsedNodeHandler, depth: Int) extends MptVisitor[ParsedNode] {

    def visitLeaf(leaf: LeafNode): ParsedNode = {
      if (leaf.isNew) {
        val leafEncoded = downstream.visitLeaf(leaf)
        parsedNodeHandler.capNode(leafEncoded, depth, List.empty)
      } else {
        AlreadyExistingNode(leaf.parsedRlp.get)
      }
    }

    override def visitHash(hashNode: HashNode): HashNodeResult[ParsedNode] = {
      Result(ExistingHashNode(hashNode.parsedRlp.get, ByteString(hashNode.hash)))
    }

    override def visitNull(): ParsedNode = {
      ParsedNull
    }

    override def visitExtension(extension: ExtensionNode): ExtensionVisitor[ParsedNode] =
      new MptCachingExtensionVisitor(downstream.visitExtension(extension), extension, parsedNodeHandler, depth)

    override def visitBranch(value: BranchNode): BranchVisitor[ParsedNode] =
      new MptCachingBranchVisitor(downstream.visitBranch(value), value, parsedNodeHandler, depth)
  }

  class MptCachingBranchVisitor(downstream: BranchVisitor[RLPEncodeable],
                                branchNode: BranchNode,
                                parsedNodeHandler: ParsedNodeHandler,
                                depth: Int) extends BranchVisitor[ParsedNode] {
    var children = List.empty[ParsedNode]

    override def done(): ParsedNode = {
      if (branchNode.isNew) {
        val branchEncoded = downstream.done()
        parsedNodeHandler.capNode(branchEncoded, depth, children)
      } else {
        AlreadyExistingNode(branchNode.parsedRlp.get)
      }
    }

    override def visitChild(): MptVisitor[ParsedNode] = new MptCachingVisitor(downstream.visitChild(), parsedNodeHandler, depth + 1)

    override def visitTerminator(term: Option[NodeHash]): Unit = {
      if (branchNode.isNew) {
        downstream.visitTerminator(term)
      }
    }

    override def visitChild(child: => ParsedNode): Unit = {
      if (branchNode.isNew) {
        val resolvedChild = child
        downstream.visitChild(resolvedChild.parsedRlp)
        children = resolvedChild :: children
      }
    }
  }

  class MptCachingExtensionVisitor(downstream: ExtensionVisitor[RLPEncodeable],
                                   extensionNode: ExtensionNode,
                                   parsedNodeHandler: ParsedNodeHandler,
                                   depth: Int) extends ExtensionVisitor[ParsedNode] {
    var resolvedChild: List[ParsedNode] = List.empty[ParsedNode]

    override def done(): ParsedNode = {
      if (extensionNode.isNew) {
        val extensionEncoded = downstream.done()
        parsedNodeHandler.capNode(extensionEncoded, depth, resolvedChild)
      } else {
        AlreadyExistingNode(extensionNode.parsedRlp.get)
      }
    }

    override def visitNext(): MptVisitor[ParsedNode] = new MptCachingVisitor(downstream.visitNext(), parsedNodeHandler, depth + 1)

    override def visitNext(child: => ParsedNode): Unit = {
      if (extensionNode.isNew) {
        val resolvedChid = child
        downstream.visitNext(resolvedChid.parsedRlp)
        resolvedChild = resolvedChid :: resolvedChild
      }
    }
  }

}
