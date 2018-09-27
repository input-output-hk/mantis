package io.iohk.ethereum.mpt

import java.util

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPList, RLPValue, rawDecode}

import io.iohk.ethereum.rlp.RLPImplicitConversions._
object MptTraversals {


  def encodeNodeWithUpdates(node: MptNode): ((ByteString, Array[Byte]), List[(ByteString, Array[Byte])]) = {
    val nodeCapper = new NodeCapper(withUpdates = true)
    val nodeEncoded = encodeNode(node, Some(nodeCapper))
    val rootHash = ByteString(Node.hashFn(nodeEncoded))
    ((rootHash, nodeEncoded), nodeCapper.getNodesToUpdate)
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
      HashNode(ByteString(bytes))

    case RLPValue(bytes) if bytes.isEmpty =>
      NullNode

    case _ => throw new MPTException("Invalid Node")
  }


  private def dispatch[T](input: MptNode, visitor: MptVisitor[T]): T = {
    input match{
      case leaf: LeafNode => visitor.visitLeaf(leaf)
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
        visitor.visitHash(hashNode)
      case nullNode: NullNode.type =>
        visitor.visitNull()
    }
  }

  abstract class MptVisitor[T]{
    def visitLeaf(value: LeafNode): T
    def visitExtension(value: ExtensionNode): ExtensionVisitor[T]
    def visitBranch(value: BranchNode): BranchVisitor[T]
    def visitHash(value: HashNode): T
    def visitNull(): T
  }

  abstract class BranchVisitor[T]{
    def visitChild(): MptVisitor[T]
    def visitChild(child: =>T): Unit
    def visitTerminator(term: Option[ByteString]): Unit
    def done(): T
  }

  abstract class ExtensionVisitor[T]{
    def visitNext(): MptVisitor[T]
    def visitNext(value: =>T): Unit
    def done(): T
  }

  class NodeCapper(withUpdates: Boolean) {
    private var nodesToUpdate = List.empty[(NodeHash, NodeEncoded)]

    def capNode(nodeEncoded: RLPEncodeable, depth: Int): RLPEncodeable = {
      if (depth > 0)
        capNode(nodeEncoded)
      else
        nodeEncoded
    }

    private def capNode(nodeEncoded: RLPEncodeable): RLPEncodeable = {
      val asArray = io.iohk.ethereum.rlp.encode(nodeEncoded)
      if (asArray.length < MptNode.MaxEncodedNodeLength)
        nodeEncoded
      else {
        val hash = Node.hashFn(asArray)
        if (withUpdates) {
          nodesToUpdate = (ByteString(hash), asArray) :: nodesToUpdate
        }
        RLPValue(hash)
      }
    }

    def getNodesToUpdate: List[(NodeHash, NodeEncoded)] = nodesToUpdate
  }

  class RlpHashingVisitor(downstream: MptVisitor[RLPEncodeable], depth: Int, nodeCapper: NodeCapper) extends MptVisitor[RLPEncodeable] {
    def visitLeaf(value: LeafNode): RLPEncodeable = {
      if (value.parsedRlp.isDefined){
        value.parsedRlp.get
      } else {
        val leafEncoded = downstream.visitLeaf(value)
        nodeCapper.capNode(leafEncoded, depth)
      }
    }

    def visitExtension(value: ExtensionNode): ExtensionVisitor[RLPEncodeable] =
      new RlpHashingExtensionVisitor(downstream.visitExtension(value), depth, value.parsedRlp, nodeCapper)

    def visitBranch(value: BranchNode): BranchVisitor[RLPEncodeable] =
      new RlpHashingBranchVisitor(downstream.visitBranch(value), depth, value.parsedRlp, nodeCapper)

    def visitHash(value: HashNode): RLPEncodeable =
      downstream.visitHash(value)

    def visitNull(): RLPEncodeable =
      downstream.visitNull()
  }

  class RlpHashingBranchVisitor(downstream: BranchVisitor[RLPEncodeable],
                                depth: Int,
                                parsedRlp: Option[RLPEncodeable],
                                nodeCapper: NodeCapper) extends BranchVisitor[RLPEncodeable] {
    override def done(): RLPEncodeable = {
      if (parsedRlp.isEmpty) {
        val branchEncoded = downstream.done()
        nodeCapper.capNode(branchEncoded, depth)
      } else {
        parsedRlp.get
      }
    }

    override def visitChild(): MptVisitor[RLPEncodeable] = new RlpHashingVisitor(downstream.visitChild(), depth + 1, nodeCapper)

    override def visitChild(child: =>RLPEncodeable): Unit = {
      if (parsedRlp.isEmpty)
        downstream.visitChild(child)
    }

    override def visitTerminator(term: Option[NodeHash]): Unit = {
      if (parsedRlp.isEmpty)
        downstream.visitTerminator(term)
    }
  }

  class RlpHashingExtensionVisitor(downstream: ExtensionVisitor[RLPEncodeable],
                                   depth: Int,
                                   parsedRlp: Option[RLPEncodeable],
                                   nodeCapper: NodeCapper) extends ExtensionVisitor[RLPEncodeable] {
    override def visitNext(value: =>RLPEncodeable): Unit = {
      if (parsedRlp.isEmpty)
        downstream.visitNext(value)
    }

    override def visitNext(): MptVisitor[RLPEncodeable] = new RlpHashingVisitor(downstream.visitNext(), depth + 1, nodeCapper)

    override def done(): RLPEncodeable = {
      if (parsedRlp.isEmpty) {
        val extensionNodeEncoded = downstream.done()
        nodeCapper.capNode(extensionNodeEncoded, depth)
      } else {
        parsedRlp.get
      }
    }
  }


  class RlpExtensionVisitor(extensionNode: ExtensionNode) extends ExtensionVisitor[RLPEncodeable] {
    val array: Array[RLPEncodeable] = new Array[RLPEncodeable](2)

    array(0) = RLPValue(HexPrefix.encode(nibbles = extensionNode.sharedKey.toArray[Byte], isLeaf = false))

    override def visitNext(): MptVisitor[RLPEncodeable] = new RlpEncVisitor

    override def visitNext(value: =>RLPEncodeable): Unit = {
      array(1) = value
    }

    override def done(): RLPEncodeable = {
      RLPList(util.Arrays.copyOf[RLPEncodeable](array, 2): _*)
    }
  }

  class RlpBranchVisitor(branchNode: BranchNode) extends BranchVisitor[RLPEncodeable] {

    var list: List[RLPEncodeable] = List.empty[RLPEncodeable]

    override def visitChild(): MptVisitor[RLPEncodeable] = new RlpEncVisitor

    override def visitChild(child: =>RLPEncodeable): Unit = {
      list = child :: list
    }

    override def visitTerminator(term: Option[NodeHash]): Unit = {
      list = RLPValue(term.map(_.toArray[Byte]).getOrElse(Array.emptyByteArray)) :: list
    }

    override def done(): RLPEncodeable = {
      RLPList(list.reverse: _*)
    }
  }

  class RlpEncVisitor extends MptVisitor[RLPEncodeable] {

    def visitLeaf(leaf: LeafNode): RLPEncodeable = {
      RLPList(RLPValue(HexPrefix.encode(nibbles = leaf.key.toArray[Byte], isLeaf = true)), RLPValue(leaf.value.toArray[Byte]))
    }
    def visitHash(hashNode: HashNode): RLPEncodeable = {
      RLPValue(hashNode.hashNode.toArray[Byte])
    }

    override def visitNull(): RLPEncodeable = {
      RLPValue(Array.emptyByteArray)
    }

    override def visitExtension(extension: ExtensionNode): ExtensionVisitor[RLPEncodeable] = new RlpExtensionVisitor(extension)

    override def visitBranch(value: BranchNode): BranchVisitor[RLPEncodeable] = new RlpBranchVisitor(value)
  }
}
