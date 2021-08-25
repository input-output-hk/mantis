package io.iohk.ethereum.mpt

import akka.util.ByteString

import io.iohk.ethereum.db.storage.MptStorage
import io.iohk.ethereum.db.storage.NodeStorage.NodeEncoded
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException
import io.iohk.ethereum.mpt.MptVisitors._
import io.iohk.ethereum.rlp.RLPEncodeable
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.RLPValue
import io.iohk.ethereum.rlp.rawDecode

object MptTraversals {

  def collapseTrie(node: MptNode): (HashNode, List[(ByteString, Array[Byte])]) = {
    val nodeCapper = new NodeCapper(withUpdates = true)
    val nodeEncoded = encodeNode(node, Some(nodeCapper))
    val rootHash = ByteString(Node.hashFn(nodeEncoded))
    (HashNode(rootHash.toArray[Byte]), (rootHash, nodeEncoded) :: nodeCapper.getNodesToUpdate)
  }

  def parseTrieIntoMemory(rootNode: MptNode, source: MptStorage): MptNode =
    dispatch(rootNode, new MptConstructionVisitor(source))

  def encodeNode(node: MptNode, nodeCapper: Option[NodeCapper] = None): Array[Byte] = {
    val nodeEncoded = encode(node, nodeCapper)
    io.iohk.ethereum.rlp.encode(nodeEncoded)
  }

  def encode(node: MptNode, nodeCapper: Option[NodeCapper] = None): RLPEncodeable = {
    val nodeCap = nodeCapper.fold(new NodeCapper(withUpdates = false))(capper => capper)
    dispatch(node, new RlpHashingVisitor(new RlpEncVisitor, 0, nodeCap))
  }

  def decodeNode(nodeEncoded: NodeEncoded): MptNode =
    parseMpt(decodeNodeRlp(nodeEncoded))

  def decodeNodeRlp(nodeEncoded: NodeEncoded): RLPEncodeable =
    rawDecode(nodeEncoded)

  private def parseMpt(nodeEncoded: RLPEncodeable): MptNode = nodeEncoded match {
    case list @ RLPList(items @ _*) if items.size == MerklePatriciaTrie.ListSize =>
      var i = 0
      val children = new Array[MptNode](BranchNode.numberOfChildren)
      while (i < BranchNode.numberOfChildren) {
        children(i) = parseMpt(items(i))
        i = i + 1
      }
      val terminatorAsArray: ByteString = items.last
      BranchNode(
        children = children,
        terminator = if (terminatorAsArray.isEmpty) None else Some(terminatorAsArray),
        parsedRlp = Some(list)
      )

    case list @ RLPList(items @ _*) if items.size == MerklePatriciaTrie.PairSize =>
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

  private def dispatch[T](input: MptNode, visitor: MptVisitor[T]): T =
    input match {
      case leaf: LeafNode =>
        visitor.visitLeaf(leaf)
      case branch: BranchNode =>
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

      case _: NullNode.type =>
        visitor.visitNull()
    }
}
