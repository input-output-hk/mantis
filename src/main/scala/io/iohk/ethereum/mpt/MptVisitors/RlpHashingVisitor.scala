package io.iohk.ethereum.mpt.MptVisitors

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.mpt.{BranchNode, ExtensionNode, HashNode, LeafNode, MptNode, Node}
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPValue}

class NodeCapper(withUpdates: Boolean) {
  private var nodesToUpdate = List.empty[(NodeHash, NodeEncoded)]

  def capNode(nodeEncoded: RLPEncodeable, depth: Int): RLPEncodeable =
    if (depth > 0)
      capNode(nodeEncoded)
    else
      nodeEncoded

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

class RlpHashingVisitor(downstream: MptVisitor[RLPEncodeable], depth: Int, nodeCapper: NodeCapper)
    extends MptVisitor[RLPEncodeable] {
  def visitLeaf(value: LeafNode): RLPEncodeable =
    if (value.parsedRlp.isDefined) {
      value.parsedRlp.get
    } else {
      val leafEncoded = downstream.visitLeaf(value)
      nodeCapper.capNode(leafEncoded, depth)
    }

  def visitExtension(value: ExtensionNode): ExtensionVisitor[RLPEncodeable] =
    new RlpHashingExtensionVisitor(downstream.visitExtension(value), depth, value.parsedRlp, nodeCapper)

  def visitBranch(value: BranchNode): BranchVisitor[RLPEncodeable] =
    new RlpHashingBranchVisitor(downstream.visitBranch(value), depth, value.parsedRlp, nodeCapper)

  def visitHash(value: HashNode): HashNodeResult[RLPEncodeable] =
    downstream.visitHash(value)

  def visitNull(): RLPEncodeable =
    downstream.visitNull()
}

class RlpHashingBranchVisitor(
    downstream: BranchVisitor[RLPEncodeable],
    depth: Int,
    parsedRlp: Option[RLPEncodeable],
    nodeCapper: NodeCapper
) extends BranchVisitor[RLPEncodeable] {
  override def done(): RLPEncodeable =
    if (parsedRlp.isEmpty) {
      val branchEncoded = downstream.done()
      nodeCapper.capNode(branchEncoded, depth)
    } else {
      parsedRlp.get
    }

  override def visitChild(): MptVisitor[RLPEncodeable] =
    new RlpHashingVisitor(downstream.visitChild(), depth + 1, nodeCapper)

  override def visitChild(child: => RLPEncodeable): Unit =
    if (parsedRlp.isEmpty)
      downstream.visitChild(child)

  override def visitTerminator(term: Option[NodeHash]): Unit =
    if (parsedRlp.isEmpty)
      downstream.visitTerminator(term)
}

class RlpHashingExtensionVisitor(
    downstream: ExtensionVisitor[RLPEncodeable],
    depth: Int,
    parsedRlp: Option[RLPEncodeable],
    nodeCapper: NodeCapper
) extends ExtensionVisitor[RLPEncodeable] {
  override def visitNext(value: => RLPEncodeable): Unit =
    if (parsedRlp.isEmpty)
      downstream.visitNext(value)

  override def visitNext(): MptVisitor[RLPEncodeable] =
    new RlpHashingVisitor(downstream.visitNext(), depth + 1, nodeCapper)

  override def done(): RLPEncodeable =
    if (parsedRlp.isEmpty) {
      val extensionNodeEncoded = downstream.done()
      nodeCapper.capNode(extensionNodeEncoded, depth)
    } else {
      parsedRlp.get
    }
}
