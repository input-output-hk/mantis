package io.iohk.ethereum.mpt.MptVisitors

import io.iohk.ethereum.mpt.{BranchNode, ExtensionNode, HashNode, LeafNode, MptNode}

class ProofVisitor[V](rootNode: MptNode) extends MptVisitor[MptNode] {
  var proof: List[MptNode] = List.empty // TODO concurrent data structure to achieve thread safety ?

  override def visitLeaf(value: LeafNode): MptNode = ???

  override def visitExtension(value: ExtensionNode): ExtensionVisitor[MptNode] = ???

  override def visitBranch(value: BranchNode): BranchVisitor[MptNode] = ???

  override def visitHash(value: HashNode): HashNodeResult[MptNode] = ???

  override def visitNull(): MptNode = ???

  private def maybeTrackNode(node: MptNode) = {
    if (node.equals(rootNode) || isReferencedByHash(node))
      proof = node :: proof
  }

  private def isReferencedByHash(node: MptNode): Boolean = ??? // TODO should be implemented by node
}
