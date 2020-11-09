package io.iohk.ethereum.mpt.MptVisitors

import akka.util.ByteString
import io.iohk.ethereum.mpt.{BranchNode, ExtensionNode, HashNode, LeafNode, MptNode, NullNode}

// TODO how to port logic from besu ProofVisitor esp. superclass GetVisitor?
// TODO how this Visitor is supposed to work
class ProofVisitor[V](rootNode: MptNode) extends MptVisitor[MptNode] { // TODO return Proof[V] ?
  var proof: List[MptNode] = List.empty // TODO concurrent data structure to achieve thread safety ?

  override def visitLeaf(value: LeafNode): MptNode = ???

  override def visitExtension(value: ExtensionNode): ExtensionVisitor[MptNode] =
    new ProofExtensionVisitor(value)

  override def visitBranch(value: BranchNode): BranchVisitor[MptNode] =
    new ProofBranchVisitor(value) // TODO add result to proof XOR pass proof as param

  override def visitHash(value: HashNode): HashNodeResult[MptNode] =
    ???

  override def visitNull(): MptNode = NullNode // TODO is this correct ?

  private def maybeTrackNode(node: MptNode) = {
    if (node.equals(rootNode) || isReferencedByHash(node))
      proof = node :: proof
  }

  private def isReferencedByHash(node: MptNode): Boolean = ??? // TODO should be implemented by node
}

class ProofBranchVisitor(value: BranchNode) extends BranchVisitor[MptNode] {
  override def visitChild(): MptVisitor[MptNode] = ???
  override def visitChild(child: => MptNode): Unit = ???
  override def visitTerminator(term: Option[ByteString]): Unit = ???
  override def done(): MptNode = ???
}

class ProofExtensionVisitor(value: ExtensionNode) extends ExtensionVisitor[MptNode] {
  override def visitNext(): MptVisitor[MptNode] = ???
  override def visitNext(value: => MptNode): Unit = ???
  override def done(): MptNode = ???
}
