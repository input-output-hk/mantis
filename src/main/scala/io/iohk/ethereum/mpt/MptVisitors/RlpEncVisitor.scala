package io.iohk.ethereum.mpt.MptVisitors

import java.util

import io.iohk.ethereum.db.storage.NodeStorage.NodeHash
import io.iohk.ethereum.mpt.{BranchNode, ExtensionNode, HashNode, HexPrefix, LeafNode}
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPList, RLPValue}

import scala.collection.immutable.ArraySeq

class RlpExtensionVisitor(extensionNode: ExtensionNode) extends ExtensionVisitor[RLPEncodeable] {
  val array: Array[RLPEncodeable] = new Array[RLPEncodeable](2)

  array(0) = RLPValue(HexPrefix.encode(nibbles = extensionNode.sharedKey.toArray[Byte], isLeaf = false))

  override def visitNext(): MptVisitor[RLPEncodeable] = new RlpEncVisitor

  override def visitNext(value: => RLPEncodeable): Unit = {
    array(1) = value
  }

  override def done(): RLPEncodeable = {
    val copy = util.Arrays.copyOf[RLPEncodeable](array, 2)
    RLPList(ArraySeq.unsafeWrapArray(copy): _*)
  }
}

class RlpBranchVisitor(branchNode: BranchNode) extends BranchVisitor[RLPEncodeable] {

  var list: List[RLPEncodeable] = List.empty[RLPEncodeable]

  override def visitChild(): MptVisitor[RLPEncodeable] = new RlpEncVisitor

  override def visitChild(child: => RLPEncodeable): Unit = {
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
    RLPList(
      RLPValue(HexPrefix.encode(nibbles = leaf.key.toArray[Byte], isLeaf = true)),
      RLPValue(leaf.value.toArray[Byte])
    )
  }
  def visitHash(hashNode: HashNode): HashNodeResult[RLPEncodeable] = {
    Result(RLPValue(hashNode.hashNode))
  }

  override def visitNull(): RLPEncodeable = {
    RLPValue(Array.emptyByteArray)
  }

  override def visitExtension(extension: ExtensionNode): ExtensionVisitor[RLPEncodeable] = new RlpExtensionVisitor(
    extension
  )

  override def visitBranch(value: BranchNode): BranchVisitor[RLPEncodeable] = new RlpBranchVisitor(value)
}
