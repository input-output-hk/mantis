package io.iohk.ethereum.mpt.MptVisitors

import akka.util.ByteString
import io.iohk.ethereum.mpt.{BranchNode, ExtensionNode, HashNode, LeafNode, MptNode}

sealed trait HashNodeResult[T] {
  def next(visitor: MptVisitor[T])(f: (MptNode, MptVisitor[T]) => T): T = this match {
    case Result(value) => value
    case ResolveResult(node) => f(node, visitor)
  }
}
final case class Result[T](t: T) extends HashNodeResult[T]
final case class ResolveResult[T](mptNode: MptNode) extends HashNodeResult[T]

trait MptVisitor[T] {
  def visitLeaf(value: LeafNode): T
  def visitExtension(value: ExtensionNode): ExtensionVisitor[T]
  def visitBranch(value: BranchNode): BranchVisitor[T]
  def visitHash(value: HashNode): HashNodeResult[T]
  def visitNull(): T
}

trait BranchVisitor[T] {
  def visitChild(): MptVisitor[T]
  def visitChild(child: => T): Unit
  def visitTerminator(term: Option[ByteString]): Unit
  def done(): T
}

trait ExtensionVisitor[T] {
  def visitNext(): MptVisitor[T]
  def visitNext(value: => T): Unit
  def done(): T
}
