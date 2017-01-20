package io.iohk.ethereum.mpt

import io.iohk.ethereum.mpt.MerklePatriciaTrie.HashFn
import io.iohk.ethereum.rlp.{encode => encodeRLP}

/**
  * Trie elements
  */
private[mpt] sealed trait Node {

  import MerklePatriciaTrie._

  protected val hashFn: HashFn

  lazy val encode: Array[Byte] = encodeRLP[Node](this)

  lazy val hash: Array[Byte] = hashFn(encode)

  def capped: Array[Byte] = {
    val encoded = encode
    if (encoded.length < 32) encoded else hash
  }
}

private[mpt] case class LeafNode(key: Array[Byte], value: Array[Byte], hashFn: HashFn) extends Node

private[mpt] case class ExtensionNode(sharedKey: Array[Byte], next: Either[Array[Byte], Node], hashFn: HashFn) extends Node

private[mpt] object ExtensionNode {
  /**
    * This function creates a new ExtensionNode with next parameter as its node pointer
    *
    * @param sharedKey of the new ExtensionNode.
    * @param next      to be inserted as the node pointer (and hashed if necessary).
    * @param hashFn    to hash the node if necessary.
    * @return a new BranchNode.
    */
  def apply(sharedKey: Array[Byte], next: Node, hashFn: HashFn): ExtensionNode = {
    val nextCapped = next.capped
    ExtensionNode(sharedKey, if (nextCapped.length == 32) Left(nextCapped) else Right(next), hashFn)
  }
}

private[mpt] case class BranchNode(children: Seq[Option[Either[Array[Byte], Node]]], terminator: Option[Array[Byte]], hashFn: HashFn) extends Node {
  /**
    * This function creates a new BranchNode by updating one of the children of the self node.
    *
    * @param childIndex of the BranchNode children where the child should be inserted.
    * @param childNode  to be inserted as a child of the new BranchNode (and hashed if necessary).
    * @param hashFn     to hash the node if necessary.
    * @return a new BranchNode.
    */
  def updateChild(childIndex: Int, childNode: Node, hashFn: HashFn): BranchNode = {
    val childCapped = childNode.capped
    BranchNode(children.updated(childIndex, Some(if (childCapped.length == 32) Left(childCapped) else Right(childNode))), terminator, hashFn)
  }
}

private[mpt] object BranchNode {
  private val emptyChildren: Seq[Option[Either[Array[Byte], Node]]] = Array.fill(MerklePatriciaTrie.ListSize - 1)(None)

  /**
    * This function creates a new terminator BranchNode having only a value associated with it.
    * This new BranchNode will be temporarily in an invalid state.
    *
    * @param terminator to be associated with the new BranchNode.
    * @return a new BranchNode.
    */
  def withValueOnly(terminator: Array[Byte], hashFn: HashFn): BranchNode =
  BranchNode(emptyChildren, Some(terminator), hashFn)

  /**
    * This function creates a new BranchNode having only one child associated with it (and optionaly a value).
    * This new BranchNode will be temporarily in an invalid state.
    *
    * @param position   of the BranchNode children where the child should be inserted.
    * @param child      to be inserted as a child of the new BranchNode (and hashed if necessary).
    * @param terminator to be associated with the new BranchNode.
    * @param hashFn     to hash the node if necessary.
    * @return a new BranchNode.
    */
  def withSingleChild(position: Byte, child: Node, terminator: Option[Array[Byte]], hashFn: HashFn): BranchNode = {
    val childCapped = child.capped
    BranchNode(emptyChildren.updated(position, Some(if (childCapped.length == 32) Left(childCapped) else Right(child))), terminator, hashFn)
  }

  /**
    * This function creates a new BranchNode having only one child associated with it (and optionaly a value).
    * This new BranchNode will be temporarily in an invalid state.
    *
    * @param position   of the BranchNode children where the child should be inserted.
    * @param child      to be inserted as a child of the new BranchNode (already hashed if necessary).
    * @param terminator to be associated with the new BranchNode.
    * @return a new BranchNode.
    */
  def withSingleChild(position: Byte, child: Either[Array[Byte], Node], terminator: Option[Array[Byte]], hashFn: HashFn): BranchNode =
  BranchNode(emptyChildren.updated(position, Some(child)), terminator, hashFn)
}
