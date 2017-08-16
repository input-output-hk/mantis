package io.iohk.ethereum.mpt

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.mpt.MerklePatriciaTrie.HashFn
import io.iohk.ethereum.network.p2p.messages.PV63.AccountImplicits
import io.iohk.ethereum.rlp.{encode => encodeRLP}

/**
  * Trie elements
  */
sealed trait MptNode {

  import MerklePatriciaTrie._

  lazy val encode: Array[Byte] = encodeRLP[MptNode](this)

  lazy val hash: Array[Byte] = Node.hashFn(encode)

  def capped: ByteString = {
    val encoded = encode
    if (encoded.length < 32) ByteString(encoded) else ByteString(hash)
  }
}

object Node {
  val hashFn: HashFn = (input: Array[Byte]) => crypto.kec256(input)
}

case class LeafNode(key: ByteString, value: ByteString) extends MptNode {
  import AccountImplicits._
  def getAccount: Account = value.toArray[Byte].toAccount
}

case class ExtensionNode(sharedKey: ByteString, next: Either[ByteString, MptNode]) extends MptNode

case class BranchNode(children: Seq[Option[Either[ByteString, MptNode]]], terminator: Option[ByteString]) extends MptNode {
  require(children.length == 16, "MptBranch childHashes length have to be 16")

  /**
    * This function creates a new BranchNode by updating one of the children of the self node.
    *
    * @param childIndex of the BranchNode children where the child should be inserted.
    * @param childNode  to be inserted as a child of the new BranchNode (and hashed if necessary).
    * @return a new BranchNode.
    */
  def updateChild(childIndex: Int, childNode: MptNode): BranchNode = {
    val childCapped = childNode.capped
    BranchNode(children.updated(childIndex, Some(if (childCapped.length == 32) Left(childCapped) else Right(childNode))), terminator)
  }
}

object ExtensionNode {
  /**
    * This function creates a new ExtensionNode with next parameter as its node pointer
    *
    * @param sharedKey of the new ExtensionNode.
    * @param next      to be inserted as the node pointer (and hashed if necessary).
    * @return a new BranchNode.
    */
  def apply(sharedKey: ByteString, next: MptNode): ExtensionNode = {
    val nextCapped = next.capped
    ExtensionNode(sharedKey, if (nextCapped.length == 32) Left(nextCapped) else Right(next))
  }
}

object BranchNode {
  private val emptyChildren: Seq[Option[Either[ByteString, MptNode]]] = Array.fill(MerklePatriciaTrie.ListSize - 1)(None)

  /**
    * This function creates a new terminator BranchNode having only a value associated with it.
    * This new BranchNode will be temporarily in an invalid state.
    *
    * @param terminator to be associated with the new BranchNode.
    * @return a new BranchNode.
    */
  def withValueOnly(terminator: Array[Byte]): BranchNode =
    BranchNode(emptyChildren, Some(ByteString(terminator)))

  /**
    * This function creates a new BranchNode having only one child associated with it (and optionaly a value).
    * This new BranchNode will be temporarily in an invalid state.
    *
    * @param position   of the BranchNode children where the child should be inserted.
    * @param child      to be inserted as a child of the new BranchNode (and hashed if necessary).
    * @param terminator to be associated with the new BranchNode.
    * @return a new BranchNode.
    */
  def withSingleChild(position: Byte, child: MptNode, terminator: Option[Array[Byte]]): BranchNode = {
    val childCapped = child.capped
    BranchNode(emptyChildren.updated(position, Some(if (childCapped.length == 32) Left(childCapped) else Right(child))), terminator.map(e => ByteString(e)))
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
  def withSingleChild(position: Byte, child: Either[ByteString, MptNode], terminator: Option[Array[Byte]]): BranchNode =
  BranchNode(emptyChildren.updated(position, Some(child)), terminator.map(e => ByteString(e)))
}
