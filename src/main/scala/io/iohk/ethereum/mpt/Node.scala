package io.iohk.ethereum.mpt

import java.util

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.rlp.{encode => encodeRLP}

/**
  * Trie elements
  */
sealed abstract class MptNode {
  val cachedHash: Option[Array[Byte]]
  val cachedRlpEncoded: Option[Array[Byte]]

  import MerklePatriciaTrie._

  def withCachedHash(cachedHash: Array[Byte]): MptNode

  def withCachedRlpEncoded(cachedEncode: Array[Byte]): MptNode

  lazy val encode: Array[Byte] = cachedRlpEncoded.getOrElse(encodeRLP[MptNode](this))

  lazy val hash: Array[Byte] = cachedHash.getOrElse(Node.hashFn(encode))

  def capped: ByteString = {
    val encoded = encode
    if (encoded.length < 32) ByteString(encoded) else ByteString(hash)
  }

  def isNull: Boolean
}

object Node {
  val hashFn: (Array[Byte]) => Array[Byte] = (input: Array[Byte]) => crypto.kec256(input)
}

case class LeafNode(key: ByteString, value: ByteString,
                    cachedHash: Option[Array[Byte]] = None, cachedRlpEncoded: Option[Array[Byte]] = None) extends MptNode {
  def withCachedHash(cachedHash: Array[Byte]): MptNode = copy(cachedHash = Some(cachedHash))

  def withCachedRlpEncoded(cachedEncode: Array[Byte]): MptNode = copy(cachedRlpEncoded = Some(cachedEncode))

  override def isNull: Boolean = false
}

case class ExtensionNode(sharedKey: ByteString, next: MptNode,
                         cachedHash: Option[Array[Byte]] = None, cachedRlpEncoded: Option[Array[Byte]] = None) extends MptNode {
  def withCachedHash(cachedHash: Array[Byte]): MptNode = copy(cachedHash = Some(cachedHash))

  def withCachedRlpEncoded(cachedEncode: Array[Byte]): MptNode = copy(cachedRlpEncoded = Some(cachedEncode))

  override def isNull: Boolean = false
}

case class BranchNode(children: Array[MptNode], terminator: Option[ByteString],
                      cachedHash: Option[Array[Byte]] = None, cachedRlpEncoded: Option[Array[Byte]] = None) extends MptNode {
  def withCachedHash(cachedHash: Array[Byte]): MptNode = copy(cachedHash = Some(cachedHash))

  def withCachedRlpEncoded(cachedEncode: Array[Byte]): MptNode = copy(cachedRlpEncoded = Some(cachedEncode))

  override def isNull: Boolean = false

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
    val updatedChild = if (childCapped.length == 32) HashNode(childCapped) else childNode
    val updatedChildren = util.Arrays.copyOf(children, BranchNode.numberOfChildren)
    updatedChildren(childIndex) = updatedChild
    BranchNode(updatedChildren, terminator)
  }


  // Overriding equals is necessery to avoid array comparisons.
  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[BranchNode]) {
      false
    } else{
      val compared = obj.asInstanceOf[BranchNode]
      hash sameElements compared.hash
    }
  }

  override def hashCode(): Int = {
    17 + util.Arrays.hashCode(hash)
  }
}

case class HashNode(hashNode: ByteString) extends MptNode {
  val cachedHash: Option[Array[Byte]] = Some(hashNode.toArray[Byte])
  val cachedRlpEncoded: Option[Array[Byte]] = None
  def withCachedHash(cachedHash: Array[Byte]): MptNode = copy()

  def withCachedRlpEncoded(cachedEncode: Array[Byte]): MptNode = copy()

  override def isNull: Boolean = false
}

case object NullNode extends MptNode {
  import MerklePatriciaTrie._
  val cachedHash: Option[Array[Byte]] = Some(EmptyRootHash)
  val cachedRlpEncoded: Option[Array[Byte]] = Some(EmptyEncoded)
  def withCachedHash(cachedHash: Array[Byte]): MptNode = this

  def withCachedRlpEncoded(cachedEncode: Array[Byte]): MptNode = this

  override def isNull: Boolean = true
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
    val nextNode = if (nextCapped.length == 32) HashNode(nextCapped) else next
    new ExtensionNode(sharedKey, nextNode)
  }
}

object BranchNode {
  val numberOfChildren = 16
  private val emptyChildren: Array[MptNode] = Array.fill(numberOfChildren)(NullNode)

  /**
    * This function creates a new terminator BranchNode having only a value associated with it.
    * This new BranchNode will be temporarily in an invalid state.
    *
    * @param terminator to be associated with the new BranchNode.
    * @return a new BranchNode.
    */
  def withValueOnly(terminator: Array[Byte]): BranchNode = {
    BranchNode(util.Arrays.copyOf(emptyChildren, numberOfChildren), Some(ByteString(terminator)))
  }

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
    val newNode = if (childCapped.length == 32) HashNode(childCapped) else child
    val emptyCopy = util.Arrays.copyOf(emptyChildren, numberOfChildren)
    emptyCopy(position) = newNode
    BranchNode(emptyCopy, terminator.map(e => ByteString(e)))
  }
}
