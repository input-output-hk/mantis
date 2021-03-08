package io.iohk.ethereum.mpt

import java.util

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPValue}

/** Trie elements
  */
sealed abstract class MptNode {
  val cachedHash: Option[Array[Byte]]
  val cachedRlpEncoded: Option[Array[Byte]]

  def withCachedHash(cachedHash: Array[Byte]): MptNode

  def withCachedRlpEncoded(cachedEncode: Array[Byte]): MptNode

  lazy val encode: Array[Byte] = cachedRlpEncoded.getOrElse {
    parsedRlp.fold(MptTraversals.encodeNode(this))(io.iohk.ethereum.rlp.encode)
  }

  lazy val hash: Array[Byte] = cachedHash.getOrElse(Node.hashFn(encode))

  def isNull: Boolean = false

  val parsedRlp: Option[RLPEncodeable]

  // Overriding equals is necessery to avoid array comparisons.
  override def equals(obj: Any): Boolean =
    if (!obj.isInstanceOf[MptNode]) {
      false
    } else {
      val compared = obj.asInstanceOf[MptNode]
      hash.sameElements(compared.hash)
    }

  override def hashCode(): Int =
    17 + util.Arrays.hashCode(hash)

  def isNew: Boolean = parsedRlp.isEmpty
}

object MptNode {
  val MaxEncodedNodeLength = 32
}

object Node {
  def hashFn(input: Array[Byte]): Array[Byte] =
    crypto.kec256(input, 0, input.length)
}

case class LeafNode(
    key: ByteString,
    value: ByteString,
    cachedHash: Option[Array[Byte]] = None,
    cachedRlpEncoded: Option[Array[Byte]] = None,
    parsedRlp: Option[RLPEncodeable] = None
) extends MptNode {
  def withCachedHash(cachedHash: Array[Byte]): MptNode = copy(cachedHash = Some(cachedHash))

  def withCachedRlpEncoded(cachedEncode: Array[Byte]): MptNode = copy(cachedRlpEncoded = Some(cachedEncode))

}

case class ExtensionNode(
    sharedKey: ByteString,
    next: MptNode,
    cachedHash: Option[Array[Byte]] = None,
    cachedRlpEncoded: Option[Array[Byte]] = None,
    parsedRlp: Option[RLPEncodeable] = None
) extends MptNode {
  def withCachedHash(cachedHash: Array[Byte]): MptNode = copy(cachedHash = Some(cachedHash))

  def withCachedRlpEncoded(cachedEncode: Array[Byte]): MptNode = copy(cachedRlpEncoded = Some(cachedEncode))

}

case class BranchNode(
    children: Array[MptNode],
    terminator: Option[ByteString],
    cachedHash: Option[Array[Byte]] = None,
    cachedRlpEncoded: Option[Array[Byte]] = None,
    parsedRlp: Option[RLPEncodeable] = None
) extends MptNode {
  def withCachedHash(cachedHash: Array[Byte]): MptNode = copy(cachedHash = Some(cachedHash))

  def withCachedRlpEncoded(cachedEncode: Array[Byte]): MptNode = copy(cachedRlpEncoded = Some(cachedEncode))

  require(children.length == 16, "MptBranch childHashes length have to be 16")

  /** This function creates a new BranchNode by updating one of the children of the self node.
    *
    * @param childIndex of the BranchNode children where the child should be inserted.
    * @param childNode  to be inserted as a child of the new BranchNode (and hashed if necessary).
    * @return a new BranchNode.
    */
  def updateChild(childIndex: Int, childNode: MptNode): BranchNode = {
    val updatedChildren = util.Arrays.copyOf(children, BranchNode.numberOfChildren)
    updatedChildren(childIndex) = childNode
    BranchNode(updatedChildren, terminator)
  }

}

case class HashNode(hashNode: Array[Byte]) extends MptNode {
  val cachedHash: Option[Array[Byte]] = Some(hashNode)
  val cachedRlpEncoded: Option[Array[Byte]] = Some(hashNode)
  def withCachedHash(cachedHash: Array[Byte]): MptNode = copy()

  def withCachedRlpEncoded(cachedEncode: Array[Byte]): MptNode = copy()
  val parsedRlp: Option[RLPEncodeable] = Some(RLPValue(hashNode))
}

case object NullNode extends MptNode {
  import MerklePatriciaTrie._
  val cachedHash: Option[Array[Byte]] = Some(EmptyRootHash)
  val cachedRlpEncoded: Option[Array[Byte]] = Some(EmptyEncoded)
  def withCachedHash(cachedHash: Array[Byte]): MptNode = this

  def withCachedRlpEncoded(cachedEncode: Array[Byte]): MptNode = this

  override def isNull: Boolean = true
  val parsedRlp: Option[RLPEncodeable] = Some(RLPValue(Array.emptyByteArray))
}

object ExtensionNode {

  /** This function creates a new ExtensionNode with next parameter as its node pointer
    *
    * @param sharedKey of the new ExtensionNode.
    * @param next      to be inserted as the node pointer (and hashed if necessary).
    * @return a new BranchNode.
    */
  def apply(sharedKey: ByteString, next: MptNode): ExtensionNode = {
    val nextNode = next
    new ExtensionNode(sharedKey, nextNode)
  }
}

object BranchNode {
  val numberOfChildren = 16
  private val emptyChildren: Array[MptNode] = Array.fill(numberOfChildren)(NullNode)

  /** This function creates a new terminator BranchNode having only a value associated with it.
    * This new BranchNode will be temporarily in an invalid state.
    *
    * @param terminator to be associated with the new BranchNode.
    * @return a new BranchNode.
    */
  def withValueOnly(terminator: Array[Byte]): BranchNode =
    BranchNode(util.Arrays.copyOf(emptyChildren, numberOfChildren), Some(ByteString(terminator)))

  /** This function creates a new BranchNode having only one child associated with it (and optionaly a value).
    * This new BranchNode will be temporarily in an invalid state.
    *
    * @param position   of the BranchNode children where the child should be inserted.
    * @param child      to be inserted as a child of the new BranchNode (and hashed if necessary).
    * @param terminator to be associated with the new BranchNode.
    * @return a new BranchNode.
    */
  def withSingleChild(position: Byte, child: MptNode, terminator: Option[Array[Byte]]): BranchNode = {
    val emptyCopy = util.Arrays.copyOf(emptyChildren, numberOfChildren)
    emptyCopy(position) = child
    BranchNode(emptyCopy, terminator.map(e => ByteString(e)))
  }
}
