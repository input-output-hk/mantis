package io.iohk.ethereum.mpt

import akka.util.ByteString
import io.iohk.ethereum.common.SimpleMap
import io.iohk.ethereum.db.storage.NodeStorage
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.domain.{Account, Address, BlockchainImpl}
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{decode ⇒ decodeRLP, encode ⇒ encodeRLP, _}
import org.spongycastle.util.encoders.Hex

import scala.annotation.tailrec

object MerklePatriciaTrie {
  type AccountMPT = MerklePatriciaTrie[Address, Account]

  /** The key prefix corresponding to a node that participates in a proof sketch. */
  type ProofStepNibbles = Array[Byte]

  /** The cryptographic hash corresponding to a node that participates in a proof sketch. */
  type ProofHash = Array[Byte]

  implicit class ByteArrayExtensions(val a: Array[Byte]) extends AnyVal {
    def toByteString: ByteString = ByteString(a)

    def toHexString: String = Hex.toHexString(a)

    def nibblesToString: String = a.map { nibble ⇒ Integer.toHexString(nibble) }. mkString
  }

  implicit class ByteStringExtensions(val a: ByteString) extends AnyVal {
    def toHexString: String = a.toArray.toHexString
  }

  /**
   * Provides a step for a key membership proof.
   *
   * @param nibbles The part of the path (key) for this step.
   * @param hash    The node hash for this step.
   */
  final case class ProofStep(nibbles: ProofStepNibbles, hash: ProofHash) {
    def nibblesToString: String = nibbles.nibblesToString
    def nibblesToByteString: ByteString = ByteString(nibbles)

    def hashToHexString: String = Hex.toHexString(hash)
    def hashToByteString: ByteString = ByteString(hash)

    private[mpt] def hashToDebugHexString: String = {
      val hs = hashToHexString
      hs.substring(0, 4) + "_" + hs.substring(hs.length - 4, hs.length)
    }

    override def toString: String = productPrefix + s"([$nibblesToString], $hashToDebugHexString)"
  }

  final case class ProofSketch(steps: List[ProofStep]) {
    def length: Int = steps.length
  }

  def sameNibblesForKey(leafNode: LeafNode, nibbles: Array[Byte]): Boolean =
    leafNode.key.toArray[Byte] sameElements nibbles

  def sameNibblesForSharedKey(extensionNode: ExtensionNode, nibbles: Array[Byte]): Boolean =
    extensionNode.sharedKey.toArray[Byte] sameElements nibbles

  def sameHashForProof(node: MptNode, proofHash: ProofHash): Boolean =
    node.hash sameElements proofHash

  class MPTException(val message: String) extends RuntimeException(message)

  class MissingNodeException protected (val hash: ByteString, message: String) extends MPTException(message) {
    def this(hash: ByteString) = this(hash, s"Node not found ${Hex.toHexString(hash.toArray)}, trie is inconsistent")
  }

  class MissingRootNodeException(hash: ByteString) extends
    MissingNodeException(hash, s"Root node not found ${Hex.toHexString(hash.toArray)}")


  val EmptyRootHash: Array[Byte] = Node.hashFn(encodeRLP(Array.emptyByteArray))

  private case class NodeInsertResult(newNode: MptNode,
    toDeleteFromStorage: Seq[MptNode] = Nil,
    toUpdateInStorage: Seq[MptNode] = Nil)

  private case class NodeRemoveResult(hasChanged: Boolean, newNode: Option[MptNode],
    toDeleteFromStorage: Seq[MptNode] = Nil,
    toUpdateInStorage: Seq[MptNode] = Nil)

  private val PairSize: Byte = 2
  private[mpt] val ListSize: Byte = 17

  def apply[K, V](source: NodesKeyValueStorage)
    (implicit kSerializer: ByteArrayEncoder[K], vSerializer: ByteArraySerializable[V])
  : MerklePatriciaTrie[K, V] = new MerklePatriciaTrie[K, V](None, source)(kSerializer, vSerializer)

  def apply[K, V](rootHash: Array[Byte], source: NodesKeyValueStorage)
    (implicit kSerializer: ByteArrayEncoder[K], vSerializer: ByteArraySerializable[V])
  : MerklePatriciaTrie[K, V] = {
    if (EmptyRootHash sameElements rootHash) MerklePatriciaTrie(source)
    else new MerklePatriciaTrie[K, V](Some(rootHash), source)(kSerializer, vSerializer)
  }

  def ofAccounts(stateRoot: Array[Byte], storage: NodesKeyValueStorage): AccountMPT = {
    import Account.accountSerializer
    import Address.hashedAddressEncoder

    MerklePatriciaTrie[Address, Account](stateRoot, storage)
  }

  def ofAccounts(stateRoot: ByteString, storage: NodesKeyValueStorage): AccountMPT = {
    ofAccounts(stateRoot.toArray, storage)
  }

  // NOTE maybe add a method to Blockchain to get the Accounts MPT?
  /**
   * Creates an accounts MPT from the current status of the given `blockchain`.
   */
  def ofAccounts(blockchain: BlockchainImpl): AccountMPT = {
    val blockNumber = blockchain.getBestBlockNumber()
    val headerOpt = blockchain.getBlockHeaderByNumber(blockNumber)
    val stateRootOpt = headerOpt.map(_.stateRoot.toArray)
    val stateRoot = stateRootOpt.getOrElse(MerklePatriciaTrie.EmptyRootHash)

    // `blockNumber` seems to be used just for updating
    // and only in the [[ReferenceCountNodeStorage]]
    // implementation of [[NodesKeyValueStorage]] (!)
    val nodeStorage = blockchain.nodesKeyValueStorageFor(Some(blockNumber))

    MerklePatriciaTrie.ofAccounts(stateRoot, nodeStorage)
  }

  /**
    * Implicits
    */
  implicit val nodeDec: RLPDecoder[MptNode] = new RLPDecoder[MptNode] {
    override def decode(rlp: RLPEncodeable): MptNode = rlp match {
      case RLPList(items@_*) if items.size == MerklePatriciaTrie.ListSize =>
        val parsedChildren: Seq[Option[Either[ByteString, MptNode]]] = items.init.map {
          case list: RLPList => Some(Right(this.decode(list)))
          case RLPValue(bytes) =>
            if (bytes.isEmpty) None
            else Some(Left(ByteString(bytes)))
        }
        val terminatorAsArray: ByteString = items.last
        BranchNode(children = parsedChildren, terminator = if (terminatorAsArray.isEmpty) None else Some(terminatorAsArray))
      case RLPList(items@_*) if items.size == MerklePatriciaTrie.PairSize =>
        val (key, isLeaf) = HexPrefix.decode(items.head)
        if (isLeaf) LeafNode(ByteString(key), items.last)
        else {
          val next = items.last match {
            case list: RLPList => Right(this.decode(list))
            case RLPValue(bytes) => Left(ByteString(bytes))
          }
          ExtensionNode(ByteString(key), next)
        }
      case _ => throw new RuntimeException("Invalid Node")
    }
  }

  private def getNode(nodeId: Array[Byte], source: NodesKeyValueStorage)(implicit nodeDec: RLPDecoder[MptNode]): Option[MptNode] = {
    val maybeNodeEncoded =
      if (nodeId.length < 32) Some(nodeId)
      else source.get(ByteString(nodeId))
    maybeNodeEncoded.map(nodeEncoded => decodeRLP[MptNode](nodeEncoded).withCachedHash(nodeId).withCachedRlpEncoded(nodeEncoded))
  }

  private def getRootNode(rootId: Array[Byte], source: NodesKeyValueStorage): MptNode =
    getNode(rootId, source).getOrElse(throw new MissingRootNodeException(ByteString(rootId)))

  private def matchingLength(a: Array[Byte], b: Array[Byte]): Int = a.zip(b).takeWhile(t => t._1 == t._2).length

  private def updateNodesInStorage(
    previousRootHash: Array[Byte],
    newRoot: Option[MptNode],
    toRemove: Seq[MptNode],
    toUpdate: Seq[MptNode],
    nodeStorage: NodesKeyValueStorage): NodesKeyValueStorage = {
    val rootCapped = newRoot.map(_.capped).getOrElse(ByteString.empty)
    val toBeRemoved = toRemove.filter { node =>
      val nCapped = node.capped
      nCapped.length == 32 || (node.hash sameElements previousRootHash)
    }.map(n => ByteString(n.hash))
    val toBeUpdated = toUpdate.filter { n =>
      val nCapped = n.capped
      nCapped.length == 32 || (nCapped == rootCapped)
    }.map(node => ByteString(node.hash) -> node.encode)
    nodeStorage.update(toBeRemoved, toBeUpdated)
  }

  private def getNextNode(extensionNode: ExtensionNode, nodeStorage: NodesKeyValueStorage)(implicit nodeDec: RLPDecoder[MptNode]): MptNode =
    extensionNode.next match {
      case Right(node) => node
      case Left(hash) =>
        val nodeId = hash.toArray[Byte]
        MerklePatriciaTrie.getNode(nodeId, nodeStorage).getOrElse(throw new MissingNodeException(ByteString(nodeId)))
    }

  private def getChild(branchNode: BranchNode, pos: Int, nodeStorage: NodesKeyValueStorage)(implicit nodeDec: RLPDecoder[MptNode]): Option[MptNode] =
    branchNode.children(pos) map {
      case Right(node) => node
      case Left(hash) =>
        val nodeId = hash.toArray[Byte]
        MerklePatriciaTrie.getNode(nodeId, nodeStorage).getOrElse(throw new MissingNodeException(ByteString(nodeId)))
    }

  implicit val nodeEnc: RLPEncoder[MptNode] = new RLPEncoder[MptNode] {
    override def encode(obj: MptNode): RLPEncodeable = obj match {
      case leaf: LeafNode => RLPList(HexPrefix.encode(nibbles = leaf.key.toArray[Byte], isLeaf = true), leaf.value)
      case extension: ExtensionNode =>
        RLPList(HexPrefix.encode(nibbles = extension.sharedKey.toArray[Byte], isLeaf = false), extension.next match {
          case Right(node) => this.encode(node)
          case Left(bytes) => bytes
        })
      case branch: BranchNode =>
        val toEncode: Seq[RLPEncodeable] = branch.children.map {
          case Some(Right(node)) => this.encode(node)
          case Some(Left(bytes)) => RLPValue(bytes.toArray[Byte])
          case None => RLPValue(Array.emptyByteArray)
        } :+ RLPValue(branch.terminator.map(_.toArray[Byte]).getOrElse(Array.emptyByteArray))
        RLPList(toEncode: _*)
    }
  }

  implicit val defaultByteArraySerializable = new ByteArraySerializable[Array[Byte]] {
    override def toBytes(input: Array[Byte]): Array[Byte] = input

    override def fromBytes(bytes: Array[Byte]): Array[Byte] = bytes
  }
}

trait NodesKeyValueStorage extends SimpleMap[NodeHash, NodeEncoded, NodesKeyValueStorage]

class MerklePatriciaTrie[K, V] private (private val rootHash: Option[Array[Byte]],
  val nodeStorage: NodesKeyValueStorage)
  (implicit kSerializer: ByteArrayEncoder[K], vSerializer: ByteArraySerializable[V])
  extends SimpleMap[K, V, MerklePatriciaTrie[K, V]] {

  import MerklePatriciaTrie._

  lazy val getRootHash: Array[Byte] = rootHash.getOrElse(EmptyRootHash)

  /**
    * This function obtains the value asociated with the key passed, if there exists one.
    *
    * @param key
    * @return Option object with value if there exists one.
    * @throws io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException if there is any inconsistency in how the trie is build.
    */
  def get(key: K): Option[V] = {
    rootHash flatMap { rootId =>
      val keyNibbles = HexPrefix.bytesToNibbles(bytes = kSerializer.toBytes(key))
      val rootNode = getRootNode(rootId, nodeStorage)
      get(rootNode, keyNibbles).map(bytes => vSerializer.fromBytes(bytes))
    }
  }

  /**
    * This function inserts a (key-value) pair into the trie. If the key is already asociated with another value it is updated.
    *
    * @param key
    * @param value
    * @return New trie with the (key-value) pair inserted.
    * @throws io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException if there is any inconsistency in how the trie is build.
    */
  override def put(key: K, value: V): MerklePatriciaTrie[K, V] = {
    val keyNibbles = HexPrefix.bytesToNibbles(kSerializer.toBytes(key))
    rootHash map { rootId =>
      val root = getRootNode(rootId, nodeStorage)
      val NodeInsertResult(newRoot, nodesToRemoveFromStorage, nodesToUpdateInStorage) = put(root, keyNibbles, vSerializer.toBytes(value))
      val newRootHash = newRoot.hash
      val newSource = updateNodesInStorage(
        previousRootHash = getRootHash,
        newRoot = Some(newRoot),
        toRemove = nodesToRemoveFromStorage,
        toUpdate = nodesToUpdateInStorage,
        nodeStorage = nodeStorage)
      new MerklePatriciaTrie(Some(newRootHash), newSource)(kSerializer, vSerializer)
    } getOrElse {
      val newRoot = LeafNode(ByteString(keyNibbles), ByteString(vSerializer.toBytes(value)))
      val newRootHash = newRoot.hash
      new MerklePatriciaTrie(Some(newRootHash),
        updateNodesInStorage(getRootHash, Some(newRoot), Nil, Seq(newRoot), nodeStorage))
    }
  }

  /**
    * This function deletes a (key-value) pair from the trie. If no (key-value) pair exists with the passed trie then there's no effect on it.
    *
    * @param key
    * @return New trie with the (key-value) pair associated with the key passed deleted from the trie.
    * @throws io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException if there is any inconsistency in how the trie is build.
    */
  override def remove(key: K): MerklePatriciaTrie[K, V] = {
    rootHash map { rootId =>
      val keyNibbles = HexPrefix.bytesToNibbles(bytes = kSerializer.toBytes(key))
      val root = getRootNode(rootId, nodeStorage)
      remove(root, keyNibbles) match {
        case NodeRemoveResult(true, Some(newRoot), nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
          val newRootHash = newRoot.hash
          val afterDeletenodeStorage = updateNodesInStorage(
            previousRootHash = getRootHash,
            newRoot = Some(newRoot),
            toRemove = nodesToRemoveFromStorage,
            toUpdate = nodesToUpdateInStorage,
            nodeStorage = nodeStorage)
          new MerklePatriciaTrie(Some(newRootHash), afterDeletenodeStorage)(kSerializer, vSerializer)
        case NodeRemoveResult(true, None, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
          val afterDeletenodeStorage = updateNodesInStorage(
            previousRootHash = getRootHash,
            newRoot = None,
            toRemove = nodesToRemoveFromStorage,
            toUpdate = nodesToUpdateInStorage,
            nodeStorage = nodeStorage)
          new MerklePatriciaTrie(None, afterDeletenodeStorage)(kSerializer, vSerializer)
        case NodeRemoveResult(false, _, _, _) => this
      }
    } getOrElse {
      this
    }
  }

  /**
    * This function updates the KeyValueStore by deleting, updating and inserting new (key-value) pairs.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStore.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStore.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): MerklePatriciaTrie[K, V] = {
    val afterRemoval = toRemove.foldLeft(this) { (acc, key) => acc - key }
    toUpsert.foldLeft(afterRemoval) { (acc, item) => acc + item }
  }

  /**
   * Provides some proof sketch that the given `key` existEs in the MPT, in the form of a path from the root
   * node to the node corresponding to the given `key`.
   * We assume of course that the root hash is known.
   * <p/>
   * A proof sketch is made of [[io.iohk.ethereum.mpt.MerklePatriciaTrie.ProofStep ProofSteps]],
   * each of which provides information that we can use in verifying the proof.
   * <p/>
   * If the proof sketch contains at least the root node (first [[io.iohk.ethereum.mpt.MerklePatriciaTrie.ProofStep ProofStep]])
   * and the node that corresponds to the given `key` (last `ProofStep`), then by definition the proof
   * sketch is fully constructed.
   * In such a case we can easily verify that the key exists in the MPT. Otherwise, we do not know for sure.
   * <p/>
   * We return `None` if there is no root hash for this MPT.
   */
  def prove(key: K): Option[ProofSketch] = {
    for {
      theRootHash ← rootHash
    } yield {
      val keyNibbles = key2nibbles(key)
      val rootNode = getRootNode(theRootHash, nodeStorage)

      @tailrec
      def walk(currentNode: MptNode, currentNibbles: Array[Byte], currentProofSteps: List[ProofStep]): List[ProofStep] = {
        currentNode match {
          case leafNode: LeafNode ⇒
            if(sameNibblesForKey(leafNode, currentNibbles)) {
              // end of chain, we got the key, return the full path.
              val proofHash = leafNode.hash
              val proofNibbles = currentNibbles
              val proofStep = ProofStep(proofNibbles, proofHash)
              val nextProofSteps = proofStep :: currentProofSteps

              nextProofSteps
            }
            else {
              // the key was not found, just return our path so far
              currentProofSteps
            }

          case extensionNode @ ExtensionNode(sharedKey, _, _, _) ⇒
            if(currentNibbles.length >= sharedKey.length) {
              val (nibblesPrefix, nibblesSuffix) = currentNibbles.splitAt(sharedKey.length)

              if(sameNibblesForSharedKey(extensionNode, nibblesPrefix)) {
                // decode the `next` node
                val nextNode = getNextNode(extensionNode, nodeStorage)
                val nextNibbles = nibblesSuffix

                val proofHash = extensionNode.hash
                val proofNibbles = nibblesPrefix
                val proofStep = ProofStep(proofNibbles, proofHash)
                val nextProofSteps = proofStep :: currentProofSteps

                walk(nextNode, nextNibbles, nextProofSteps)
              }
              else {
                // the key was not found, just return our path so far
                currentProofSteps
              }
            }
            else {
              // the key was not found, just return our path so far
              currentProofSteps
            }

          case branchNode: BranchNode ⇒
            if(currentNibbles.length == 0) {
              // We have a branch node and no (remaining) key to compare ...
              // so probably that's it ....
              // FIXME check "probably" above
              currentProofSteps
            }
            else {
              // Use the first nibble to find the next (child) node
              val index = currentNibbles(0)
              val childNodeOpt = getChild(branchNode, index, nodeStorage)
              childNodeOpt match {
                case None ⇒
                  // Not found ...
                  currentProofSteps

                case Some(nextNode) ⇒
                  val nextNibbles = currentNibbles.drop(1)

                  val proofHash = branchNode.hash
                  val proofNibbles = Array(index)
                  val proofStep = ProofStep(proofNibbles, proofHash)
                  val nextProofSteps = proofStep :: currentProofSteps

                  walk(nextNode, nextNibbles, nextProofSteps)
              }
            }
        }
      }

      // Walk from root to the node of the given key and record all intermediate node hashes
      // in a list.
      val steps = walk(rootNode, keyNibbles, Nil).reverse

      ProofSketch(steps)
    }
  }

  protected def rlpDecodeNode(bytes: NodeStorage.NodeEncoded): MptNode = rlp.decode[MptNode](bytes)

  protected def findNode(parentStepOpt: Option[ProofStep], currentStep: ProofStep): Option[MptNode] = {
    val currentHash = currentStep.hashToByteString

    // 1. Try find the node directly in the storage, based on its hash
    nodeStorage.get(currentHash) match {
      case Some(rlpEncodedNode) ⇒
        val node = rlpDecodeNode(rlpEncodedNode)
        Some(node)

      case None ⇒
        parentStepOpt match {
          case None ⇒
            // Too bad, no parent to help us
            None

          case Some(parentStep) ⇒
            val parentHash = parentStep.hashToByteString
            // 2. It must be stored in the parent node, if its size is small
            nodeStorage.get(parentHash) match {
              case None ⇒
                None // too bad, not even the parent node there!

              case Some(rlpEncodedParentNode) ⇒
                val parentNode = rlpDecodeNode(rlpEncodedParentNode)

                parentNode match {
                  case node: LeafNode ⇒
                    // Too bad, the parent is a Leaf node, so nothing else to find here
                    None

                  case node: BranchNode ⇒
                    val nibblesToCheck = parentStep.nibbles
                    assert(nibblesToCheck.length == 1)
                    val index = nibblesToCheck(0)
                    node.children(index) match {
                      case None ⇒
                        // Too bad, no child in that position!
                        None
                      case Some(Left(childNodeHash)) ⇒
                        // Too bad! In that position a child is represented by a hash
                        // but in such a case we should have found the child in the storage !
                        None
                      case Some(Right(mptNode)) ⇒
                        if(sameHashForProof(mptNode, currentStep.hash)) {
                          Some(mptNode)
                        }
                        else {
                          // Too bad, the hash that the proof has provided does not match.
                          None
                        }
                    }

                  case node: ExtensionNode ⇒
                    node.next match {
                      case Left(childNodeHash) ⇒
                        // Too bad! The child is represented by a hash but in this  case we
                        // should have found the child in the storage directly in the first place !
                        None

                      case Right(mptNode) ⇒
                        if(sameHashForProof(mptNode, currentStep.hash)) {
                          Some(mptNode)
                        }
                        else {
                          // Too bad, the hash that the proof has provided does not match.
                          None
                        }
                    }
                }
            }
        }
    }
  }

  /**
   * Verifies a proof.
   *
   * <p/>
   * This initial implementation is not meant as a fully correct verification; rather
   * it is a first, non-general approximation that has been developed to ease testing
   * of the proof algorithm. Also see the following note.
   *
   * @note
   * Fundamentally, proof and verification are the same algorithm that walks a tree, the difference
   * being in the action done at each node. Proof accumulates node hashes, verification checks those
   * (previously accumulated) hashes. So, in true modular style, a unified proof/verification algorithm
   * is a composition of tree traversal plus the action.
   *
   * @return `true` iff the proof can be verified.
   */
  def verify(proof: ProofSketch): Boolean = {
    def _verifyStep(proofStep: ProofStep, node: MptNode): Boolean = {
      val proofHash = proofStep.hash
      val proofNibbles = proofStep.nibbles

      // 1. Check the hash
      val realHash = node.hash
      require(proofHash sameElements realHash)

      // 2. Check the key part (="path")
      node match {
        case BranchNode(children, terminatorOpt, _, _) ⇒
          // By design, in a branch node only one nibble is used for routing
          proofNibbles.length == 1

        case node @ ExtensionNode(sharedKey, next, _, _) ⇒
          sameNibblesForSharedKey(node, proofNibbles)

        case node @ LeafNode(key, value, _, _) ⇒
          sameNibblesForKey(node, proofNibbles)
      }
    }

    @tailrec
    def _verifySteps(parentStepOpt: Option[ProofStep], nodeStep: ProofStep, nextSteps: List[ProofStep]): Boolean = {
      val nodeOpt = findNode(parentStepOpt, nodeStep)
      nodeOpt match {
        case None ⇒
          false

        case Some(node) ⇒
          if(_verifyStep(nodeStep, node)) {
            nextSteps match {
              case Nil ⇒
                // nothing else to do, success !
                true
              case newNodeStep :: newNextSteps ⇒
                val newParentStepOpt = Some(nodeStep)
                _verifySteps(newParentStepOpt, newNodeStep, newNextSteps)
            }
          }
          else false
      }
    }

    val steps = proof.steps
    steps match {
      case Nil ⇒
        false // we know nothing since we don't have even a single proof step

      case rootStep :: nextSteps ⇒
        _verifySteps(None, rootStep, nextSteps)
    }
  }

  /** Serializes a key to a byte array. */
  private[this] def key2bytes(key: K): Array[Byte] = kSerializer.toBytes(key)

  private[this] def bytes2nibbles(bytes: Array[Byte]): Array[Byte] = HexPrefix.bytesToNibbles(bytes)

  /**
   * Transforms a key to a nibbles byte array, using
   * [[io.iohk.ethereum.mpt.HexPrefix#bytesToNibbles(byte[]) bytesToNibbles]].
   */
  private[this] def key2nibbles(key: K): Array[Byte] = bytes2nibbles(key2bytes(key))

  @tailrec
  private def get(node: MptNode, searchKey: Array[Byte]): Option[Array[Byte]] = node match {
    case LeafNode(key, value, _, _) =>
      if (key.toArray[Byte] sameElements searchKey) Some(value.toArray[Byte]) else None
    case extNode@ExtensionNode(sharedKey, _, _, _) =>
      val (commonKey, remainingKey) = searchKey.splitAt(sharedKey.length)
      if (searchKey.length >= sharedKey.length && (sharedKey sameElements commonKey)) {
        val nextNode = getNextNode(extNode, nodeStorage)
        get(nextNode, remainingKey)
      }
      else None
    case branch@BranchNode(_, terminator, _, _) =>
      if (searchKey.isEmpty) terminator.map(_.toArray[Byte])
      else getChild(branch, searchKey(0), nodeStorage) match {
        case Some(child) => get(child, searchKey.slice(1, searchKey.length))
        case None => None
      }
  }

  private def put(node: MptNode, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = node match {
    case leafNode: LeafNode => putInLeafNode(leafNode, searchKey, value)
    case extensionNode: ExtensionNode => putInExtensionNode(extensionNode, searchKey, value)
    case branchNode: BranchNode => putInBranchNode(branchNode, searchKey, value)
  }

  private def putInLeafNode(node: LeafNode, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    val LeafNode(existingKey, storedValue, _, _) = node
    matchingLength(existingKey.toArray[Byte], searchKey) match {
      case ml if ml == existingKey.length && ml == searchKey.length =>
        // We are trying to insert a leaf node that has the same key as this one but different value so we need to
        // replace it
        val newLeafNode = LeafNode(existingKey, ByteString(value))
        NodeInsertResult(
          newNode = newLeafNode,
          toDeleteFromStorage = Seq(node),
          toUpdateInStorage = Seq(newLeafNode)
        )
      case 0 =>
        // There is no common prefix between the node which means that we need to replace this leaf node
        val (temporalBranchNode, maybeNewLeaf) =
          if (existingKey.isEmpty) // This node has no key so it should be stored as branch's value
            BranchNode.withValueOnly(storedValue.toArray[Byte]) -> None
          else {
            // The leaf should be put inside one of new branch nibbles
            val newLeafNode = LeafNode(existingKey.tail, storedValue)
            BranchNode.withSingleChild(existingKey(0), newLeafNode, None) -> Some(newLeafNode)
          }
        val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage, toUpdateInStorage) = put(temporalBranchNode, searchKey, value)
        NodeInsertResult(
          newNode = newBranchNode,
          toDeleteFromStorage = node +: toDeleteFromStorage.filterNot(_ == temporalBranchNode),
          toUpdateInStorage = maybeNewLeaf.toList ++ toUpdateInStorage
        )
      case ml =>
        // Partially shared prefix, we replace the leaf with an extension and a branch node
        val (searchKeyPrefix, searchKeySuffix) = searchKey.splitAt(ml)
        val temporalNode =
          if (ml == existingKey.length) BranchNode.withValueOnly(storedValue.toArray[Byte])
          else LeafNode(existingKey.drop(ml), storedValue)
        val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage, toUpdateInStorage) = put(temporalNode, searchKeySuffix, value)
        val newExtNode = ExtensionNode(ByteString(searchKeyPrefix), newBranchNode)
        NodeInsertResult(
          newNode = newExtNode,
          toDeleteFromStorage = node +: toDeleteFromStorage.filterNot(_ == temporalNode),
          toUpdateInStorage = newExtNode +: toUpdateInStorage
        )
    }
  }

  private def putInExtensionNode(extensionNode: ExtensionNode, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    val ExtensionNode(sharedKey, next, _, _) = extensionNode
    matchingLength(sharedKey.toArray[Byte], searchKey) match {
      case 0 =>
        // There is no common prefix with the node which means we have to replace it for a branch node
        val sharedKeyHead = sharedKey(0)
        val (temporalBranchNode, maybeNewExtNode) = {
          // Direct extension, we just replace the extension with a branch
          if (sharedKey.length == 1) BranchNode.withSingleChild(sharedKeyHead, next, None) -> None
          else {
            // The new branch node will have an extension that replaces current one
            val newExtNode = ExtensionNode(sharedKey.tail, next)
            BranchNode.withSingleChild(sharedKeyHead, newExtNode, None) -> Some(newExtNode)
          }
        }
        val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage, toUpdateInStorage) = put(temporalBranchNode, searchKey, value)
        NodeInsertResult(
          newNode = newBranchNode,
          toDeleteFromStorage = extensionNode +: toDeleteFromStorage.filterNot(_ == temporalBranchNode),
          toUpdateInStorage = maybeNewExtNode.toList ++ toUpdateInStorage
        )
      case ml if ml == sharedKey.length =>
        // Current extension node's key is a prefix of the one being inserted, so we insert recursively on the extension's child
        val NodeInsertResult(newChild: BranchNode, toDeleteFromStorage, toUpdateInStorage) =
          put(getNextNode(extensionNode, nodeStorage), searchKey.drop(ml), value)
        val newExtNode = ExtensionNode(sharedKey, newChild)
        NodeInsertResult(
          newNode = newExtNode,
          toDeleteFromStorage = extensionNode +: toDeleteFromStorage,
          toUpdateInStorage = newExtNode +: toUpdateInStorage
        )
      case ml =>
        // Partially shared prefix, we have to replace the node with an extension with the shared prefix
        val (sharedKeyPrefix, sharedKeySuffix) = sharedKey.splitAt(ml)
        val temporalExtensionNode = ExtensionNode(sharedKeySuffix, next)
        val NodeInsertResult(newBranchNode: BranchNode, toDeleteFromStorage, toUpdateInStorage) = put(temporalExtensionNode, searchKey.drop(ml), value)
        val newExtNode = ExtensionNode(sharedKeyPrefix, newBranchNode)
        NodeInsertResult(
          newNode = newExtNode,
          toDeleteFromStorage = extensionNode +: toDeleteFromStorage.filterNot(_ == temporalExtensionNode),
          toUpdateInStorage = newExtNode +: toUpdateInStorage
        )
    }
  }

  private def putInBranchNode(branchNode: BranchNode, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    val BranchNode(children, _, _, _) = branchNode
    if (searchKey.isEmpty) {
      // The key is empty, the branch node should now be a terminator node with the new value asociated with it
      val newBranchNode = BranchNode(children, Some(ByteString(value)))
      NodeInsertResult(
        newNode = newBranchNode,
        toDeleteFromStorage = Seq(branchNode),
        toUpdateInStorage = Seq(newBranchNode)
      )
    }
    else {
      // Non empty key, we need to insert the value in the correct branch node's child
      val searchKeyHead: Int = searchKey(0)
      val searchKeyRemaining = searchKey.tail
      if (children(searchKeyHead).isDefined) {
        // The associated child is not empty, we recursively insert in that child
        val NodeInsertResult(changedChild, toDeleteFromStorage, toUpdateInStorage) =
          put(getChild(branchNode, searchKeyHead, nodeStorage).get, searchKeyRemaining, value)
        val newBranchNode = branchNode.updateChild(searchKeyHead, changedChild)
        NodeInsertResult(
          newNode = newBranchNode,
          toDeleteFromStorage = branchNode +: toDeleteFromStorage,
          toUpdateInStorage = newBranchNode +: toUpdateInStorage
        )
      }
      else {
        // The associated child is empty, we just replace it with a leaf
        val newLeafNode = LeafNode(ByteString(searchKeyRemaining), ByteString(value))
        val newBranchNode = branchNode.updateChild(searchKeyHead, newLeafNode)
        NodeInsertResult(
          newNode = newBranchNode,
          toDeleteFromStorage = Seq(branchNode),
          toUpdateInStorage = Seq(newLeafNode, newBranchNode)
        )
      }
    }
  }

  private def remove(node: MptNode, searchKey: Array[Byte]): NodeRemoveResult = node match {
    case leafNode: LeafNode => removeFromLeafNode(leafNode, searchKey)
    case extensionNode: ExtensionNode => removeFromExtensionNode(extensionNode, searchKey)
    case branchNode: BranchNode => removeFromBranchNode(branchNode, searchKey)
  }

  private def removeFromBranchNode(node: BranchNode, searchKey: Array[Byte]): NodeRemoveResult = (node, searchKey.isEmpty) match {
    // They key matches a branch node but it's value doesn't match the key
    case (BranchNode(_, None, _, _), true) => NodeRemoveResult(hasChanged = false, newNode = None)
    // We want to delete Branch node value
    case (BranchNode(children, _, _, _), true) =>
      // We need to remove old node and fix it because we removed the value
      val fixedNode = fix(BranchNode(children, None), nodeStorage, Nil)
      NodeRemoveResult(hasChanged = true, newNode = Some(fixedNode), toDeleteFromStorage = Seq(node), toUpdateInStorage = Seq(fixedNode))
    case (branchNode@BranchNode(children, optStoredValue, _, _), false) =>
      // We might be trying to remove a node that's inside one of the 16 mapped nibbles
      val searchKeyHead = searchKey(0)
      getChild(branchNode, searchKeyHead, nodeStorage) map { child =>
        // Child has been found so we try to remove it
        remove(child, searchKey.tail) match {
          case NodeRemoveResult(true, maybeNewChild, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
            // Something changed in a child so we need to fix
            val nodeToFix = maybeNewChild map { newChild =>
              branchNode.updateChild(searchKeyHead, newChild)
            } getOrElse {
              BranchNode(children.updated(searchKeyHead, None), optStoredValue)
            }
            val fixedNode = fix(nodeToFix, nodeStorage, nodesToUpdateInStorage)
            NodeRemoveResult(
              hasChanged = true,
              newNode = Some(fixedNode),
              toDeleteFromStorage = node +: nodesToRemoveFromStorage,
              toUpdateInStorage = fixedNode +: nodesToUpdateInStorage)
          // No removal made on children, so we return without any change
          case NodeRemoveResult(false, _, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
            NodeRemoveResult(
              hasChanged = false,
              newNode = None,
              toDeleteFromStorage = nodesToRemoveFromStorage,
              toUpdateInStorage = nodesToUpdateInStorage)
        }
      } getOrElse {
        // Child not found in this branch node, so key is not present
        NodeRemoveResult(hasChanged = false, newNode = None)
      }
  }

  private def removeFromLeafNode(leafNode: LeafNode, searchKey: Array[Byte]): NodeRemoveResult = {
    val LeafNode(existingKey, _, _, _) = leafNode
    if (existingKey sameElements searchKey) {
      // We found the node to delete
      NodeRemoveResult(hasChanged = true, newNode = None, toDeleteFromStorage = Seq(leafNode))
    }
    else NodeRemoveResult(hasChanged = false, newNode = None)
  }

  private def removeFromExtensionNode(extensionNode: ExtensionNode, searchKey: Array[Byte]): NodeRemoveResult = {
    val ExtensionNode(sharedKey, _, _, _) = extensionNode
    val cp = matchingLength(sharedKey.toArray[Byte], searchKey)
    if (cp == sharedKey.length) {
      // A child node of this extension is removed, so move forward
      remove(getNextNode(extensionNode, nodeStorage), searchKey.drop(cp)) match {
        case NodeRemoveResult(true, maybeNewChild, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
          // If we changed the child, we need to fix this extension node
          maybeNewChild map { newChild =>
            val toFix = ExtensionNode(sharedKey, newChild)
            val fixedNode = fix(toFix, nodeStorage, nodesToUpdateInStorage)
            NodeRemoveResult(
              hasChanged = true,
              newNode = Some(fixedNode),
              toDeleteFromStorage = extensionNode +: nodesToRemoveFromStorage,
              toUpdateInStorage = fixedNode +: nodesToUpdateInStorage)
          } getOrElse {
            throw new MPTException("A trie with newRoot extension should have at least 2 values stored")
          }
        case NodeRemoveResult(false, _, nodesToRemoveFromStorage, nodesToUpdateInStorage) =>
          NodeRemoveResult(
            hasChanged = false,
            newNode = None,
            toDeleteFromStorage = nodesToRemoveFromStorage,
            toUpdateInStorage = nodesToUpdateInStorage)
      }
    }
    else NodeRemoveResult(hasChanged = false, newNode = Some(extensionNode))
  }

  /**
    * Given a node which may be in an invalid state, fix it such that it is then in a valid state. Invalid state means:
    *   - Branch node where there is only a single entry;
    *   - Extension node followed by anything other than a Branch node.
    *
    * @param node         that may be in an invalid state.
    * @param nodeStorage  to obtain the nodes referenced in the node that may be in an invalid state.
    * @param notStoredYet to obtain the nodes referenced in the node that may be in an invalid state,
    *                     if they were not yet inserted into the nodeStorage.
    * @return fixed node.
    * @throws io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException if there is any inconsistency in how the trie is build.
    */
  @tailrec
  private def fix(node: MptNode, nodeStorage: NodesKeyValueStorage, notStoredYet: Seq[MptNode]): MptNode = node match {
    case BranchNode(children, optStoredValue, _, _) =>
      val usedIndexes = children.indices.foldLeft[Seq[Int]](Nil) {
        (acc, i) =>
          if (children(i).isDefined) i +: acc else acc
      }
      (usedIndexes, optStoredValue) match {
        case (Nil, None) => throw new MPTException("Branch with no subvalues")
        case (index :: Nil, None) =>
          val temporalExtNode = ExtensionNode(ByteString(index.toByte), children(index).get)
          fix(temporalExtNode, nodeStorage, notStoredYet)
        case (Nil, Some(value)) => LeafNode(ByteString.empty, value)
        case _ => node
      }
    case extensionNode@ExtensionNode(sharedKey, _, _, _) =>
      val nextNode = extensionNode.next match {
        case Left(nextHash) =>
          // If the node is not in the extension node then it might be a node to be inserted at the end of this remove
          // so we search in this list too
          notStoredYet.find(n => n.hash sameElements nextHash).getOrElse(
            getNextNode(extensionNode, nodeStorage) // We search for the node in the db
          )
        case Right(nextNodeOnExt) => nextNodeOnExt
      }
      val newNode = nextNode match {
        // Compact Two extensions into one
        case ExtensionNode(subSharedKey, subNext, _, _) => ExtensionNode(sharedKey ++ subSharedKey, subNext)
        // Compact the extension and the leaf into the same leaf node
        case LeafNode(subRemainingKey, subValue, _, _) => LeafNode(sharedKey ++ subRemainingKey, subValue)
        // It's ok
        case _: BranchNode => node
      }
      newNode
    case _ => node
  }
}

