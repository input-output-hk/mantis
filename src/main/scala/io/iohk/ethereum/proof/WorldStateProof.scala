package io.iohk.ethereum.proof

import akka.util.ByteString
import io.iohk.ethereum.domain.UInt256

import scala.collection.{SortedMap, SortedSet}

/** An individual node used to prove a path down a merkle-patricia-tree */
final case class ProofNode(b: ByteString) extends AnyVal

/** The key used to get the storage slot in its account tree */
final case class StorageProofKey(v: BigInt) extends AnyVal

final case class StateTrieAccountValue(
    balance: BigInt,
    codeHash: ByteString,
    nonce: UInt256,
    storageRoot: ByteString
)

trait RLPInput // TODO translate from besu StateTrieAccountValue readFrom

object StateTrieAccountValue {
  def readFrom(in: RLPInput): StateTrieAccountValue = ??? // TODO translate from besu StateTrieAccountValue readFrom
}

final case class WorldStateProof(
    stateTrieAccountValue: StateTrieAccountValue,
    accountProof: Proof[ByteString],
    storageProofs: SortedMap[BigInt, Proof[ByteString]]
) {
  val ZERO = BigInt(0)

  def storageKeys(): SortedSet[BigInt] = storageProofs.keySet

  def storageValue(key: BigInt): UInt256 = {
    // storageProofs.get(key).fold(UInt256.Zero)(k => UInt256(k.value)) // TODO UInt256 RLP ?
    ???
  }

  def storageProof(key: BigInt): List[ByteString] =
    storageProofs
      .get(key)
      .fold(List.empty[ByteString])(k => k.proofRelatedNodes)

  def getAccountProof: List[ByteString] = accountProof.proofRelatedNodes
}
