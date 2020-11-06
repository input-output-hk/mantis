package io.iohk.ethereum.proof

import akka.util.ByteString
import io.iohk.ethereum.domain.UInt256

/** An individual node used to prove a path down a merkle-patricia-tree */
case class ProofNode(b: ByteString)

/** The key used to get the storage slot in its account tree */
case class StorageProofKey(v: BigInt)

final case class StateTrieAccountValue(
    balance: BigInt,
    codeHash: ByteString,
    nonce: UInt256,
    storageRoot: ByteString,
    accountProof: Seq[ProofNode]
)

class WorldStateProof {
  def stateTrieAccountValue(): StateTrieAccountValue = ???
  def storageKeys(): Seq[StorageProofKey] = ???
  def storageValue(key: StorageProofKey): BigInt = ???
  def storageProof(key: StorageProofKey): Seq[ProofNode] = ???
}
