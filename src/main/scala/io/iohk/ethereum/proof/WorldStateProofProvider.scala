package io.iohk.ethereum.proof

import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, UInt256}
import io.iohk.ethereum.domain.Address.hashedAddressEncoder

import scala.collection.SortedMap

trait WorldStateStorage {
  def isWorldStateAvailable(rootHash: Array[Byte]): Boolean // TODO implement this trait somewhere ?
}

class WorldStateProofProvider(worldState: WorldStateStorage) {

  def getAccountProof(
      rootHash: Array[Byte],
      address: Address,
      storageKeys: Seq[StorageProofKey]
  ): Option[WorldStateProof] = {
    if (!worldState.isWorldStateAvailable(rootHash)) {
      None
    } else {
      val accountHash: Array[Byte] = hashedAddressEncoder.toBytes(address)
      val accountProof: Proof[ByteString] = newAccountStateTrie(accountHash).getValueWithProof(accountHash)
      accountProof.value
        .map(encodeRLP)
        .map(StateTrieAccountValue.readFrom)
        .map { account =>
          val storageProofs = getStorageProofs(account, storageKeys)
          WorldStateProof(account, accountProof, storageProofs)
        }
    }
  }

  def getStorageProofs(
      account: StateTrieAccountValue,
      storageKeys: Seq[StorageProofKey]
  ): SortedMap[BigInt, Proof[ByteString]] = {
    val storageTrie = newAccountStorageTrie(account.storageRoot)
    val init = SortedMap.empty[BigInt, Proof[ByteString]]
    storageKeys.map { key =>
      val hash = ??? // TODO Hash.hash(key.toBytes()) ?
      key -> storageTrie.getValueWithProof(???)
    }.toMap
  }

  def newAccountStateTrie(rootHash: Array[Byte]): StoredMerklePatriciaTrie[Array[Byte], ByteString] = {
    // TODO in besu we create StoredMerklePatriciaTrie using worldState, rootHash, and identity serialize/deserialize
    ???
  }

  def newAccountStorageTrie(storageRoot: ByteString): StoredMerklePatriciaTrie[Array[Byte], ByteString] = ???

  def encodeRLP: ByteString => RLPInput = ??? // TODO translate from besu RLPInput input
}
