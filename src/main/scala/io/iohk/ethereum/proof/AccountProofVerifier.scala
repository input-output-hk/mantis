package io.iohk.ethereum.proof

import akka.util.ByteString
import cats.syntax.either._
import cats.syntax.functor._
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.{NodeStorage, SerializingMptStorage, StateStorage}
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.jsonrpc.{AccountProofError, InvalidAccountProofForAccount, InvalidAccountProofOrRootHash}
import io.iohk.ethereum.mpt.{MerklePatriciaTrie, MptNode}

object AccountProofVerifier {

  def verifyProof(
      stateTrieRoot: Array[Byte],
      address: Address,
      proof: Vector[MptNode]
  ): Either[AccountProofError, Unit] = {
    val emptyStorage = new NodeStorage(EphemDataSource())
    val nodeStorage = proof.foldLeft(emptyStorage) { case (storage, node) =>
      storage.put(ByteString(node.hash), node.encode)
    }
    val mptStore: SerializingMptStorage = StateStorage.mptStorageFromNodeStorage(nodeStorage)

    Either
      .catchNonFatal {
        MerklePatriciaTrie[Address, Account](
          rootHash = stateTrieRoot,
          source = mptStore
        )(Address.hashedAddressEncoder, Account.accountSerializer)
      }
      .leftMap(_ => InvalidAccountProofOrRootHash)
      .flatMap { trie =>
        Either
          .catchNonFatal { trie.get(address) }
          .leftMap(_ => InvalidAccountProofForAccount)
      }
      .void
  }
}
