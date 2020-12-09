package io.iohk.ethereum.proof

import akka.util.ByteString
import cats.syntax.either._
import cats.syntax.functor._
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.{ArchiveNodeStorage, NodeStorage, SerializingMptStorage}
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.jsonrpc.{AccountProofError, InvalidAccountProofForAccount, InvalidAccountProofOrRootHash}
import io.iohk.ethereum.mpt.{MerklePatriciaTrie, MptNode}

object AccountProofVerifier {

  def verifyProof(
      stateTrieRoot: ByteString,
      address: Address,
      proof: Vector[MptNode]
  ): Either[AccountProofError, Unit] = {
    val storage = new ArchiveNodeStorage(new NodeStorage(EphemDataSource()))
    proof.foreach { node =>
      storage.put(ByteString(node.hash), node.encode)
    }

    Either
      .catchNonFatal {
        MerklePatriciaTrie[Address, Account](
          rootHash = stateTrieRoot.toArray,
          source = new SerializingMptStorage(storage)
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
