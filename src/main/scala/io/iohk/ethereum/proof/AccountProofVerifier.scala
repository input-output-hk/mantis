package io.iohk.ethereum.proof

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.{ArchiveNodeStorage, NodeStorage, SerializingMptStorage}
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.jsonrpc.{AccountProofError, InvalidAccountProofForAccount, InvalidAccountProofOrRootHash}
import io.iohk.ethereum.mpt.{MerklePatriciaTrie, MptNode}

import scala.util.Try

object AccountProofVerifier {

  def verifyProof(
      stateTrieRoot: ByteString,
      address: Address,
      proof: Vector[MptNode]
  ): Either[AccountProofError, Unit] = {
    val storage = new ArchiveNodeStorage(new NodeStorage(EphemDataSource()))
    println(s"\n verifyProof stateTrieRoot ${stateTrieRoot}")
    println(s"verifyProof address $address")
    println(s"verifyProof proof $proof")
    proof.foreach { node =>
      println(s"\n verifyProof fill trie ByteString(node.hash) ${ByteString(node.hash)}")
      storage.put(ByteString(node.hash), node.encode)
    }

    for {
      trie <- Try {
        val mpt = new SerializingMptStorage(storage)
        val mptTrie = MerklePatriciaTrie[Address, Account](
          rootHash = stateTrieRoot.toArray,
          source = mpt
        )(Address.hashedAddressEncoder, Account.accountSerializer)
        println(s"verifyProof mk MPT root hash ${mptTrie.getRootHash.mkString(", ")}")
        mptTrie
      }.toEither.left.map(_ => InvalidAccountProofOrRootHash)
      _ <- Try(trie.get(address).get).toEither.left.map(_ => InvalidAccountProofForAccount)
    } yield ()
  }
}
