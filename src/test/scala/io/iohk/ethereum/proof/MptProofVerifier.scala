package io.iohk.ethereum.proof

import akka.util.ByteString
import cats.syntax.either._
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.{NodeStorage, SerializingMptStorage, StateStorage}
import io.iohk.ethereum.jsonrpc.ProofService.MptProofError
import io.iohk.ethereum.mpt.{ByteArrayEncoder, ByteArraySerializable, MerklePatriciaTrie, MptNode}
import io.iohk.ethereum.proof.ProofVerifyResult.{InvalidProof, ValidProof}

sealed trait ProofVerifyResult
object ProofVerifyResult {
  case object ValidProof extends ProofVerifyResult
  case class InvalidProof(reason: MptProofError) extends ProofVerifyResult
}

object MptProofVerifier {

  def verifyProof[K, V](
      rootHash: Array[Byte],
      key: K,
      proof: Vector[MptNode]
  )(implicit kSer: ByteArrayEncoder[K], vSer: ByteArraySerializable[V]): ProofVerifyResult = {
    val mptStore = mkStorage(proof)
    rebuildMpt(rootHash, mptStore)(kSer, vSer)
      .flatMap( trie => getKey(key, trie) )
      .fold(InvalidProof.apply, _ => ValidProof)
  }

  private def mkStorage[V, K](proof: Vector[MptNode]): SerializingMptStorage = {
    val emptyStorage = new NodeStorage(EphemDataSource())
    val nodeStorage = proof.foldLeft(emptyStorage) { case (storage, node) =>
      storage.put(ByteString(node.hash), node.encode)
    }
    StateStorage.mptStorageFromNodeStorage(nodeStorage)
  }

  private def rebuildMpt[V, K](rootHash: Array[Byte], storage: SerializingMptStorage)(implicit
      kSer: ByteArrayEncoder[K],
      vSer: ByteArraySerializable[V]
  ): Either[MptProofError, MerklePatriciaTrie[K, V]] =
    Either.catchNonFatal {
      MerklePatriciaTrie[K, V](
        rootHash = rootHash,
        source = storage
      )
    }.leftMap(_ => MptProofError.UnableRebuildMpt)

  private def getKey[V, K](key: K, trie: MerklePatriciaTrie[K, V]): Either[MptProofError, Option[V]] =
    Either
      .catchNonFatal(trie.get(key))
      .leftMap(_ => MptProofError.KeyNotFoundInRebuidMpt)
}
