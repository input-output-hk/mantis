package io.iohk.ethereum.proof

import akka.util.ByteString
import cats.syntax.either._
import cats.syntax.functor._
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.{NodeStorage, SerializingMptStorage, StateStorage}
import io.iohk.ethereum.jsonrpc.{MptProofError, KeyNotFoundInRebuidMpt, UnableRebuildMpt}
import io.iohk.ethereum.mpt.{ByteArrayEncoder, ByteArraySerializable, MerklePatriciaTrie, MptNode}

object MptProofVerifier {

  def verifyProof[K, V](
      rootHash: Array[Byte],
      key: K,
      proof: Vector[MptNode]
  )(implicit kSer: ByteArrayEncoder[K], vSer: ByteArraySerializable[V]): Either[MptProofError, Unit] = {
    val mptStore = mkStorage(proof)
    rebuildMpt(rootHash, mptStore)(kSer, vSer)
      .leftMap(_ => UnableRebuildMpt)
      .flatMap { trie =>
        getKey(key, trie)
          .leftMap(_ => KeyNotFoundInRebuidMpt)
      }
      .void
  }

  private def mkStorage[V, K](proof: Vector[MptNode]) = {
    val emptyStorage = new NodeStorage(EphemDataSource())
    val nodeStorage = proof.foldLeft(emptyStorage) { case (storage, node) =>
      storage.put(ByteString(node.hash), node.encode)
    }
    val mptStore: SerializingMptStorage = StateStorage.mptStorageFromNodeStorage(nodeStorage)
    mptStore
  }

  private def rebuildMpt[V, K](rootHash: Array[Byte], storage: SerializingMptStorage)(implicit
      kSer: ByteArrayEncoder[K],
      vSer: ByteArraySerializable[V]
  ): Either[Throwable, MerklePatriciaTrie[K, V]] =
    Either.catchNonFatal {
      MerklePatriciaTrie[K, V](
        rootHash = rootHash,
        source = storage
      )
    }

  private def getKey[V, K](key: K, trie: MerklePatriciaTrie[K, V]): Either[Throwable, Option[V]] =
    Either.catchNonFatal {
      trie.get(key)
    }
}
