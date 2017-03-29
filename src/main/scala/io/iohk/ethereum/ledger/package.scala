package io.iohk.ethereum

import io.iohk.ethereum.db.storage.{EvmCodeStorage, NodeStorage}
import io.iohk.ethereum.db.storage.EvmCodeStorage.{Code, CodeHash}
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import scala.language.implicitConversions

package object ledger {

  object MPTWrappedMap {
    implicit def wrappedMpt[K, V](mpt: MerklePatriciaTrie[K, V]): MPTWrappedMap[K, V] = new MPTWrappedMap[K, V](mpt)
  }

  class MPTWrappedMap[K, V](mpt: MerklePatriciaTrie[K, V]) extends WrappedMap[K, V, MerklePatriciaTrie[K, V]] {
    override val wrapped: MerklePatriciaTrie[K, V] = mpt

    override def get(key: K): Option[V] = mpt.get(key)

    override def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): WrappedMap[K, V, MerklePatriciaTrie[K, V]] =
      new MPTWrappedMap[K, V](mpt.update(toRemove, toUpsert))
  }

  object EvmCodeStorageWrappedMap {
    implicit def wrappedEmvCodeStorage[K, V](storage: EvmCodeStorage): EvmCodeStorageWrappedMap = new EvmCodeStorageWrappedMap(storage)
  }

  class EvmCodeStorageWrappedMap(storage: EvmCodeStorage) extends WrappedMap[CodeHash, Code, EvmCodeStorage] {
    override val wrapped: EvmCodeStorage = storage

    override def get(key: CodeHash): Option[Code] = storage.get(key)

    override def update(toRemove: Seq[CodeHash], toUpsert: Seq[(CodeHash, Code)]): WrappedMap[CodeHash, Code, EvmCodeStorage] =
      new EvmCodeStorageWrappedMap(storage.update(toRemove, toUpsert))
  }

  object NodeStorageWrappedMap {
    implicit def wrappedNodeCodeStorage[K, V](storage: NodeStorage): NodeStorageWrappedMap = new NodeStorageWrappedMap(storage)
  }

  class NodeStorageWrappedMap(storage: NodeStorage) extends WrappedMap[NodeHash, NodeEncoded, NodeStorage] {
    override val wrapped: NodeStorage = storage

    override def get(key: NodeHash): Option[NodeEncoded] = storage.get(key)

    override def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): WrappedMap[NodeHash, NodeEncoded, NodeStorage] =
      new NodeStorageWrappedMap(storage.update(toRemove, toUpsert))
  }
}
