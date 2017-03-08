package io.iohk.ethereum.db.storage

import java.nio.ByteBuffer

import akka.util.ByteString
import boopickle.CompositePickler
import boopickle.Default._
import io.iohk.ethereum.blockchain.sync.FastSyncController._
import io.iohk.ethereum.db.dataSource.DataSource

object FastSyncStateStorage {

  val syncStateKey: String = "fast-sync-state"

}

class FastSyncStateStorage(val dataSource: DataSource) extends KeyValueStorage[String, SyncState] {
  type T = FastSyncStateStorage

  import FastSyncStateStorage._

  override val namespace: IndexedSeq[Byte] = Namespaces.FastSyncStateNamespace

  implicit val byteStringPickler: Pickler[ByteString] = transformPickler[ByteString, Array[Byte]](ByteString(_))(_.toArray[Byte])

  implicit val hashTypePickler: CompositePickler[HashType] =
    compositePickler[HashType]
      .addConcreteType[StateMptNodeHash]
      .addConcreteType[ContractStorageMptNodeHash]
      .addConcreteType[EvmCodeHash]
      .addConcreteType[StorageRootHash]

  override def keySerializer: String => IndexedSeq[Byte] = _.getBytes

  override def valueSerializer: SyncState => IndexedSeq[Byte] = Pickle.intoBytes(_).array()

  override def valueDeserializer: IndexedSeq[Byte] => SyncState =
    (bytes: IndexedSeq[Byte]) => Unpickle[SyncState].fromBytes(ByteBuffer.wrap(bytes.toArray[Byte]))

  protected def apply(dataSource: DataSource): FastSyncStateStorage = new FastSyncStateStorage(dataSource)

  def putSyncState(syncState: SyncState): FastSyncStateStorage = put(syncStateKey, syncState)

  def getSyncState(): Option[SyncState] = get(syncStateKey)

  def purge(): FastSyncStateStorage = remove(syncStateKey)

}
