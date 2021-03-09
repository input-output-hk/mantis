package io.iohk.ethereum.db.storage

import akka.util.ByteString
import boopickle.Default._
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.TransactionMappingStorage.{TransactionLocation, TxHash}
import io.iohk.ethereum.utils.ByteUtils.{byteSequenceToBuffer, compactPickledBytes}

class TransactionMappingStorage(val dataSource: DataSource)
    extends TransactionalKeyValueStorage[TxHash, TransactionLocation] {

  val namespace: IndexedSeq[Byte] = Namespaces.TransactionMappingNamespace
  def keySerializer: TxHash => IndexedSeq[Byte] = identity
  def keyDeserializer: IndexedSeq[Byte] => TxHash = identity
  def valueSerializer: TransactionLocation => IndexedSeq[Byte] = tl => compactPickledBytes(Pickle.intoBytes(tl))
  def valueDeserializer: IndexedSeq[Byte] => TransactionLocation =
    (byteSequenceToBuffer _).andThen(Unpickle[TransactionLocation].fromBytes)

  implicit val byteStringPickler: Pickler[ByteString] =
    transformPickler[ByteString, Array[Byte]](ByteString(_))(_.toArray[Byte])
}

object TransactionMappingStorage {
  type TxHash = IndexedSeq[Byte]

  case class TransactionLocation(blockHash: ByteString, txIndex: Int)

}
