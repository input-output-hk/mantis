package io.iohk.ethereum.db.storage

import java.nio.ByteBuffer

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.TransactionMappingStorage.{TransactionLocation, TxHash}
import io.iohk.ethereum.utils.ByteUtils.compactPickledBytes
import boopickle.Default._

class TransactionMappingStorage(val dataSource: DataSource) extends KeyValueStorage[TxHash, TransactionLocation, TransactionMappingStorage] {

  val namespace: IndexedSeq[Byte] = Namespaces.TransactionMappingNamespace
  def keySerializer: TxHash => Array[Byte] = _.toArray[Byte]
  def valueSerializer: TransactionLocation => Array[Byte] = tl => compactPickledBytes(Pickle.intoBytes(tl)).toArray[Byte]
  def valueDeserializer: Array[Byte] => TransactionLocation =
    bytes => Unpickle[TransactionLocation].fromBytes(ByteBuffer.wrap(bytes))

  implicit val byteStringPickler: Pickler[ByteString] = transformPickler[ByteString, Array[Byte]](ByteString(_))(_.toArray[Byte])

  protected def apply(dataSource: DataSource): TransactionMappingStorage = new TransactionMappingStorage(dataSource)

}

object TransactionMappingStorage {
  type TxHash = IndexedSeq[Byte]

  case class TransactionLocation(blockHash: ByteString, txIndex: Int)

}
