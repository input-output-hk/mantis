package io.iohk.ethereum.db.storage

import java.math.BigInteger

import akka.util.ByteString

import scala.collection.immutable.ArraySeq

import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.StorageTypes.BlockHash

class BlockNumberMappingStorage(val dataSource: DataSource) extends TransactionalKeyValueStorage[BigInt, BlockHash] {
  override val namespace: IndexedSeq[Byte] = Namespaces.HeightsNamespace

  override def keySerializer: BigInt => IndexedSeq[Byte] = index => ArraySeq.unsafeWrapArray(index.toByteArray)

  override def keyDeserializer: IndexedSeq[Byte] => BigInt = bytes => new BigInt(new BigInteger(bytes.toArray))

  override def valueSerializer: BlockHash => IndexedSeq[Byte] = identity

  override def valueDeserializer: IndexedSeq[Byte] => BlockHash = arr => ByteString(arr.toArray[Byte])
}
