package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.dataSource.{DataSource, IodbDataSource}

/**
  * This Storage pads zeroes to integer keys in order to satisfy IODB fixed length keys restrinctions
  */
class IodbBlockNumberMappingStorage(dataSource: DataSource) extends BlockNumberMappingStorage(dataSource) {

  override def keySerializer: (BigInt) => IndexedSeq[Byte] = index => index.toByteArray.padTo(IodbDataSource.KeySizeWithoutNamespace, 0.toByte)

  override protected def apply(dataSource: DataSource): IodbBlockNumberMappingStorage = new IodbBlockNumberMappingStorage(dataSource)
}
