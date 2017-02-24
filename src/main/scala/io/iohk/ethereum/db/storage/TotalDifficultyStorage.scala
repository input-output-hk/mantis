package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.TotalDifficultyStorage._

/**
  * This class is used to store the total difficulty of blocks, by using:
  *   Key: hash of the block
  *   Value: the total difficulty
  */
class TotalDifficultyStorage(val dataSource: DataSource) extends KeyValueStorage[BlockHash, TotalDifficulty]{
  type T = TotalDifficultyStorage

  val namespace: IndexedSeq[Byte] = Namespaces.TotalDifficultyNamespace
  def keySerializer: BlockHash => IndexedSeq[Byte] = _.toIndexedSeq
  def valueSerializer: TotalDifficulty => IndexedSeq[Byte] = _.toByteArray.toIndexedSeq
  def valueDeserializer: IndexedSeq[Byte] => BigInt = (valueBytes: IndexedSeq[Byte]) => BigInt(1, valueBytes.toArray)

  protected def apply(dataSource: DataSource): TotalDifficultyStorage = new TotalDifficultyStorage(dataSource)
}

object TotalDifficultyStorage {
  type BlockHash = ByteString
  type TotalDifficulty = BigInt
}
