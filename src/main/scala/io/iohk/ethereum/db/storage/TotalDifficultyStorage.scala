package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.TotalDifficultyStorage._

/**
  * This class is used to store the total difficulty of blocks, by using:
  *   Key: hash of the block
  *   Value: the total difficulty
  */
class TotalDifficultyStorage(val dataSource: DataSource) extends KeyValueStorage[BlockHash, TotalDifficulty, TotalDifficultyStorage]{
  val namespace: IndexedSeq[Byte] = Namespaces.TotalDifficultyNamespace
  def keySerializer: BlockHash => Array[Byte] = _.toArray[Byte]
  def valueSerializer: TotalDifficulty => Array[Byte] = _.toByteArray
  def valueDeserializer: Array[Byte] => BigInt = (valueBytes: Array[Byte]) => BigInt(1, valueBytes)

  protected def apply(dataSource: DataSource): TotalDifficultyStorage = new TotalDifficultyStorage(dataSource)
}

object TotalDifficultyStorage {
  type BlockHash = ByteString
  type TotalDifficulty = BigInt
}
