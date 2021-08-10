package io.iohk.ethereum.db.storage

import java.math.BigInteger

import akka.util.ByteString

import scala.collection.immutable.ArraySeq

import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.dataSource.DataSourceBatchUpdate
import io.iohk.ethereum.db.storage.AppStateStorage._
import io.iohk.ethereum.domain.appstate.BestBlockInfo
import io.iohk.ethereum.utils.Hex

/** This class is used to store app state variables
  *   Key: see AppStateStorage.Keys
  *   Value: stored string value
  */
class AppStateStorage(val dataSource: DataSource) extends TransactionalKeyValueStorage[Key, Value] {

  val namespace: IndexedSeq[Byte] = Namespaces.AppStateNamespace
  def keySerializer: Key => IndexedSeq[Byte] = k =>
    ArraySeq.unsafeWrapArray(k.getBytes(StorageStringCharset.UTF8Charset))

  def keyDeserializer: IndexedSeq[Byte] => Key = k => new String(k.toArray, StorageStringCharset.UTF8Charset)
  def valueSerializer: String => IndexedSeq[Byte] = k =>
    ArraySeq.unsafeWrapArray(k.getBytes(StorageStringCharset.UTF8Charset))
  def valueDeserializer: IndexedSeq[Byte] => String = (valueBytes: IndexedSeq[Byte]) =>
    new String(valueBytes.toArray, StorageStringCharset.UTF8Charset)

  def getBestBlockNumber(): BigInt =
    getBigInt(Keys.BestBlockNumber)

  def getBestBlockInfo(): BestBlockInfo =
    BestBlockInfo( // FIXME default value for hash ?
      get(Keys.BestBlockHash).map(v => ByteString(Hex.decode(v))).getOrElse(ByteString.empty),
      getBigInt(Keys.BestBlockNumber)
    )

  def putBestBlockData(b: BestBlockInfo): DataSourceBatchUpdate =
    put(Keys.BestBlockNumber, b.number.toString)
      .and(put(Keys.BestBlockHash, Hex.toHexString(b.hash.toArray)))

  def putBestBlockNumber(bestBlockNumber: BigInt): DataSourceBatchUpdate =
    put(Keys.BestBlockNumber, bestBlockNumber.toString)

  def isFastSyncDone(): Boolean =
    get(Keys.FastSyncDone).exists(_.toBoolean)

  def fastSyncDone(): DataSourceBatchUpdate =
    put(Keys.FastSyncDone, true.toString)

  def getEstimatedHighestBlock(): BigInt =
    getBigInt(Keys.EstimatedHighestBlock)

  def putEstimatedHighestBlock(n: BigInt): DataSourceBatchUpdate =
    put(Keys.EstimatedHighestBlock, n.toString)

  def getSyncStartingBlock(): BigInt =
    getBigInt(Keys.SyncStartingBlock)

  def putSyncStartingBlock(n: BigInt): DataSourceBatchUpdate =
    put(Keys.SyncStartingBlock, n.toString)

  private def getBigInt(key: Key): BigInt =
    get(key).map(BigInt(_)).getOrElse(BigInt(BigInteger.ZERO))

  /** It is safe to return zero in case of not having any checkpoint block,
    * because we assume that genesis block is a kinda stable checkpoint block (without real checkpoint)
    *
    * @return Latest CheckpointBlock Number
    */
  def getLatestCheckpointBlockNumber(): BigInt =
    getBigInt(Keys.LatestCheckpointBlockNumber)

  def removeLatestCheckpointBlockNumber(): DataSourceBatchUpdate =
    update(toRemove = Seq(Keys.LatestCheckpointBlockNumber), toUpsert = Nil)

  def putLatestCheckpointBlockNumber(latestCheckpointBlockNumber: BigInt): DataSourceBatchUpdate =
    update(Nil, Seq(Keys.LatestCheckpointBlockNumber -> latestCheckpointBlockNumber.toString))
}

object AppStateStorage {
  type Key = String
  type Value = String

  object Keys {
    val BestBlockNumber = "BestBlockNumber"
    val BestBlockHash = "BestBlockHash"
    val FastSyncDone = "FastSyncDone"
    val EstimatedHighestBlock = "EstimatedHighestBlock"
    val SyncStartingBlock = "SyncStartingBlock"
    val LatestCheckpointBlockNumber = "LatestCheckpointBlockNumber"
  }

}
