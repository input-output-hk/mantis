package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.AppStateStorage._
import io.iohk.ethereum.db.storage.PruningMode.PruneFn

/**
  * This class is used to store app state variables
  *   Key: see AppStateStorage.Keys
  *   Value: stored string value
  */
class AppStateStorage(val dataSource: DataSource, pruneFn: PruneFn) extends KeyValueStorage[Key, Value, AppStateStorage]{
  type T = AppStateStorage

  val namespace: IndexedSeq[Byte] = Namespaces.AppStateNamespace
  def keySerializer: Key => IndexedSeq[Byte] = _.name.getBytes
  def valueSerializer: String => IndexedSeq[Byte] = _.getBytes
  def valueDeserializer: IndexedSeq[Byte] => String = (valueBytes: IndexedSeq[Byte]) => new String(valueBytes.toArray)

  protected def apply(dataSource: DataSource): AppStateStorage = new AppStateStorage(dataSource, pruneFn)

  def getBestBlockNumber(): BigInt =
    BigInt(get(Keys.BestBlockNumber).getOrElse("0"))

  def putBestBlockNumber(bestBlockNumber: BigInt): AppStateStorage = {
    // FIXME We need to decouple pruning from best block number storing in this fn
    pruneFn(getLastPrunedBlock(), bestBlockNumber) match {
      case PruneResult(_, 0) =>
      case PruneResult(lastPrunedBlockNumber, _) => putLastPrunedBlock(lastPrunedBlockNumber)
    }

    put(Keys.BestBlockNumber, bestBlockNumber.toString)
  }

  def isFastSyncDone(): Boolean =
    get(Keys.FastSyncDone).exists(_.toBoolean)

  def fastSyncDone(): AppStateStorage =
    put(Keys.FastSyncDone, true.toString)

  def getEstimatedHighestBlock(): BigInt =
    BigInt(get(Keys.EstimatedHighestBlock).getOrElse("0"))

  def putEstimatedHighestBlock(n: BigInt): AppStateStorage =
    put(Keys.EstimatedHighestBlock, n.toString)

  def getSyncStartingBlock(): BigInt =
    BigInt(get(Keys.SyncStartingBlock).getOrElse("0"))

  def putSyncStartingBlock(n: BigInt): AppStateStorage =
    put(Keys.SyncStartingBlock, n.toString)

  def putLastPrunedBlock(n: BigInt): AppStateStorage =
    put(Keys.LastPrunedBlock, n.toString())

  def getLastPrunedBlock(): BigInt =
    BigInt(get(Keys.LastPrunedBlock).getOrElse("-1"))
}

object AppStateStorage {
  type Value = String

  case class Key private (name: String)

  object Keys {
    val BestBlockNumber = Key("BestBlockNumber")
    val FastSyncDone = Key("FastSyncDone")
    val EstimatedHighestBlock = Key("EstimatedHighestBlock")
    val SyncStartingBlock = Key("SyncStartingBlock")
    val LastPrunedBlock = Key("LastPrunedBlock")
  }
}
