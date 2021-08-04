package io.iohk.ethereum.db.storage

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration

import io.iohk.ethereum.db.cache.LruCache
import io.iohk.ethereum.db.cache.MapCache
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.NodeStorage.NodeEncoded
import io.iohk.ethereum.db.storage.NodeStorage.NodeHash
import io.iohk.ethereum.db.storage.StateStorage.FlushSituation
import io.iohk.ethereum.db.storage.StateStorage.GenesisDataLoad
import io.iohk.ethereum.db.storage.pruning.ArchivePruning
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.mpt.MptNode
import io.iohk.ethereum.network.p2p.messages.ETH63.MptNodeEncoders._
import io.iohk.ethereum.utils.Config.NodeCacheConfig

// scalastyle:off
trait StateStorage {
  def getBackingStorage(bn: BigInt): MptStorage
  def getReadOnlyStorage: MptStorage

  def onBlockSave(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlocksData: () => Unit): Unit
  def onBlockRollback(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlocksData: () => Unit): Unit

  def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, bn: BigInt): Unit
  def getNode(nodeHash: NodeHash): Option[MptNode]
  def forcePersist(reason: FlushSituation): Boolean
}

class ArchiveStateStorage(private val nodeStorage: NodeStorage) extends StateStorage {

  override def forcePersist(reason: FlushSituation): Boolean = true

  override def onBlockSave(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlocksData: () => Unit): Unit =
    updateBestBlocksData()

  override def onBlockRollback(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlocksData: () => Unit): Unit =
    updateBestBlocksData()

  override def getReadOnlyStorage: MptStorage =
    new SerializingMptStorage(ReadOnlyNodeStorage(new ArchiveNodeStorage(nodeStorage)))

  override def getBackingStorage(bn: BigInt): MptStorage =
    new SerializingMptStorage(new ArchiveNodeStorage(nodeStorage))

  override def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, bn: BigInt): Unit =
    nodeStorage.put(nodeHash, nodeEncoded)

  override def getNode(nodeHash: NodeHash): Option[MptNode] =
    nodeStorage.get(nodeHash).map(_.toMptNode)
}

class ReferenceCountedStateStorage(
    private val nodeStorage: NodeStorage,
    private val pruningHistory: BigInt
) extends StateStorage {
  override def forcePersist(reason: FlushSituation): Boolean = true

  override def onBlockSave(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlocksData: () => Unit): Unit = {
    val blockToPrune = bn - pruningHistory
    ReferenceCountNodeStorage.prune(blockToPrune, nodeStorage, inMemory = blockToPrune > currentBestSavedBlock)
    updateBestBlocksData()
  }

  override def onBlockRollback(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlocksData: () => Unit): Unit = {
    ReferenceCountNodeStorage.rollback(bn, nodeStorage, inMemory = bn > currentBestSavedBlock)
    updateBestBlocksData()
  }

  override def getBackingStorage(bn: BigInt): MptStorage =
    new SerializingMptStorage(new ReferenceCountNodeStorage(nodeStorage, bn))

  override def getReadOnlyStorage: MptStorage =
    new SerializingMptStorage(ReadOnlyNodeStorage(new FastSyncNodeStorage(nodeStorage, 0)))

  override def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, bn: BigInt): Unit =
    new FastSyncNodeStorage(nodeStorage, bn).update(Nil, Seq(nodeHash -> nodeEncoded))

  override def getNode(nodeHash: NodeHash): Option[MptNode] =
    new FastSyncNodeStorage(nodeStorage, 0).get(nodeHash).map(_.toMptNode)
}

class CachedReferenceCountedStateStorage(
    private val nodeStorage: NodeStorage,
    private val pruningHistory: Int,
    private val lruCache: LruCache[NodeHash, HeapEntry]
) extends StateStorage {

  private val changeLog = new ChangeLog(nodeStorage)

  override def forcePersist(reason: FlushSituation): Boolean =
    reason match {
      case GenesisDataLoad => CachedReferenceCountedStorage.persistCache(lruCache, nodeStorage, forced = true)
    }

  override def onBlockSave(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlocksData: () => Unit): Unit = {
    val blockToPrune = bn - pruningHistory
    changeLog.persistChangeLog(bn)
    changeLog.getDeathRowFromStorage(blockToPrune).foreach { deathRow =>
      CachedReferenceCountedStorage.prune(deathRow, lruCache, blockToPrune)
    }
    if (CachedReferenceCountedStorage.persistCache(lruCache, nodeStorage)) {
      updateBestBlocksData()
    }
    changeLog.removeBlockMetaData(blockToPrune)
  }

  override def onBlockRollback(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlocksData: () => Unit): Unit = {
    changeLog.getChangeLogFromStorage(bn).foreach { changeLog =>
      CachedReferenceCountedStorage.rollback(lruCache, nodeStorage, changeLog, bn)
    }
    changeLog.removeBlockMetaData(bn)
  }

  override def getReadOnlyStorage: MptStorage =
    new SerializingMptStorage(ReadOnlyNodeStorage(new NoHistoryCachedReferenceCountedStorage(nodeStorage, lruCache, 0)))

  override def getBackingStorage(bn: BigInt): MptStorage =
    new SerializingMptStorage(new CachedReferenceCountedStorage(nodeStorage, lruCache, changeLog, bn))

  override def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, bn: BigInt): Unit =
    nodeStorage.put(nodeHash, HeapEntry.toBytes(HeapEntry(nodeEncoded, 1, bn)))

  override def getNode(nodeHash: NodeHash): Option[MptNode] =
    lruCache
      .get(nodeHash)
      .map(_.nodeEncoded.toMptNode)
      .orElse(
        nodeStorage
          .get(nodeHash)
          .map(enc => HeapEntry.fromBytes(enc).nodeEncoded.toMptNode)
      )
}

object StateStorage {
  def apply(
      pruningMode: PruningMode,
      nodeStorage: NodeStorage,
      lruCache: LruCache[NodeHash, HeapEntry]
  ): StateStorage =
    pruningMode match {
      case ArchivePruning                   => new ArchiveStateStorage(nodeStorage)
      case pruning.BasicPruning(history)    => new ReferenceCountedStateStorage(nodeStorage, history)
      case pruning.InMemoryPruning(history) => new CachedReferenceCountedStateStorage(nodeStorage, history, lruCache)
    }

  def getReadOnlyStorage(source: EphemDataSource): MptStorage =
    mptStorageFromNodeStorage(new NodeStorage(source))

  def mptStorageFromNodeStorage(storage: NodeStorage): SerializingMptStorage =
    new SerializingMptStorage(new ArchiveNodeStorage(storage))

  def createTestStateStorage(
      source: DataSource,
      pruningMode: PruningMode = ArchivePruning
  ): (StateStorage, NodeStorage, CachedNodeStorage) = {
    val testCacheSize = 10000
    val testCacheConfig = new NodeCacheConfig {
      override val maxSize: Long = 10000
      override val maxHoldTime: FiniteDuration = FiniteDuration(10, TimeUnit.MINUTES)
    }
    val nodeStorage = new NodeStorage(source)
    val cachedNodeStorage = new CachedNodeStorage(nodeStorage, MapCache.createTestCache(testCacheSize))

    (
      StateStorage(pruningMode, nodeStorage, new LruCache[NodeHash, HeapEntry](testCacheConfig)),
      nodeStorage,
      cachedNodeStorage
    )
  }

  sealed abstract class FlushSituation
  case object GenesisDataLoad extends FlushSituation

}
