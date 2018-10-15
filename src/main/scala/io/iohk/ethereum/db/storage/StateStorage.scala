package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.cache.MapCache
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.mpt.MptNode
import io.iohk.ethereum.network.p2p.messages.PV63.MptNodeEncoders._

trait StateStorage {
  def getBackingStorage(bn: BigInt): MptStorage
  def getReadOnlyStorage: MptStorage

  def onBlockSave(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlock: () => Unit): Unit
  def onBlockRollback(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlock: () => Unit): Unit

  def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, bn: BigInt)
  def getNode(nodeHash: NodeHash): Option[MptNode]
  def forcePersist: Unit
}

class ArchiveStateStorage(private val nodeStorage: NodeStorage,
                          private val cachedNodeStorage: CachedNodeStorage) extends StateStorage {

  override def forcePersist: Unit = {
    cachedNodeStorage.forcePersist()
  }

  override def onBlockSave(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlock: () => Unit): Unit = {
    if (cachedNodeStorage.persist()) {
      updateBestBlock()
    }
  }

  override def onBlockRollback(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlock: () => Unit): Unit = {
    if (cachedNodeStorage.persist()) {
      updateBestBlock()
    }
  }

  override def getReadOnlyStorage: MptStorage = {
    new SerializingMptStorage(ReadOnlyNodeStorage(new ArchiveNodeStorage(cachedNodeStorage)))
  }

  override def getBackingStorage(bn: BigInt): MptStorage = {
    new SerializingMptStorage(new ArchiveNodeStorage(cachedNodeStorage))
  }

  override def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, bn: BigInt): Unit = {
    nodeStorage.put(nodeHash, nodeEncoded)
  }

  override def getNode(nodeHash: NodeHash): Option[MptNode] = {
    cachedNodeStorage.get(nodeHash).map(_.toMptNode)
  }
}

class ReferenceCountedStateStorage(private val nodeStorage: NodeStorage,
                                   private val cachedNodeStorage: CachedNodeStorage,
                                   private val pruningHistory: BigInt) extends StateStorage {
  override def forcePersist: Unit = {
    cachedNodeStorage.forcePersist()
  }

  override def onBlockSave(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlock: () => Unit): Unit = {
    val blockToPrune = bn - pruningHistory

    if (blockToPrune <= currentBestSavedBlock){
      ReferenceCountNodeStorage.prune(blockToPrune, cachedNodeStorage, inMemory = false)
    } else{
      ReferenceCountNodeStorage.prune(blockToPrune, cachedNodeStorage, inMemory = true)
    }

    if (cachedNodeStorage.persist()) {
      updateBestBlock()
    }
  }

  override def onBlockRollback(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlock: () => Unit): Unit = {
    if (bn <= currentBestSavedBlock)
      ReferenceCountNodeStorage.rollback(bn, cachedNodeStorage, inMemory = false)
    else
      ReferenceCountNodeStorage.rollback(bn, cachedNodeStorage, inMemory = true)

    if (cachedNodeStorage.persist()) {
      updateBestBlock()
    }
  }

  override def getBackingStorage(bn: BigInt): MptStorage = {
    new SerializingMptStorage(new ReferenceCountNodeStorage(cachedNodeStorage, bn))
  }

  override def getReadOnlyStorage: MptStorage = {
    new SerializingMptStorage(ReadOnlyNodeStorage(new FastSyncNodeStorage(cachedNodeStorage, 0)))
  }

  override def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, bn: BigInt): Unit = {
    new FastSyncNodeStorage(nodeStorage, bn).update(Nil, Seq(nodeHash -> nodeEncoded))
  }

  override def getNode(nodeHash: NodeHash): Option[MptNode] = {
    new FastSyncNodeStorage(cachedNodeStorage, 0).get(nodeHash).map(_.toMptNode)
  }
}

object StateStorage {
  def apply(pruningMode: PruningMode,
            nodeStorage: NodeStorage,
            cachedNodeStorage: CachedNodeStorage): StateStorage = {
    pruningMode match {
      case ArchivePruning                => new ArchiveStateStorage(nodeStorage, cachedNodeStorage)
      case pruning.BasicPruning(history) => new ReferenceCountedStateStorage(nodeStorage, cachedNodeStorage, history)
    }
  }

  def getReadOnlyStorage(source: DataSource): MptStorage = {
    new SerializingMptStorage(new ArchiveNodeStorage(new NodeStorage(source)))
  }

  def createTestStateStorage(source: DataSource, pruningMode: PruningMode = ArchivePruning): (StateStorage, NodeStorage, CachedNodeStorage) = {
    val testCacheSize = 10000
    val nodeStorage = new NodeStorage(source)
    val cachedNodeStorage = new CachedNodeStorage(nodeStorage, MapCache.createTestCache(testCacheSize))
    (StateStorage(pruningMode, nodeStorage, cachedNodeStorage), nodeStorage, cachedNodeStorage)
  }

}