package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.cache.{Cache, MapCache}
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.mpt.MptNode
import io.iohk.ethereum.network.p2p.messages.PV63.MptNodeEncoders._
import io.iohk.ethereum.utils.Config

trait StateStorage {
  def getBackingStorage(bn: BigInt): MptStorage
  def getReadOnlyStorage: MptStorage

  def onBlockSave(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlock: () => Unit): Unit
  def onBlockRollback(bn: BigInt, currentBestSavedBlock: BigInt)(updateBestBlock: () => Unit): Unit

  def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, bn: BigInt)
  def getNode(nodeHash: NodeHash): Option[MptNode]
  def forcePersist: Unit
}

class ArchiveStateStorage(private val source: DataSource) extends StateStorage {
  private val cache = MapCache.createCache[NodeHash, NodeEncoded](Config.NodeCacheConfig)
  private val nodeStorage = new NodeStorage(source)
  private val cachedNodeStorage = new CachedNodeStorage(nodeStorage, cache)

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
    val temporaryCache = MapCache.createCache[NodeHash, NodeEncoded](Config.NodeCacheConfig)
    val temporaryCachedStorage = new CachedNodeStorage(nodeStorage, temporaryCache)

    new SerializingMptStorage(new ArchiveNodeStorage(temporaryCachedStorage), Some(temporaryCachedStorage))
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

class ReferenceCountedStateStorage(private val source: DataSource,
                                   private val cache: Cache[NodeHash, NodeEncoded],
                                   private val pruningHistory: BigInt) extends StateStorage {
  private val nodeStorage = new NodeStorage(source)
  private val cachedNodeStorage = new CachedNodeStorage(nodeStorage, cache)

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
    val temporaryCache = MapCache.createCache[NodeHash, NodeEncoded](Config.NodeCacheConfig)
    val temporaryCachedStroage = new CachedNodeStorage(nodeStorage, temporaryCache)

    new SerializingMptStorage(new FastSyncNodeStorage(temporaryCachedStroage, 0), Some(temporaryCachedStroage))
  }

  override def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, bn: BigInt): Unit = {
    new FastSyncNodeStorage(nodeStorage, bn).update(Nil, Seq(nodeHash -> nodeEncoded))
  }

  override def getNode(nodeHash: NodeHash): Option[MptNode] = {
    new FastSyncNodeStorage(nodeStorage, 0).get(nodeHash).map(_.toMptNode)
  }
}


object StateStorage {
  def apply(source: DataSource, pruningMode: PruningMode): StateStorage = {
    pruningMode match {
      case ArchivePruning                => new ArchiveStateStorage(source)
      case pruning.BasicPruning(history) => new ReferenceCountedStateStorage(source, MapCache.createCache(Config.NodeCacheConfig), history)
    }
  }

  def getReadOnlyStorage(source: DataSource): MptStorage = {
    new SerializingMptStorage(new ArchiveNodeStorage(new NodeStorage(source)))
  }

  sealed trait StorageType
  case class  WriteAbleStorage(bn: BigInt) extends StorageType
  case object ReadOnlyStorage extends StorageType

}