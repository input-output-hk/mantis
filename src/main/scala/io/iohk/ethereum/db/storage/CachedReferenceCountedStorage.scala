package io.iohk.ethereum.db.storage

import java.nio.ByteBuffer

import akka.util.ByteString
import boopickle.Default._
import com.google.common.cache.RemovalNotification
import io.iohk.ethereum.db.cache.Cache
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.mpt.{ByteArraySerializable, NodesKeyValueStorage}

import scala.collection.mutable

class CachedReferenceCountedStorage(nodeStorage: NodeStorage,
                                    cache: Cache[ByteString, HeapEntry],
                                    changeLog: ChangeLog,
                                    bn: BigInt) extends NodesKeyValueStorage {

  def get(nodeHash: NodeHash): Option[NodeEncoded] = {
    cache.get(nodeHash).fold {
      nodeStorage.get(nodeHash).map(enc => HeapEntry.fromBytes(enc)).map {entry =>
        cache.put(nodeHash, entry)
        entry.nodeEncoded
      }
    }(entry => Some(entry.nodeEncoded))
  }

  def update(toRemove: Seq[ByteString], toUpsert: Seq[(ByteString, NodeEncoded)]): NodesKeyValueStorage = {
    changeLog.withChangelog(bn) { blockChangeLog =>
      toUpsert.foreach{ case (nodeKey, nodeValue) =>
        // When node is not available in cache, it means it can be new node, but also it can already be in db inserted
        // on earlier block and flushed from cache.
        val (updatedValue, change) = {
          val fromCache = cache.get(nodeKey)
          if (fromCache.isDefined)
            (fromCache.get.incrementParents(bn), Increase(nodeKey))
          else
            (HeapEntry(nodeValue, 1, bn), New(nodeKey))
        }

        cache.put(nodeKey, updatedValue)
        blockChangeLog.registerChange(change, updatedValue.numOfParents)
      }

      toRemove.foreach {node =>
        // Removal can only hapen after previously loading it from db/cache, so now it should be loaded in cache.
        val updatedValue = cache.get(node).get.decrementParents(bn)
        cache.put(node, updatedValue)
        blockChangeLog.registerChange(Decrease(node), updatedValue.numOfParents)
      }
    }
    this
  }

  def persist(): Unit = ()
}

object CachedReferenceCountedStorage {
  private def getNodesToPruneInCache(cache: Cache[NodeHash, HeapEntry], deathRow: List[NodeHash], blockToPrune: BigInt): List[NodeHash] = {
    var nodesToDeleteFromCache = List.empty[NodeHash]
    deathRow.foreach { nodeHash =>
      cache.get(nodeHash).foreach {nodeFromCache =>
        if (nodeFromCache.numOfParents == 0 && nodeFromCache.bn <= blockToPrune) {
          nodesToDeleteFromCache = nodeHash :: nodesToDeleteFromCache
        }
      }
    }
    nodesToDeleteFromCache
  }

  def persistCache[V](cache: Cache[ByteString, V], storage: NodeStorage)(implicit  ser: ByteArraySerializable[V]): Boolean = {
    if (cache.shouldPersist) {
      val values = cache.getValues
      val serialized = values.map {case (key, value) => key -> ser.toBytes(value)}
      storage.update(Nil, serialized)
      cache.clear()
      true
    } else {
      false
    }
  }

  def prune(deathRow: List[NodeHash], cache: Cache[NodeHash, HeapEntry], blockToPrune: BigInt): Unit = {
    val toDelFromCache = getNodesToPruneInCache(cache, deathRow, blockToPrune)
    cache.update(toDelFromCache, Nil)
  }

  private def getPreviousBlockState(cache: Cache[NodeHash, HeapEntry],
                                    nodeStorage: NodeStorage,
                                    changeLog: List[Update],
                                    newBestAfterRollback: BigInt): Map[NodeHash, (HeapEntry, Boolean)] = {
    var newState = Map.empty[NodeHash, (HeapEntry, Boolean)]

    changeLog.foreach { update =>
      val nodeHash = update.hash
      val currentState =
        newState.get(nodeHash)              orElse
          cache.get(nodeHash).map((_, false)) orElse
          nodeStorage.get(nodeHash).map(HeapEntry.fromBytes).map((_, false))

      currentState.foreach { case (current, deletable) =>
        val reversedState = update match {
          case Increase(_) => (current.decrementParents(newBestAfterRollback), deletable)
          case Decrease(_) => (current.incrementParents(newBestAfterRollback), deletable)
          case New(_)      => (current.decrementParents(newBestAfterRollback), true)
        }
        newState += nodeHash -> reversedState
      }
    }

    newState
  }

  private def gatherCacheUpdates(previousState: Map[NodeHash, (HeapEntry, Boolean)]): (List[NodeHash], List[(NodeHash, HeapEntry)]) = {
    previousState.foldLeft(List.empty[NodeHash], List.empty[(NodeHash, HeapEntry)]){ case ((toDel, toUpdate), (entryKey, (entryValue, deletable))) =>
      if (entryValue.numOfParents == 0 && deletable)
        (entryKey :: toDel, toUpdate)
      else
        (toDel, (entryKey, entryValue) :: toUpdate)
    }
  }


  def rollback(cache: Cache[NodeHash, HeapEntry], nodeStorage: NodeStorage, changeLog: List[Update], bn: BigInt): Unit = {
    val newBestAfterRollback = bn - 1
    val previousState = getPreviousBlockState(cache, nodeStorage, changeLog, newBestAfterRollback)
    val (nodesToDelete, nodesToUpdate) = gatherCacheUpdates(previousState)
    cache.update(nodesToDelete, nodesToUpdate)
  }

  def saveOnlyNotificationHandler(nodeStorage: NodeStorage)(notification: RemovalNotification[ByteString, HeapEntry]): Unit ={
    val entry = notification.getValue
    val key = notification.getKey
    nodeStorage.update(Nil, Seq(key -> HeapEntry.toBytes(entry)))
  }
}

class NoHistoryCachedReferenceCountedStorage(nodeStorage: NodeStorage, cache: mutable.Map[ByteString, NodeEncoded]) extends NodesKeyValueStorage {

  def get(nodeHash: NodeHash): Option[NodeEncoded] = {
    cache.get(nodeHash) orElse nodeStorage.get(nodeHash)
  }

  def update(toRemove: Seq[ByteString], toUpsert: Seq[(ByteString, NodeEncoded)]): NodesKeyValueStorage = {
    toUpsert.foreach {case (key, value) =>
      cache.put(key, value)
    }

    toRemove.foreach {key =>
      cache.remove(key)
    }

    this
  }

  def persist(): Unit = {
    val nodesToSave = cache.toSeq.map(node => node._1 -> HeapEntry.toBytes(HeapEntry(node._2, 1, 0)))
    nodeStorage.update(Nil, nodesToSave)
    cache.clear()
  }
}

import io.iohk.ethereum.utils.ByteUtils._

final case class HeapEntry(nodeEncoded: NodeEncoded, numOfParents:Int, bn: BigInt) {

  def incrementParents(incrementationBlock: BigInt): HeapEntry = {
    copy(numOfParents = numOfParents + 1, bn = incrementationBlock)
  }

  def decrementParents(decrementationBlock: BigInt): HeapEntry = {
    copy(numOfParents = numOfParents - 1, bn = decrementationBlock)
  }
}
object HeapEntry {
  import boopickle.Default._

  type HeapEntryBody = (Array[Byte], Int, BigInt)

  implicit val HeapEntryPickler: Pickler[HeapEntry] = transformPickler[HeapEntry, HeapEntryBody]
    { case (enc, par, bn) =>
      HeapEntry(enc, par, bn)
    } {entry => (entry.nodeEncoded, entry.numOfParents, entry.bn)}


  def toBytes(entry: HeapEntry): Array[Byte] = {
    compactPickledBytes(Pickle.intoBytes(entry)).toArray[Byte]
  }

  def fromBytes(asbytes: Array[Byte]): HeapEntry = {
    Unpickle[HeapEntry].fromBytes(ByteBuffer.wrap(asbytes))
  }

  implicit val heapEntrySerializer = new ByteArraySerializable[HeapEntry] {
    override def toBytes(input: HeapEntry): Array[Byte] = HeapEntry.toBytes(input)
    override def fromBytes(bytes: Array[Byte]): HeapEntry = HeapEntry.fromBytes(bytes)
  }

}

sealed abstract class Update {
  val hash: ByteString
}
final case class Increase(hash: ByteString) extends Update
final case class Decrease(hash: ByteString) extends Update
final case class New(hash: ByteString)      extends Update

object Update {
  implicit val byteStringPickler: Pickler[ByteString] = transformPickler[ByteString, Array[Byte]](ByteString(_))(_.toArray[Byte])
  implicit val updatePickler: Pickler[Update] = compositePickler[Update].
    addConcreteType[Increase].
    addConcreteType[Decrease].
    addConcreteType[New]
}

class ChangeLog(nodeStorage: NodeStorage) {
  private val logs = mutable.Map.empty[BigInt, BlockChangeLog]

  def persistChangeLog(forBlock: BigInt): Unit = {
    logs.get(forBlock).foreach {changeLog =>
      nodeStorage.update(Nil,
        Seq(
          ChangeLog.getLogKey(forBlock) -> ChangeLog.serializeChangeLog(changeLog.getAllChanges),
          ChangeLog.getDrwKey(forBlock) -> ChangeLog.serializeDeathRow(changeLog.getAllToDelete)
        )
      )
      logs.remove(forBlock)
    }
  }

  def withChangelog(bn: BigInt)(updates: BlockChangeLog => Unit): Unit = {
    val changeLog = getChangeLogForBlock(bn)
    updates(changeLog)
    logs.update(bn, changeLog)
  }

  def getChangeLogForBlock(bn: BigInt): BlockChangeLog = {
    logs.getOrElse(bn, {
      val newChangeLog = new BlockChangeLog(List.empty, Set.empty)
      logs += bn -> newChangeLog
      newChangeLog
    })
  }

  def getChangeLogFromStorage(bn: BigInt): Option[List[Update]] = {
    nodeStorage.get(ChangeLog.getLogKey(bn)).map(ChangeLog.deserializeChangeLog)
  }

  def removeChangeLogFromStorage(bn: BigInt): Unit = {
    nodeStorage.remove(ChangeLog.getLogKey(bn))
  }

  def getDeathRowFromStorage(bn: BigInt): Option[List[NodeHash]] = {
    nodeStorage.get(ChangeLog.getDrwKey(bn)).map(ChangeLog.deserializeDeathRow)
  }

  def removeDeathRowFromStorage(bn: BigInt): Unit = {
    nodeStorage.remove(ChangeLog.getDrwKey(bn))
  }
}

object ChangeLog {
  private val changeLogPrefix = "log".getBytes
  private val deathRowPrefix  = "drw".getBytes

  def getLogKey(bn: BigInt): ByteString = ByteString(changeLogPrefix ++ bn.toByteArray)
  def getDrwKey(bn: BigInt): ByteString = ByteString(deathRowPrefix ++ bn.toByteArray)

  import Update.{updatePickler, byteStringPickler}
  import boopickle.Default._

  def serializeChangeLog(changeLog: List[Update]): Array[Byte] =
    compactPickledBytes(Pickle.intoBytes(changeLog)).toArray[Byte]

  def serializeDeathRow(deathRow: List[NodeHash]): Array[Byte] =
    compactPickledBytes(Pickle.intoBytes(deathRow)).toArray[Byte]

  def deserializeChangeLog(bytes: Array[Byte]): List[Update] =
    Unpickle[List[Update]].fromBytes(ByteBuffer.wrap(bytes))

  def deserializeDeathRow(bytes: Array[Byte]): List[NodeHash] =
    Unpickle[List[NodeHash]].fromBytes(ByteBuffer.wrap(bytes))
}

class BlockChangeLog(private val initialLog: List[Update], private val initialToDel: Set[NodeHash]) {
  private var updates = initialLog
  private var potentialNodesToDel = initialToDel

  def registerChange(update: Update, refCountAfterUpdate: Int): Unit = {
    updates = update :: updates

    if (refCountAfterUpdate == 0) {
      potentialNodesToDel = potentialNodesToDel + update.hash
    }
  }

  def getAllChanges: List[Update] = updates

  def getAllToDelete: List[NodeHash] = potentialNodesToDel.toList
}
