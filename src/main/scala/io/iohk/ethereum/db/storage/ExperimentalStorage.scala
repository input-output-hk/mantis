package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.mpt.NodesKeyValueStorage
import io.iohk.ethereum.db.storage.pruning.PruneSupport
import io.iohk.ethereum.utils.Logger

import scala.concurrent.ExecutionContext

/**
  * This class helps to deal with two problems regarding MptNodes storage:
  * 1) Define a way to delete ones that are no longer needed but allow rollbacks to be performed
  * 2) Avoids removal of nodes that can be used in different trie branches because the hash is the same
  *
  * To deal with (1) when a node is no longer needed, block number alongside with a stored node snapshot is saved so
  * it can be restored in case of rollback.
  *
  * In order to solve (2), before saving a node, it's wrapped with the number of references it has.
  *
  * Using this storage will change data to be stored in nodeStorage in two ways (and it will, as consequence, make
  * different pruning mechanisms incompatible):
  * - Instead of saving KEY -> VALUE, it will store KEY -> STORED_NODE(VALUE, REFERENCE_COUNT, LAST_USED_BY_BLOCK)
  *
  * Also, additional data will be saved in this storage:
  * - For each block: BLOCK_NUMBER_TAG -> NUMBER_OF_SNAPSHOTS
  * - For each node changed within a block: (BLOCK_NUMBER_TAG ++ SNAPSHOT_INDEX) -> SNAPSHOT
  *
  * Storing snapshot info this way allows for easy construction of snapshot key (based on a block number
  * and number of snapshots) and therefore, fast access to each snapshot individually.
  */
class ExperimentalStorage(nodeStorage: NodesStorage, blockNumber: Option[BigInt] = None)
  extends NodesKeyValueStorage {

  import ExperimentalStorage._

  override def get(key: ByteString): Option[NodeEncoded] = {
    nodeStorage.get(key)
  }

  trait Pruner {
    type NodesToRemove
    type NodesToUpsert
    def saveNodes(nodesToRemove: NodesToRemove, nodesToUpsert: NodesToUpsert): Unit = ???


  }


  override def update(toRemove: Seq[NodeHash], toUpsert: Seq[(NodeHash, NodeEncoded)]): NodesKeyValueStorage = {
    require(blockNumber.isDefined)

    val bn = blockNumber.get
    var referenceCounts = Map.empty[RefKey, PruningData]
    val touchedKey = touchedNodesKey(bn)

    // Get current deathrow (nodes with )
    var nodes: Array[Byte] = nodeStorage.get(touchedKey).getOrElse(Array())

    def getExistingOrEmpty(key:NodeHash, map: Map[NodeHash, PruningData]): PruningData = {
      map.getOrElse(key, PruningData(0, bn))
    }

    toUpsert.foreach {hash =>
      nodes = nodes ++ hash._1.toArray[Byte]
      referenceCounts = referenceCounts.updated(getRefCountKey(hash._1), getExistingOrEmpty(hash._1, referenceCounts).incrementCounts)
    }

    toRemove.foreach {hash =>
      nodes = nodes ++ hash.toArray[Byte]
      referenceCounts = referenceCounts.updated(getRefCountKey(hash), getExistingOrEmpty(hash, referenceCounts).decrementCounts)
    }

    val touched = touchedKey -> nodes

    val refrenceCountUpdates = referenceCounts.map {data =>
      val currentCount = nodeStorage.get(data._1).map(PruningData.fromBytes).getOrElse(PruningData.empty)
      val updated = currentCount.update(data._2)
      data._1 -> PruningData.toBytes(updated)
    }.toSeq

    val writebatch = toUpsert ++ Seq(touched) ++ refrenceCountUpdates
    nodeStorage.updateCond(Nil, writebatch, inMemory = true)
    this
  }
}

object ExperimentalStorage extends PruneSupport with Logger {
  case class PruningData(referenceCount: Int, lastUsedBLock: BigInt) {
    def incrementCounts: PruningData = copy(referenceCount = referenceCount + 1)
    def decrementCounts: PruningData = copy(referenceCount = referenceCount - 1)

    def update(newData: PruningData): PruningData = {
      copy(referenceCount = referenceCount + newData.referenceCount, lastUsedBLock =  newData.lastUsedBLock)
    }
  }

  object PruningData {
    val empty = PruningData(0, 0)

    import io.iohk.ethereum.utils.ByteUtils.padLeft


    def toBytes(data: PruningData): Array[Byte] = {
      val refs = padLeft(ByteString(BigInt(data.referenceCount).toByteArray), 4)
      val lastused = padLeft(ByteString(data.lastUsedBLock.toByteArray), 4)
      (refs ++ lastused).toArray
    }

    def fromBytes(bytes: Array[Byte]): PruningData = {
      PruningData(BigInt(bytes.take(4)).toInt, BigInt(bytes.drop(4)))
    }
  }


  type RefKey = ByteString
  import java.util.concurrent.Executors
  val threadpool = Executors.newFixedThreadPool(10)

  val ioThreadPool: ExecutionContext = ExecutionContext.fromExecutor(threadpool)

  def touchedNodesKey(bn: BigInt): ByteString = {
    ByteString("touch".getBytes ++ bn.toByteArray)
  }

  def toNodeHashes(bytes: Array[Byte]): Seq[ByteString] = {
    ByteString(bytes).grouped(32).toSeq
  }

  def getRefCountKey(key: ByteString): ByteString = {
    ByteString("refs".getBytes) ++ key
  }


  override def prune(blockNumber: BigInt, nodeStorage: NodesStorage, inMemory: Boolean): Unit = {
    var toDel = List.empty[ByteString]
    val touchedKey = touchedNodesKey(blockNumber)
    val nodes = nodeStorage.get(touchedKey).getOrElse(Array())
    nodes.grouped(32).foreach {nodekey =>
      val key = getRefCountKey(ByteString(nodekey))
      nodeStorage.get(key).map(PruningData.fromBytes).foreach {ref =>
        if (ref.referenceCount == 0 && ref.lastUsedBLock <= blockNumber ){
          toDel = key :: toDel
        }
      }
    }
    //println(s"Deleting ${toDel.size} nodes")
    nodeStorage.updateCond(Seq(touchedKey) ++ toDel, Nil, inMemory)
  }


  /**
    * Looks for the StoredNode snapshots based on block number and saves (or deletes) them
    *
    * @param blockNumber BlockNumber to rollback
    * @param nodeStorage NodeStorage
    */
  override def rollback(blockNumber: BigInt, nodeStorage: NodesStorage, inMemory: Boolean): Unit = ()

}
