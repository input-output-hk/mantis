package io.iohk.ethereum.mpt

import java.util.Map.Entry

import akka.util.ByteString
import io.iohk.ethereum.db.storage.Namespaces
import io.iohk.ethereum.db.storage.NodeStorage.NodeHash
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.nodebuilder.{BlockChainBuilder, ShutdownHookBuilder, StorageBuilder, Node => NodeApp}
import io.iohk.ethereum.utils.Logger
import org.spongycastle.util.encoders.Hex

import scala.util.{Failure, Success, Try}

object Export extends StorageBuilder with
  BlockChainBuilder with
  ShutdownHookBuilder with
  Logger {


  case object RolledUpStats {
    var keyCount: BigInt = 0
    var keyByteCount: BigInt = 0
    var valueByteCount: BigInt = 0

    def totalByteCount = keyByteCount + valueByteCount

    val stats: Map[IndexedSeq[Byte], Stats] = Map(
      Namespaces.AppStateNamespace -> Stats("AppStateNamespace"),
      Namespaces.BodyNamespace -> Stats("BodyNamespace"),
      Namespaces.CodeNamespace -> Stats("CodeNamespace"),
      Namespaces.FastSyncStateNamespace -> Stats("FastSyncStateNamespace"),
      Namespaces.HeaderNamespace -> Stats("HeaderNamespace"),
      Namespaces.HeightsNamespace -> Stats("HeightsNamespace"),
      Namespaces.KnownNodesNamespace -> Stats("KnownNodesNamespace"),
      Namespaces.NodeNamespace -> Stats("NodeNamespace"),
      Namespaces.ReceiptsNamespace -> Stats("ReceiptsNamespace"),
      Namespaces.TotalDifficultyNamespace -> Stats("TotalDifficultyNamespace"),
      Namespaces.TransactionMappingNamespace -> Stats("TransactionMappingNamespace")
    )

    def apply(key: Array[Byte], value:Array[Byte]): Unit = {
      keyCount += 1
      keyByteCount += key.length
      valueByteCount += value.length
      val keyCode = IndexedSeq(key.head)
      stats(keyCode)(key, value)
    }
  }

  case class Stats(name: String) {
    var keyCount: BigInt = 0
    var keyByteCount: BigInt = 0
    var valueByteCount: BigInt = 0

    def apply(key: Array[Byte], value:Array[Byte]): Unit = {
      keyCount += 1
      keyByteCount += key.length
      valueByteCount += value.length
    }

    def totalByteCount = keyByteCount + valueByteCount

    def percentageSize: Double = {
      if(totalByteCount > 0) {
        ((totalByteCount.toDouble / RolledUpStats.totalByteCount.toDouble) * 100).round
      } else 0
    }
  }

  def tryAndLogFailure(f: () => Any): Unit = Try(f()) match {
    case Failure(e) => log.warn("Error while shutting down...", e.getMessage)
    case Success(_) =>
  }

  override def shutdown(): Unit = {
    tryAndLogFailure(() => storagesInstance.dataSources.closeAll())
  }

  def main(args: Array[String]): Unit = {
    try {
      //export(args)
      println("Begin analysis ... ")
      listMissingBlocks()
      //removeKnownNodes()
      println("Done!")
    } finally {
      shutdown()
    }
    /*
    Num key values is 419196, total key size 12772256, total value size 106668601
Total bytes = 119440857
Exported 19399226
Export done ...
Num key values is 116398338, total key size 3813442561, total value size 41312341905

     */
  }

  def listMissingBlocks(): Unit = {
    var headerMissing = 0
    var bodyMissing = 0
    var headerFound = 0

    val start = System.currentTimeMillis()

    (0 to 4300000) foreach { i =>
      val header = blockchain.getBlockHeaderByNumber(i) match {
        case Some(header) => headerFound += 1; if(false == blockchain.asInstanceOf[BlockchainImpl].hasBlockBodyByHash(header.hash)) {println(s"BlockBody $i is missing"); bodyMissing += 1; }
        case _ => println(s"BlockHeader $i is missing"); headerMissing += 1
      }
      if(i % 200000 == 0) {
        var now: Double = System.currentTimeMillis() - start
        println(s"Up to $i. Header found = $headerFound, header missing = $headerMissing, body missing = $bodyMissing in ${now/1000/60}")
      }
    }
    var taken: Double = System.currentTimeMillis() - start
    println(s"Header found = $headerFound, header missing = $headerMissing, body missing = $bodyMissing in ${taken/1000/60}")
  }

  def removeKnownNodes(): Unit = {
    val known = storagesInstance.storages.knownNodesStorage.getKnownNodes()
    println("Known ... ")
    known.foreach(u => println(u))
    storagesInstance.storages.knownNodesStorage.updateKnownNodes(toAdd = Set(), toRemove = known)
    println("Known deleted ... ")
    val nowKnown = storagesInstance.storages.knownNodesStorage.getKnownNodes()
    println("Now known ... ")
    nowKnown.foreach(u => println(u))
    println("Now known ... END ")
  }

  def analysis(): Unit = {

    foreachStorage((k,v) => RolledUpStats(k, v))

    println(s"Num key values is ${RolledUpStats.keyCount}" +
      s", total key size ${RolledUpStats.keyByteCount}" +
      s", total value size ${RolledUpStats.valueByteCount}")

    RolledUpStats.stats.values.foreach(s => println(s"${s.name} -> ${s.percentageSize}"))
  }

  def foreachStorage(f: (Array[Byte], Array[Byte]) => Unit): Unit = {

    val iter = storagesInstance.dataSource.iterator()

    try {
      iter.seekToFirst()

      while (iter.hasNext) {
        val elem: Entry[Array[Byte], Array[Byte]] = iter.next()
        val key = elem.getKey
        f(key, elem.getValue)
      }
    } finally iter.close()

  }

  type ProcessCache[T] = List[T] => Unit

  class Cache[T](maxBufferSize: Int, f: ProcessCache[T], private var buffer: List[T] = List()) {


    def cacheOr(node: T){
      buffer :+ node
      if(buffer.size > maxBufferSize) {
        f(buffer)
        buffer = List()
      }

    }

    def close() : Unit = {
      f(buffer)
      buffer = List()
    }
  }

  /*def export(args: Array[String]): Unit = {

    import MerklePatriciaTrie._

    def writeNodesToMoveStorage(nodes: List[Node]): Unit = {
      val kvs = nodes.map(n => ByteString(n.hash) -> n.encode)
      storagesInstance.storages.moveNodeStorage.update(Seq(), kvs)
    }

    def deleteNodeKeys(keys: List[NodeHash]): Unit = {
      storagesInstance.storages.nodeStorage.update(keys, Seq())
    }

    def deleteMoveNodeKeys(keys: List[NodeHash]): Unit = {
      storagesInstance.storages.moveNodeStorage.update(keys, Seq())
    }

    def renameKeys(kvs: List[(Array[Byte], Array[Byte])]): Unit = {
      val mapped = kvs.map(kv => ByteString(kv._1) -> kv._2)
      storagesInstance.storages.nodeStorage.update(Seq(), mapped)
    }

    var count = 0
    val cache = new Cache[Node](1000, writeNodesToMoveStorage)

    def save(node: Node): Node = {
      count += 1
      cache.cacheOr(node)
      if (count % 5000 == 0) println(s"Count now $count")
      node
    }

    def exports(nodes: Seq[Node]): Unit = nodes map export

    def export(node: Node): Unit = save(node) match {
      case ExtensionNode(_, Right(n), _) => export(n)
      case ExtensionNode(_, Left(bs), _) =>
        val n = MerklePatriciaTrie.getNode(bs, storagesInstance.storages.nodeStorage)
        export(n)
      case BranchNode(children, _, _) =>
        exports(children.collect({
          case Some(Right(n)) => n
          case Some(Left(bs)) => MerklePatriciaTrie.getNode(bs, storagesInstance.storages.nodeStorage)
        }))
      case _ =>
    }

    val topBlock = storagesInstance.storages.appStateStorage.getBestBlockNumber()

    val header = blockchain.getBlockHeaderByNumber(topBlock)
    val stateTrieRoot = header.getOrElse(throw new Error("Cannot continue")).stateRoot


    val n = MerklePatriciaTrie.getNode(stateTrieRoot.toArray, storagesInstance.storages.nodeStorage)

    export(n)
    cache.close()

    println(s"Exported to 'm' $count")

    {
      val cacheToDelete = new Cache[NodeHash](1000, deleteNodeKeys)
      foreachStorage((key, value) => {
        if (key.head == 'n'.toByte) cacheToDelete.cacheOr(ByteString(key.tail))
      })

      cacheToDelete.close()
    }

    println(s"Done deleting 'n's ")

    {
      val cacheToRename = new Cache[(Array[Byte], Array[Byte])](1000, renameKeys)
      foreachStorage((key, value) => {
        if (key.head == 'm'.toByte) cacheToRename.cacheOr((key.tail, value))
      })

      cacheToRename.close()
    }
    println(s"Done renaming 'm's to 'n's")

    {
      val cacheToDeleteMoved = new Cache[NodeHash](1000, deleteMoveNodeKeys)

      foreachStorage((key, value) => {
        if (key.head == 'm'.toByte) cacheToDeleteMoved.cacheOr(ByteString(key.tail))
      })
      cacheToDeleteMoved.close()
    }
    println(s"Done deleting 'm's")
  }*/

}

