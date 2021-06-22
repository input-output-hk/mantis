package io.iohk.ethereum.txExecTest.util

import java.io.Closeable

import akka.util.ByteString
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.domain.BlockBody._
import io.iohk.ethereum.network.p2p.messages.ETH63._
import MptNodeEncoders._
import ReceiptImplicits._
import io.iohk.ethereum.db.cache.{AppCaches, LruCache}
import io.iohk.ethereum.db.components.EphemDataSourceComponent
import io.iohk.ethereum.db.storage.NodeStorage.NodeHash
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.mpt.{BranchNode, ExtensionNode, HashNode, LeafNode, MptNode}
import io.iohk.ethereum.utils.Config
import org.bouncycastle.util.encoders.Hex

import scala.io.Source
import scala.util.Try

object FixtureProvider {

  case class Fixture(
      blockByNumber: Map[BigInt, Block],
      blockByHash: Map[ByteString, Block],
      blockHeaders: Map[ByteString, BlockHeader],
      blockBodies: Map[ByteString, BlockBody],
      receipts: Map[ByteString, Seq[Receipt]],
      stateMpt: Map[ByteString, MptNode],
      contractMpts: Map[ByteString, MptNode],
      evmCode: Map[ByteString, ByteString]
  )

  // scalastyle:off
  def prepareStorages(blockNumber: BigInt, fixtures: Fixture): BlockchainStorages = {

    val storages: BlockchainStorages = new BlockchainStorages with AppCaches with EphemDataSourceComponent {

      override val receiptStorage: ReceiptStorage = new ReceiptStorage(dataSource)
      override val evmCodeStorage: EvmCodeStorage = new EvmCodeStorage(dataSource)
      override val blockHeadersStorage: BlockHeadersStorage = new BlockHeadersStorage(dataSource)
      override val blockNumberMappingStorage: BlockNumberMappingStorage = new BlockNumberMappingStorage(dataSource)
      override val blockBodiesStorage: BlockBodiesStorage = new BlockBodiesStorage(dataSource)
      override val chainWeightStorage: ChainWeightStorage = new ChainWeightStorage(dataSource)
      override val transactionMappingStorage: TransactionMappingStorage = new TransactionMappingStorage(dataSource)
      override val appStateStorage: AppStateStorage = new AppStateStorage(dataSource)
      val nodeStorage: NodeStorage = new NodeStorage(dataSource)
      val cachedNodeStorage: CachedNodeStorage = new CachedNodeStorage(nodeStorage, caches.nodeCache)
      val pruningMode: PruningMode = ArchivePruning
      override val stateStorage: StateStorage =
        StateStorage(
          pruningMode,
          nodeStorage,
          cachedNodeStorage,
          new LruCache[NodeHash, HeapEntry](
            Config.InMemoryPruningNodeCacheConfig,
            Some(CachedReferenceCountedStorage.saveOnlyNotificationHandler(nodeStorage))
          )
        )
    }

    val blocksToInclude = fixtures.blockByNumber.toSeq.sortBy { case (number, _) => number }.takeWhile {
      case (number, _) => number <= blockNumber
    }

    blocksToInclude.foreach { case (_, block) =>
      val receiptsUpdates = fixtures.receipts
        .get(block.header.hash)
        .map(r => storages.receiptStorage.put(block.header.hash, r))
        .getOrElse(storages.receiptStorage.emptyBatchUpdate)

      storages.blockBodiesStorage
        .put(block.header.hash, fixtures.blockBodies(block.header.hash))
        .and(storages.blockHeadersStorage.put(block.header.hash, fixtures.blockHeaders(block.header.hash)))
        .and(storages.blockNumberMappingStorage.put(block.header.number, block.header.hash))
        .and(receiptsUpdates)
        .commit()

      def traverse(nodeHash: ByteString): Unit =
        fixtures.stateMpt.get(nodeHash).orElse(fixtures.contractMpts.get(nodeHash)) match {
          case Some(m: BranchNode) =>
            storages.stateStorage.saveNode(ByteString(m.hash), m.toBytes, block.header.number)
            m.children.collect { case HashNode(hash) => traverse(ByteString(hash)) }

          case Some(m: ExtensionNode) =>
            storages.stateStorage.saveNode(ByteString(m.hash), m.toBytes, block.header.number)
            m.next match {
              case HashNode(hash) if hash.nonEmpty => traverse(ByteString(hash))
              case _ =>
            }

          case Some(m: LeafNode) =>
            import AccountImplicits._
            storages.stateStorage.saveNode(ByteString(m.hash), m.toBytes, block.header.number)
            Try(m.value.toArray[Byte].toAccount).toOption.foreach { account =>
              if (account.codeHash != DumpChainActor.emptyEvm) {
                storages.evmCodeStorage.put(account.codeHash, fixtures.evmCode(account.codeHash)).commit()
              }
              if (account.storageRoot != DumpChainActor.emptyStorage) {
                traverse(account.storageRoot)
              }
            }

          case _ =>

        }

      traverse(block.header.stateRoot)
    }

    storages
  }

  def loadFixtures(path: String): Fixture = {
    val bodies: Map[ByteString, BlockBody] =
      withClose(Source.fromFile(getClass.getResource(s"$path/bodies.txt").getPath))(
        _.getLines()
          .map(s => s.split(" ").toSeq)
          .collect { case Seq(h, v) =>
            val key = ByteString(Hex.decode(h))
            val value: BlockBody = Hex.decode(v).toBlockBody
            key -> value
          }
          .toMap
      )

    val headers: Map[ByteString, BlockHeader] =
      withClose(Source.fromFile(getClass.getResource(s"$path/headers.txt").getPath))(
        _.getLines()
          .map(s => s.split(" ").toSeq)
          .collect { case Seq(h, v) =>
            val key = ByteString(Hex.decode(h))
            val value: BlockHeader = Hex.decode(v).toBlockHeader
            key -> value
          }
          .toMap
      )

    val receipts: Map[ByteString, Seq[Receipt]] =
      withClose(Source.fromFile(getClass.getResource(s"$path/receipts.txt").getPath))(
        _.getLines()
          .map(s => s.split(" ").toSeq)
          .collect { case Seq(h, v) =>
            val key = ByteString(Hex.decode(h))
            val value: Seq[Receipt] = Hex.decode(v).toReceipts
            key -> value
          }
          .toMap
      )

    val stateTree: Map[ByteString, MptNode] =
      withClose(Source.fromFile(getClass.getResource(s"$path/stateTree.txt").getPath))(
        _.getLines()
          .map(s => s.split(" ").toSeq)
          .collect { case Seq(h, v) =>
            val key = ByteString(Hex.decode(h))
            val value: MptNode = Hex.decode(v).toMptNode
            key -> value
          }
          .toMap
      )

    val contractTrees: Map[ByteString, MptNode] =
      withClose(Source.fromFile(getClass.getResource(s"$path/contractTrees.txt").getPath))(
        _.getLines()
          .map(s => s.split(" ").toSeq)
          .collect { case Seq(h, v) =>
            val key = ByteString(Hex.decode(h))
            val value: MptNode = Hex.decode(v).toMptNode
            key -> value
          }
          .toMap
      )

    val evmCode: Map[ByteString, ByteString] =
      withClose(Source.fromFile(getClass.getResource(s"$path/evmCode.txt").getPath))(
        _.getLines()
          .map(s => s.split(" ").toSeq)
          .collect { case Seq(h, v) =>
            ByteString(Hex.decode(h)) -> ByteString(Hex.decode(v))
          }
          .toMap
      )

    val blocks = headers.toList
      .sortBy { case (hash, _) => Hex.toHexString(hash.toArray[Byte]) }
      .map { case (_, header) => header }
      .zip(bodies.toList.sortBy { case (hash, _) => Hex.toHexString(hash.toArray[Byte]) }.map { case (_, body) =>
        body
      })
      .map { case (h, b) => Block(h, b) }

    Fixture(
      blocks.map(b => b.header.number -> b).toMap,
      blocks.map(b => b.header.hash -> b).toMap,
      headers,
      bodies,
      receipts,
      stateTree,
      contractTrees,
      evmCode
    )
  }

  private def withClose[A, B <: Closeable](closeable: B)(f: B => A): A = try { f(closeable) }
  finally { closeable.close() }
}
