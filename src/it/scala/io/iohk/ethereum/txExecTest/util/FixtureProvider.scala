package io.iohk.ethereum.txExecTest.util

import java.io.Closeable

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain.{Block, BlockHeader, BlockchainStorages, Receipt}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, BlockHeaderImplicits}
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp._
import org.spongycastle.util.encoders.Hex

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

    val storages: BlockchainStorages = new BlockchainStorages {
      override val receiptStorage: ReceiptStorage = new ReceiptStorage(EphemDataSource())
      override val evmCodeStorage: EvmCodeStorage = new EvmCodeStorage(EphemDataSource())
      override val blockHeadersStorage: BlockHeadersStorage = new BlockHeadersStorage(EphemDataSource())
      override val blockNumberMappingStorage: BlockNumberMappingStorage = new BlockNumberMappingStorage(EphemDataSource())
      override val mptNodeStorage: MptNodeStorage = new MptNodeStorage(EphemDataSource())
      override val blockBodiesStorage: BlockBodiesStorage = new BlockBodiesStorage(EphemDataSource())
      override val totalDifficultyStorage: TotalDifficultyStorage = new TotalDifficultyStorage(EphemDataSource())
      override val nodeStorage: NodeStorage = new NodeStorage(EphemDataSource())
    }

    val blocksToInclude = fixtures.blockByNumber.toSeq.sortBy { case (number, _) => number }.takeWhile { case (number, _) => number <= blockNumber }

    blocksToInclude.foreach { case (_, block) =>
      storages.blockBodiesStorage.put(block.header.hash, fixtures.blockBodies(block.header.hash))
      storages.blockHeadersStorage.put(block.header.hash, fixtures.blockHeaders(block.header.hash))
      storages.blockNumberMappingStorage.put(block.header.number, block.header.hash)
      fixtures.receipts.get(block.header.hash).foreach(r => storages.receiptStorage.put(block.header.hash, r))

      def traverse(nodeHash: ByteString): Unit = fixtures.stateMpt.get(nodeHash).orElse(fixtures.contractMpts.get(nodeHash)) match {
        case Some(m: MptBranch) =>
          storages.mptNodeStorage.put(m)
          storages.nodeStorage.put(m.hash, encode(m: MptNode)(MptNode.rlpEncDec))
          m.children.collect { case Left(MptHash(hash)) if hash.nonEmpty => hash }.foreach(traverse)

        case Some(m: MptExtension) =>
          storages.mptNodeStorage.put(m)
          storages.nodeStorage.put(m.hash, encode(m: MptNode)(MptNode.rlpEncDec))
          m.child match {
            case Left(MptHash(hash)) if hash.nonEmpty => traverse(hash)
            case _ =>
          }

        case Some(m: MptLeaf) =>
          storages.mptNodeStorage.put(m)
          storages.nodeStorage.put(m.hash, encode(m: MptNode)(MptNode.rlpEncDec))
          Try(m.getAccount).toOption.foreach { account =>
            if (account.codeHash != DumpChainActor.emptyEvm) {
              storages.evmCodeStorage.put(account.codeHash, fixtures.evmCode(account.codeHash))
            }
            if (account.storageRoot != DumpChainActor.emptyStorage) {
              traverse(account.storageRoot)
            }
          }

        case None =>
      }

      traverse(block.header.stateRoot)
    }

    storages
  }

  def loadFixtures(path: String): Fixture = {
    val bodies: Map[ByteString, BlockBody] = withClose(Source.fromFile(getClass.getResource(s"$path/bodies.txt").getPath))(_.getLines()
      .map(s => s.split(" ").toSeq).collect {
      case Seq(h, v) =>
        val key = ByteString(Hex.decode(h))
        val value: BlockBody = decode(Hex.decode(v))(BlockBody.rlpEncDec)
        key -> value
    }.toMap)

    val headers: Map[ByteString, BlockHeader] = withClose(Source.fromFile(getClass.getResource(s"$path/headers.txt").getPath))(_.getLines()
      .map(s => s.split(" ").toSeq).collect {
      case Seq(h, v) =>
        val key = ByteString(Hex.decode(h))
        val value: BlockHeader = decode(Hex.decode(v))(BlockHeaderImplicits.headerRlpEncDec)
        key -> value
    }.toMap)

    val receipts: Map[ByteString, Seq[Receipt]] = withClose(Source.fromFile(getClass.getResource(s"$path/receipts.txt").getPath))(_.getLines()
      .map(s => s.split(" ").toSeq).collect {
      case Seq(h, v) =>
        val key = ByteString(Hex.decode(h))
        val value: Seq[Receipt] = fromRlpList(rawDecode(Hex.decode(v)).asInstanceOf[RLPList])(ReceiptImplicits.receiptRlpEncDec)
        key -> value
    }.toMap)

    val stateTree: Map[ByteString, MptNode] = withClose(Source.fromFile(getClass.getResource(s"$path/stateTree.txt").getPath))(_.getLines()
      .map(s => s.split(" ").toSeq).collect {
      case Seq(h, v) =>
        val key = ByteString(Hex.decode(h))
        val value: MptNode = decode(Hex.decode(v))(MptNode.rlpEncDec)
        key -> value
    }.toMap)

    val contractTrees: Map[ByteString, MptNode] = withClose(Source.fromFile(getClass.getResource(s"$path/contractTrees.txt").getPath))(_.getLines()
      .map(s => s.split(" ").toSeq).collect {
      case Seq(h, v) =>
        val key = ByteString(Hex.decode(h))
        val value: MptNode = decode(Hex.decode(v))(MptNode.rlpEncDec)
        key -> value
    }.toMap)

    val evmCode: Map[ByteString, ByteString] = withClose(Source.fromFile(getClass.getResource(s"$path/evmCode.txt").getPath))(_.getLines()
      .map(s => s.split(" ").toSeq).collect {
      case Seq(h, v) => ByteString(Hex.decode(h)) -> ByteString(Hex.decode(v))
    }.toMap)

    val blocks = headers.toList.sortBy { case (hash, _) => Hex.toHexString(hash.toArray[Byte]) }.map { case (_, header) => header }
      .zip(bodies.toList.sortBy { case (hash, _) => Hex.toHexString(hash.toArray[Byte]) }.map { case (_, body) => body })
      .map { case (h, b) => Block(h, b) }

    Fixture(blocks.map(b => b.header.number -> b).toMap, blocks.map(b => b.header.hash -> b).toMap, headers, bodies, receipts, stateTree, contractTrees, evmCode)
  }

  private def withClose[A, B <: Closeable] (closeable: B) (f: B => A): A = try { f(closeable) } finally { closeable.close() }
}
