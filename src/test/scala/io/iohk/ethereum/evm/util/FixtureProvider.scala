package io.iohk.ethereum.evm.util

import akka.util.ByteString
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, BlockHeaderImplicits}
import io.iohk.ethereum.network.p2p.messages.PV63.{MptNode, Receipt}
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp._
import org.spongycastle.util.encoders.Hex

import scala.io.Source

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
  def loadFixtures(path: String): Fixture = {
    val bodies: Map[ByteString, BlockBody] = Source.fromFile(getClass.getResource(s"$path/bodies.txt").getPath).getLines()
      .map(s => s.split(" ").toSeq).collect {
      case Seq(h, v) =>
        val key = ByteString(Hex.decode(h))
        val value: BlockBody = decode(Hex.decode(v))(BlockBody.rlpEncDec)
        key -> value
    }.toMap

    val headers: Map[ByteString, BlockHeader] = Source.fromFile(getClass.getResource(s"$path/headers.txt").getPath).getLines()
      .map(s => s.split(" ").toSeq).collect {
      case Seq(h, v) =>
        val key = ByteString(Hex.decode(h))
        val value: BlockHeader = decode(Hex.decode(v))(BlockHeaderImplicits.headerRlpEncDec)
        key -> value
    }.toMap

    val receipts: Map[ByteString, Seq[Receipt]] = Source.fromFile(getClass.getResource(s"$path/receipts.txt").getPath).getLines()
      .map(s => s.split(" ").toSeq).collect {
      case Seq(h, v) =>
        val key = ByteString(Hex.decode(h))
        val value: Seq[Receipt] = fromRlpList(rawDecode(Hex.decode(v)).asInstanceOf[RLPList])(Receipt.rlpEncDec)
        key -> value
    }.toMap

    val stateTree: Map[ByteString, MptNode] = Source.fromFile(getClass.getResource(s"$path/stateTree.txt").getPath).getLines()
      .map(s => s.split(" ").toSeq).collect {
      case Seq(h, v) =>
        val key = ByteString(Hex.decode(h))
        val value: MptNode = decode(Hex.decode(v))(MptNode.rlpEncDec)
        key -> value
    }.toMap

    val contractTrees: Map[ByteString, MptNode] = Source.fromFile(getClass.getResource(s"$path/contractTrees.txt").getPath).getLines()
      .map(s => s.split(" ").toSeq).collect {
      case Seq(h, v) =>
        val key = ByteString(Hex.decode(h))
        val value: MptNode = decode(Hex.decode(v))(MptNode.rlpEncDec)
        key -> value
    }.toMap

    val evmCode: Map[ByteString, ByteString] = Source.fromFile(getClass.getResource(s"$path/evmCode.txt").getPath).getLines()
      .map(s => s.split(" ").toSeq).collect {
      case Seq(h, v) => ByteString(Hex.decode(h)) -> ByteString(Hex.decode(v))
    }.toMap

    val blocks = headers.toList.sortBy { case (hash, _) => Hex.toHexString(hash.toArray[Byte]) }.map { case (_, header) => header }
      .zip(bodies.toList.sortBy { case (hash, _) => Hex.toHexString(hash.toArray[Byte]) }.map { case (_, body) => body })
      .map { case (h, b) => Block(h, b) }

    Fixture(blocks.map(b => b.header.number -> b).toMap, blocks.map(b => b.header.hash -> b).toMap, headers, bodies, receipts, stateTree, contractTrees, evmCode)
  }
}
