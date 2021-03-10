package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPList, RLPSerializable, rawDecode}

/**
  * This class represent a block as a header and a body which are returned in two different messages
  *
  * @param header Block header
  * @param body   Block body
  */
case class Block(header: BlockHeader, body: BlockBody) {
  override def toString: String =
    s"Block { header: $header, body: $body }"

  def idTag: String =
    header.idTag

  def number: BigInt = header.number

  def hash: ByteString = header.hash

  val hasCheckpoint: Boolean = header.hasCheckpoint

  def isParentOf(child: Block): Boolean = header.isParentOf(child.header)
}

object Block {

  implicit class BlockEnc(val obj: Block) extends RLPSerializable {
    import io.iohk.ethereum.network.p2p.messages.PV60.SignedTransactions._

    override def toRLPEncodable: RLPEncodeable = RLPList(
      obj.header.toRLPEncodable,
      RLPList(obj.body.transactionList.map(_.toRLPEncodable): _*),
      RLPList(obj.body.uncleNodesList.map(_.toRLPEncodable): _*)
    )
  }

  implicit class BlockDec(val bytes: Array[Byte]) extends AnyVal {
    import io.iohk.ethereum.network.p2p.messages.PV60.SignedTransactions._
    def toBlock: Block = rawDecode(bytes) match {
      case RLPList(header: RLPList, stx: RLPList, uncles: RLPList) =>
        Block(
          header.toBlockHeader,
          BlockBody(
            stx.items.map(_.toSignedTransaction),
            uncles.items.map(_.toBlockHeader)
          )
        )
      case _ => throw new RuntimeException("Cannot decode block")
    }
  }

  def size(block: Block): Long = (block.toBytes: Array[Byte]).length
}
