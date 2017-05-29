package io.iohk.ethereum.domain

import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, BlockHeaderImplicits}
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPList, RLPSerializable, rawDecode}

/**
  * This class represent a block as a header and a body which are returned in two different messages
  *
  * @param header Block header
  * @param body   Block body
  */
case class Block(header: BlockHeader, body: BlockBody) {
  override def toString: String = {
    s"""BlockHeader {
       | header: $header
       | body: $body
     """.stripMargin
  }
}

object Block {

  implicit class BlockEnc(val obj: Block) extends RLPSerializable {
    import BlockHeaderImplicits._
    import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._

    override def toRLPEncodable: RLPEncodeable =  RLPList(
      obj.header.toRLPEncodable,
      RLPList(obj.body.transactionList.map(_.toRLPEncodable): _*),
      RLPList(obj.body.uncleNodesList.map(_.toRLPEncodable): _*)
    )
  }

  implicit class BlockDec(val bytes: Array[Byte]) extends AnyVal {
    import BlockHeaderImplicits._
    import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._
    def toBlock: Block = rawDecode(bytes) match {
      case RLPList(header: RLPList, stx: RLPList, uncles: RLPList) => Block(
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
