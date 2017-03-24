package io.iohk.ethereum.domain

import io.iohk.ethereum.network.p2p.messages.CommonMessages
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, BlockHeaderImplicits}
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPEncoder, RLPList, encode}

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
  implicit val rlpEncDec = new RLPEncoder[Block] {
    override def encode(obj: Block): RLPEncodeable =
      RLPList(
        BlockHeaderImplicits.headerRlpEncDec.encode(obj.header),
        RLPList(obj.body.transactionList.map(CommonMessages.SignedTransactions.txRlpEncDec.encode): _*),
        RLPList(obj.body.uncleNodesList.map(BlockHeaderImplicits.headerRlpEncDec.encode): _*)
      )
  }

  def size(block: Block): Long = encode[Block](block).length
}
