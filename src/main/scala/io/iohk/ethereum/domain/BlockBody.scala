package io.iohk.ethereum.domain

import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPList, RLPSerializable, rawDecode}

case class BlockBody(transactionList: Seq[SignedTransaction], uncleNodesList: Seq[BlockHeader]) {
  override def toString: String =
    s"BlockBody{ transactionList: $transactionList, uncleNodesList: $uncleNodesList }"

  def toShortString: String =
    s"BlockBody { transactionsList: ${transactionList.map(_.hashAsHexString)}, uncleNodesList: ${uncleNodesList.map(_.hashAsHexString)} }"

  lazy val numberOfTxs: Int = transactionList.size

  lazy val numberOfUncles: Int = uncleNodesList.size
}

object BlockBody {

  val empty = BlockBody(Seq.empty, Seq.empty)

  def blockBodyToRlpEncodable(
      blockBody: BlockBody,
      signedTxToRlpEncodable: SignedTransaction => RLPEncodeable,
      blockHeaderToRlpEncodable: BlockHeader => RLPEncodeable
  ): RLPEncodeable =
    RLPList(
      RLPList(blockBody.transactionList.map(signedTxToRlpEncodable): _*),
      RLPList(blockBody.uncleNodesList.map(blockHeaderToRlpEncodable): _*)
    )

  implicit class BlockBodyEnc(msg: BlockBody) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable = {
      import io.iohk.ethereum.network.p2p.messages.PV60.SignedTransactions._

      blockBodyToRlpEncodable(
        msg,
        stx => SignedTransactionEnc(stx).toRLPEncodable,
        header => BlockHeaderEnc(header).toRLPEncodable
      )
    }
  }

  implicit class BlockBlodyDec(val bytes: Array[Byte]) extends AnyVal {
    def toBlockBody: BlockBody = BlockBodyRLPEncodableDec(rawDecode(bytes)).toBlockBody
  }

  def rlpEncodableToBlockBody(
      rlpEncodeable: RLPEncodeable,
      rlpEncodableToSignedTransaction: RLPEncodeable => SignedTransaction,
      rlpEncodableToBlockHeader: RLPEncodeable => BlockHeader
  ): BlockBody =
    rlpEncodeable match {
      case RLPList((transactions: RLPList), (uncles: RLPList)) =>
        BlockBody(
          transactions.items.map(rlpEncodableToSignedTransaction),
          uncles.items.map(rlpEncodableToBlockHeader)
        )
      case _ => throw new RuntimeException("Cannot decode BlockBody")
    }

  implicit class BlockBodyRLPEncodableDec(val rlpEncodeable: RLPEncodeable) {
    def toBlockBody: BlockBody = {
      import io.iohk.ethereum.network.p2p.messages.PV60.SignedTransactions._

      rlpEncodableToBlockBody(
        rlpEncodeable,
        rlp => SignedTransactionRlpEncodableDec(rlp).toSignedTransaction,
        rlp => BlockHeaderDec(rlp).toBlockHeader
      )

    }
  }

}
