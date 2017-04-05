package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.ReceiptStorage._
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}
import io.iohk.ethereum.network.p2p.messages.PV63.ReceiptImplicits.receiptRlpEncDec

/**
  * This class is used to store the Receipts, by using:
  *   Key: hash of the block to which the list of receipts belong
  *   Value: the list of receipts
  */
class ReceiptStorage(val dataSource: DataSource) extends KeyValueStorage[BlockHash, Seq[Receipt], ReceiptStorage] {
  val namespace: IndexedSeq[Byte] = Namespaces.ReceiptsNamespace
  def keySerializer: BlockHash => IndexedSeq[Byte] = identity
  def valueSerializer: Seq[Receipt] => IndexedSeq[Byte] =
    (receipts: Seq[Receipt]) => rlpEncode(receipts)(seqEncDec[Receipt]).toIndexedSeq
  def valueDeserializer: IndexedSeq[Byte] => Seq[Receipt] =
    (encodedReceipts: IndexedSeq[Byte]) => rlpDecode[Seq[Receipt]](encodedReceipts.toArray)(seqEncDec[Receipt])

  protected def apply(dataSource: DataSource): ReceiptStorage = new ReceiptStorage(dataSource)
}

object ReceiptStorage {
  type BlockHash = ByteString
}
