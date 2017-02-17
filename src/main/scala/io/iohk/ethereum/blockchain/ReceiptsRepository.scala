package io.iohk.ethereum.blockchain

import akka.util.ByteString
import io.iohk.ethereum.db.storage.ReceiptStorage
import io.iohk.ethereum.network.p2p.messages.PV63.Receipt


trait ReceiptsRepository {

  protected def receiptStorage: ReceiptStorage

  def save(blockHash: ByteString, receipts: Seq[Receipt]): Unit = receiptStorage.put(blockHash, receipts)

  def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]] = receiptStorage.get(blockhash)
}
