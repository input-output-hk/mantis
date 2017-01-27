package io.iohk.ethereum.db

import io.iohk.ethereum.mpt.DataSource
import io.iohk.ethereum.network.p2p.messages.PV63.Receipt
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}

object ReceiptStorage {
  def put(blockHash: Array[Byte], blockReceipts: Seq[Receipt], dataSource: DataSource): DataSource = {
    val encodedReceipts = rlpEncode(blockReceipts)(seqEncDec[Receipt])
    dataSource.update(blockHash, Seq(), Seq(blockHash -> encodedReceipts))
  }

  def get(blockHash: Array[Byte], dataSource: DataSource): Option[Seq[Receipt]] = {
    val encodedReceipts = dataSource.get(blockHash)
    encodedReceipts.map(rlpDecode[Seq[Receipt]](_)(seqEncDec[Receipt]))
  }

  def remove(blockHash: Array[Byte], dataSource: DataSource): DataSource = {
    dataSource.update(blockHash, Seq(blockHash), Seq())
  }
}
