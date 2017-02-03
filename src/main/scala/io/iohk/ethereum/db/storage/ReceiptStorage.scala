package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.network.p2p.messages.PV63.Receipt
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}

import ReceiptStorage._

class ReceiptStorage(val dataSource: DataSource) extends KeyValueStorage[BlockHash, Seq[Receipt]] {
  type T = ReceiptStorage

  val namespace: Byte = Namespaces.ReceiptsNamespace
  def keySerializer: BlockHash => Array[Byte] = _.toArray
  def valueSerializer: Seq[Receipt] => Array[Byte] = rlpEncode(_)(seqEncDec[Receipt])
  def valueDeserializer: Array[Byte] => Seq[Receipt] = rlpDecode[Seq[Receipt]](_)(seqEncDec[Receipt])

  protected def apply(dataSource: DataSource): ReceiptStorage = new ReceiptStorage(dataSource)
}

object ReceiptStorage {
  type BlockHash = ByteString
}
