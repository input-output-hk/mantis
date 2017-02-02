package io.iohk.ethereum.db

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.messages.PV63.Receipt
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}

class ReceiptStorage(val dataSource: DataSource) extends KeyValueStorage[ByteString, Seq[Receipt]] {
  type T = ReceiptStorage
  override def keySerializer: ByteString => Array[Byte] = _.toArray
  override def valueSerializer: Seq[Receipt] => Array[Byte] = rlpEncode(_)(seqEncDec[Receipt])
  override def valueDeserializer: Array[Byte] => Seq[Receipt] = rlpDecode[Seq[Receipt]](_)(seqEncDec[Receipt])

  def apply(dataSource: DataSource): ReceiptStorage = new ReceiptStorage(dataSource)
}
