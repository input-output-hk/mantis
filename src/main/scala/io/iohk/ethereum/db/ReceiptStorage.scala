package io.iohk.ethereum.db

import io.iohk.ethereum.network.p2p.messages.PV63.Receipt
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}

object ReceiptStorage {
  type ReceiptStorage = KeyValueStorage[Array[Byte], Seq[Receipt]]

  def apply(dataSource: DataSource): ReceiptStorage = new ReceiptStorage(
    dataSource = dataSource,
    keySerializer = identity,
    valueSerializer = rlpEncode(_)(seqEncDec[Receipt]),
    valueDeserializer = rlpDecode[Seq[Receipt]](_)(seqEncDec[Receipt])
  )
}
