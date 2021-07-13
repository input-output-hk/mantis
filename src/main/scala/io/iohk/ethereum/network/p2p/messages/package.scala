package io.iohk.ethereum.network.p2p

import io.iohk.ethereum.rlp.RLPEncodeable
import io.iohk.ethereum.rlp._

package messages {
  sealed trait ProtocolFamily
  object ProtocolFamily {
    final case object ETH extends ProtocolFamily
    final case object ETC extends ProtocolFamily

    implicit class ProtocolFamilyEnc(val msg: ProtocolFamily) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = msg match {
        case ETH => RLPValue("eth".getBytes())
        case ETC => RLPValue("etc".getBytes())
      }
    }
  }
}

package object messages {

  val SubProtocolOffset = 0x10

  object Codes {
    val StatusCode: Int = SubProtocolOffset + 0x00
    val NewBlockHashesCode: Int = SubProtocolOffset + 0x01
    val SignedTransactionsCode: Int = SubProtocolOffset + 0x02
    val GetBlockHeadersCode: Int = SubProtocolOffset + 0x03
    val BlockHeadersCode: Int = SubProtocolOffset + 0x04
    val GetBlockBodiesCode: Int = SubProtocolOffset + 0x05
    val BlockBodiesCode: Int = SubProtocolOffset + 0x06
    val NewBlockCode: Int = SubProtocolOffset + 0x07
    // This message is removed in ETH62 and this code is reused in ETH65 with different msg type
    val BlockHashesFromNumberCode: Int = SubProtocolOffset + 0x08
    val GetNodeDataCode: Int = SubProtocolOffset + 0x0d
    val NodeDataCode: Int = SubProtocolOffset + 0x0e
    val GetReceiptsCode: Int = SubProtocolOffset + 0x0f
    val ReceiptsCode: Int = SubProtocolOffset + 0x10
  }
}
