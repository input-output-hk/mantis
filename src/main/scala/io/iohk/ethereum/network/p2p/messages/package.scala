package io.iohk.ethereum.network.p2p

import io.iohk.ethereum.network.p2p.Message.Version

package object messages {
  object ProtocolVersions {
    val PV61: Version = 61
    val PV62: Version = 62
    val PV63: Version = 63
    //FIXME This PV is considered WIP and needs to be reassessed
    val PV164: Version = 164

    val SubProtocolOffset = 0x10
  }

  object Codes {
    val StatusCode: Int = ProtocolVersions.SubProtocolOffset + 0x00
    val NewBlockHashesCode: Int = ProtocolVersions.SubProtocolOffset + 0x01
    val SignedTransactionsCode: Int = ProtocolVersions.SubProtocolOffset + 0x02
    val GetBlockHeadersCode: Int = ProtocolVersions.SubProtocolOffset + 0x03
    val BlockHeadersCode: Int = ProtocolVersions.SubProtocolOffset + 0x04
    val GetBlockBodiesCode: Int = ProtocolVersions.SubProtocolOffset + 0x05
    val BlockBodiesCode: Int = ProtocolVersions.SubProtocolOffset + 0x06
    val NewBlockCode: Int = ProtocolVersions.SubProtocolOffset + 0x07
    // This message is removed in PV62 and this code is reused in PV65 with different msg type
    val BlockHashesFromNumberCode: Int = ProtocolVersions.SubProtocolOffset + 0x08
    val GetNodeDataCode: Int = ProtocolVersions.SubProtocolOffset + 0x0d
    val NodeDataCode: Int = ProtocolVersions.SubProtocolOffset + 0x0e
    val GetReceiptsCode: Int = ProtocolVersions.SubProtocolOffset + 0x0f
    val ReceiptsCode: Int = ProtocolVersions.SubProtocolOffset + 0x10
  }
}
