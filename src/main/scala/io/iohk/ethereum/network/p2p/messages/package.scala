package io.iohk.ethereum.network.p2p

package object messages {
  object ProtocolVersions {
    val PV61: Capability = Capability("eth", 61.toByte)
    val PV62: Capability = Capability("eth", 62.toByte)
    val PV63: Capability = Capability("eth", 63.toByte)
    val PV64: Capability = Capability("etc", 64.toByte)

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
