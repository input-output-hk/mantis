package io.iohk.ethereum.network.p2p

import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.SignedTransactions._
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.ETH61.BlockHashesFromNumber._
import io.iohk.ethereum.network.p2p.messages.ETH62.BlockBodies._
import io.iohk.ethereum.network.p2p.messages.ETH62.BlockHeaders._
import io.iohk.ethereum.network.p2p.messages.ETH62.GetBlockBodies._
import io.iohk.ethereum.network.p2p.messages.ETH62.GetBlockHeaders._
import io.iohk.ethereum.network.p2p.messages.ETH62.NewBlockHashes._
import io.iohk.ethereum.network.p2p.messages.ETH63.GetNodeData._
import io.iohk.ethereum.network.p2p.messages.ETH63.GetReceipts._
import io.iohk.ethereum.network.p2p.messages.ETH63.NodeData._
import io.iohk.ethereum.network.p2p.messages.ETH63.Receipts._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Ping._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Pong._
import io.iohk.ethereum.network.p2p.messages.WireProtocol._

object NetworkMessageDecoder extends MessageDecoder {

  override def fromBytes(msgCode: Int, payload: Array[Byte]): Message =
    msgCode match {
      case Disconnect.code => payload.toDisconnect
      case Ping.code       => payload.toPing
      case Pong.code       => payload.toPong
      case Hello.code      => payload.toHello
      case _               => throw new RuntimeException(s"Unknown message type: $msgCode")
    }

}

object ETC64MessageDecoder extends MessageDecoder {
  import io.iohk.ethereum.network.p2p.messages.ETC64.Status._
  import io.iohk.ethereum.network.p2p.messages.ETC64.NewBlock._

  def fromBytes(msgCode: Int, payload: Array[Byte]): Message =
    msgCode match {
      case Codes.StatusCode                => payload.toStatus
      case Codes.NewBlockCode              => payload.toNewBlock
      case Codes.GetNodeDataCode           => payload.toGetNodeData
      case Codes.NodeDataCode              => payload.toNodeData
      case Codes.GetReceiptsCode           => payload.toGetReceipts
      case Codes.ReceiptsCode              => payload.toReceipts
      case Codes.NewBlockHashesCode        => payload.toNewBlockHashes
      case Codes.GetBlockHeadersCode       => payload.toGetBlockHeaders
      case Codes.BlockHeadersCode          => payload.toBlockHeaders
      case Codes.GetBlockBodiesCode        => payload.toGetBlockBodies
      case Codes.BlockBodiesCode           => payload.toBlockBodies
      case Codes.BlockHashesFromNumberCode => payload.toBlockHashesFromNumber
      case Codes.SignedTransactionsCode    => payload.toSignedTransactions
      case _                               => throw new RuntimeException(s"Unknown message type: $msgCode")
    }
}

object ETH64MessageDecoder extends MessageDecoder {
  import io.iohk.ethereum.network.p2p.messages.ETH64.Status._
  import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.NewBlock._

  def fromBytes(msgCode: Int, payload: Array[Byte]): Message =
    msgCode match {
      case Codes.GetNodeDataCode           => payload.toGetNodeData
      case Codes.NodeDataCode              => payload.toNodeData
      case Codes.GetReceiptsCode           => payload.toGetReceipts
      case Codes.ReceiptsCode              => payload.toReceipts
      case Codes.NewBlockHashesCode        => payload.toNewBlockHashes
      case Codes.GetBlockHeadersCode       => payload.toGetBlockHeaders
      case Codes.BlockHeadersCode          => payload.toBlockHeaders
      case Codes.GetBlockBodiesCode        => payload.toGetBlockBodies
      case Codes.BlockBodiesCode           => payload.toBlockBodies
      case Codes.BlockHashesFromNumberCode => payload.toBlockHashesFromNumber
      case Codes.StatusCode                => payload.toStatus
      case Codes.NewBlockCode              => payload.toNewBlock
      case Codes.SignedTransactionsCode    => payload.toSignedTransactions
      case _                               => throw new RuntimeException(s"Unknown message type: $msgCode")
    }
}

object ETH63MessageDecoder extends MessageDecoder {
  import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.Status._
  import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.NewBlock._

  def fromBytes(msgCode: Int, payload: Array[Byte]): Message =
    msgCode match {
      case Codes.GetNodeDataCode           => payload.toGetNodeData
      case Codes.NodeDataCode              => payload.toNodeData
      case Codes.GetReceiptsCode           => payload.toGetReceipts
      case Codes.ReceiptsCode              => payload.toReceipts
      case Codes.NewBlockHashesCode        => payload.toNewBlockHashes
      case Codes.GetBlockHeadersCode       => payload.toGetBlockHeaders
      case Codes.BlockHeadersCode          => payload.toBlockHeaders
      case Codes.GetBlockBodiesCode        => payload.toGetBlockBodies
      case Codes.BlockBodiesCode           => payload.toBlockBodies
      case Codes.BlockHashesFromNumberCode => payload.toBlockHashesFromNumber
      case Codes.StatusCode                => payload.toStatus
      case Codes.NewBlockCode              => payload.toNewBlock
      case Codes.SignedTransactionsCode    => payload.toSignedTransactions
      case _                               => throw new RuntimeException(s"Unknown message type: $msgCode")
    }
}

// scalastyle:off
object EthereumMessageDecoder {
  type Decoder = (Int, Array[Byte]) => Message
  def ethMessageDecoder(protocolVersion: Capability): MessageDecoder =
    protocolVersion match {
      case Capability.Capabilities.Etc64Capability => ETC64MessageDecoder.fromBytes
      case Capability.Capabilities.Eth63Capability => ETH63MessageDecoder.fromBytes
      case Capability.Capabilities.Eth64Capability => ETH64MessageDecoder.fromBytes
      case _                                       => throw new RuntimeException(s"Unsupported Protocol Version $protocolVersion")
    }
}
