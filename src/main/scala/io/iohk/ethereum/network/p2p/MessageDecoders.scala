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
import scala.util.Try
import MessageDecoder._

object NetworkMessageDecoder extends MessageDecoder {

  override def fromBytes(msgCode: Int, payload: Array[Byte]): Either[DecodingError, Message] =
    msgCode match {
      case Disconnect.code => Try(payload.toDisconnect).toEither
      case Ping.code       => Try(payload.toPing).toEither
      case Pong.code       => Try(payload.toPong).toEither
      case Hello.code      => Try(payload.toHello).toEither
      case _               => Left(new RuntimeException(s"Unknown network message type: $msgCode"))
    }

}

object ETC64MessageDecoder extends MessageDecoder {
  import io.iohk.ethereum.network.p2p.messages.ETC64.Status._
  import io.iohk.ethereum.network.p2p.messages.ETC64.NewBlock._

  def fromBytes(msgCode: Int, payload: Array[Byte]): Either[DecodingError, Message] =
    msgCode match {
      case Codes.StatusCode                => Try(payload.toStatus).toEither
      case Codes.NewBlockCode              => Try(payload.toNewBlock).toEither
      case Codes.GetNodeDataCode           => Try(payload.toGetNodeData).toEither
      case Codes.NodeDataCode              => Try(payload.toNodeData).toEither
      case Codes.GetReceiptsCode           => Try(payload.toGetReceipts).toEither
      case Codes.ReceiptsCode              => Try(payload.toReceipts).toEither
      case Codes.NewBlockHashesCode        => Try(payload.toNewBlockHashes).toEither
      case Codes.GetBlockHeadersCode       => Try(payload.toGetBlockHeaders).toEither
      case Codes.BlockHeadersCode          => Try(payload.toBlockHeaders).toEither
      case Codes.GetBlockBodiesCode        => Try(payload.toGetBlockBodies).toEither
      case Codes.BlockBodiesCode           => Try(payload.toBlockBodies).toEither
      case Codes.BlockHashesFromNumberCode => Try(payload.toBlockHashesFromNumber).toEither
      case Codes.SignedTransactionsCode    => Try(payload.toSignedTransactions).toEither
      case _                               => Left(new RuntimeException(s"Unknown etc/64 message type: $msgCode"))
    }
}

object ETH64MessageDecoder extends MessageDecoder {
  import io.iohk.ethereum.network.p2p.messages.ETH64.Status._
  import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.NewBlock._

  def fromBytes(msgCode: Int, payload: Array[Byte]): Either[DecodingError, Message] =
    msgCode match {
      case Codes.GetNodeDataCode           => Try(payload.toGetNodeData).toEither
      case Codes.NodeDataCode              => Try(payload.toNodeData).toEither
      case Codes.GetReceiptsCode           => Try(payload.toGetReceipts).toEither
      case Codes.ReceiptsCode              => Try(payload.toReceipts).toEither
      case Codes.NewBlockHashesCode        => Try(payload.toNewBlockHashes).toEither
      case Codes.GetBlockHeadersCode       => Try(payload.toGetBlockHeaders).toEither
      case Codes.BlockHeadersCode          => Try(payload.toBlockHeaders).toEither
      case Codes.GetBlockBodiesCode        => Try(payload.toGetBlockBodies).toEither
      case Codes.BlockBodiesCode           => Try(payload.toBlockBodies).toEither
      case Codes.BlockHashesFromNumberCode => Try(payload.toBlockHashesFromNumber).toEither
      case Codes.StatusCode                => Try(payload.toStatus).toEither
      case Codes.NewBlockCode              => Try(payload.toNewBlock).toEither
      case Codes.SignedTransactionsCode    => Try(payload.toSignedTransactions).toEither
      case _                               => Left(new RuntimeException(s"Unknown eth/64 message type: $msgCode"))
    }
}

object ETH63MessageDecoder extends MessageDecoder {
  import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.Status._
  import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.NewBlock._

  def fromBytes(msgCode: Int, payload: Array[Byte]): Either[DecodingError, Message] =
    msgCode match {
      case Codes.GetNodeDataCode           => Try(payload.toGetNodeData).toEither
      case Codes.NodeDataCode              => Try(payload.toNodeData).toEither
      case Codes.GetReceiptsCode           => Try(payload.toGetReceipts).toEither
      case Codes.ReceiptsCode              => Try(payload.toReceipts).toEither
      case Codes.NewBlockHashesCode        => Try(payload.toNewBlockHashes).toEither
      case Codes.GetBlockHeadersCode       => Try(payload.toGetBlockHeaders).toEither
      case Codes.BlockHeadersCode          => Try(payload.toBlockHeaders).toEither
      case Codes.GetBlockBodiesCode        => Try(payload.toGetBlockBodies).toEither
      case Codes.BlockBodiesCode           => Try(payload.toBlockBodies).toEither
      case Codes.BlockHashesFromNumberCode => Try(payload.toBlockHashesFromNumber).toEither
      case Codes.StatusCode                => Try(payload.toStatus).toEither
      case Codes.NewBlockCode              => Try(payload.toNewBlock).toEither
      case Codes.SignedTransactionsCode    => Try(payload.toSignedTransactions).toEither
      case _                               => Left(new RuntimeException(s"Unknown eth/63 message type: $msgCode"))
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
