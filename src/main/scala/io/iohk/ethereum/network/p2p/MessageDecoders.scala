package io.iohk.ethereum.network.p2p

import io.iohk.ethereum.network.p2p.messages.{Capability, Codes}
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.SignedTransactions._
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
import io.iohk.ethereum.network.p2p.messages.ProtocolVersions._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Ping._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Pong._
import io.iohk.ethereum.network.p2p.messages.WireProtocol._

object NetworkMessageDecoder extends MessageDecoder {

  override def fromBytes(msgCode: Int, payload: Array[Byte], protocolVersion: Capability): Message =
    msgCode match {
      case Disconnect.code => payload.toDisconnect
      case Ping.code => payload.toPing
      case Pong.code => payload.toPong
      case Hello.code => payload.toHello
      case _ => throw new RuntimeException(s"Unknown message type: ${msgCode}")
    }

}

// scalastyle:off
object EthereumMessageDecoder extends MessageDecoder {

  override def fromBytes(msgCode: Int, payload: Array[Byte], protocolVersion: Capability): Message = {
    protocolVersion match {
      case ETC64 => handleETC64(msgCode, payload)
      case ETH63 => handleETH63(msgCode, payload)
      case ETH62 => handleETH62(msgCode, payload)
      case ETH61 => handleETH61(msgCode, payload)
      case pv => throw new RuntimeException("Unknown protocol version: " + pv)
    }
  }

  private def handleCommonMessages(msgCode: Int, payload: Array[Byte]): Message = {
    msgCode match {
      case Codes.StatusCode =>
        import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.Status._
        payload.toStatus
      case Codes.NewBlockCode =>
        import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.NewBlock._
        payload.toNewBlock
      case Codes.SignedTransactionsCode =>
        payload.toSignedTransactions
      case _ =>
        throw new RuntimeException("Unknown message type: " + msgCode)
    }
  }

  private def handleETH61(msgCode: Int, payload: Array[Byte]): Message = {
    msgCode match {
      case Codes.NewBlockHashesCode =>
        import io.iohk.ethereum.network.p2p.messages.ETH61.NewBlockHashes._
        payload.toNewBlockHashes
      case Codes.BlockHashesFromNumberCode =>
        payload.toBlockHashesFromNumber
      case _ => handleCommonMessages(msgCode, payload)
    }
  }

  private def handleETH62(msgCode: Int, payload: Array[Byte]): Message = {
    msgCode match {
      case Codes.NewBlockHashesCode => payload.toNewBlockHashes
      case Codes.GetBlockHeadersCode => payload.toGetBlockHeaders
      case Codes.BlockHeadersCode => payload.toBlockHeaders
      case Codes.GetBlockBodiesCode => payload.toGetBlockBodies
      case Codes.BlockBodiesCode => payload.toBlockBodies
      case _ => handleCommonMessages(msgCode, payload)
    }
  }

  private def handleETH63(msgCode: Int, payload: Array[Byte]): Message = {
    msgCode match {
      case Codes.GetNodeDataCode => payload.toGetNodeData
      case Codes.NodeDataCode => payload.toNodeData
      case Codes.GetReceiptsCode => payload.toGetReceipts
      case Codes.ReceiptsCode => payload.toReceipts
      case _ => handleETH62(msgCode, payload)
    }
  }

  private def handleETC64(msgCode: Int, payload: Array[Byte]): Message = {
    msgCode match {
      case Codes.StatusCode =>
        import io.iohk.ethereum.network.p2p.messages.ETC64.Status._
        payload.toStatus
      case Codes.NewBlockCode =>
        import io.iohk.ethereum.network.p2p.messages.ETC64.NewBlock._
        payload.toNewBlock
      case _ => handleETH63(msgCode, payload)
    }
  }
}
