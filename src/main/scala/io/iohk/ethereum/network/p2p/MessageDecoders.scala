package io.iohk.ethereum.network.p2p

import io.iohk.ethereum.network.p2p.Message.Version
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.PV60.SignedTransactions._
import io.iohk.ethereum.network.p2p.messages.PV61.BlockHashesFromNumber._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBodies._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaders._
import io.iohk.ethereum.network.p2p.messages.PV62.GetBlockBodies._
import io.iohk.ethereum.network.p2p.messages.PV62.GetBlockHeaders._
import io.iohk.ethereum.network.p2p.messages.PV62.NewBlockHashes._
import io.iohk.ethereum.network.p2p.messages.PV63.GetNodeData._
import io.iohk.ethereum.network.p2p.messages.PV63.GetReceipts._
import io.iohk.ethereum.network.p2p.messages.PV63.NodeData._
import io.iohk.ethereum.network.p2p.messages.PV63.Receipts._
import io.iohk.ethereum.network.p2p.messages.ProtocolVersions._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Ping._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Pong._
import io.iohk.ethereum.network.p2p.messages.WireProtocol._

object NetworkMessageDecoder extends MessageDecoder {

  override def fromBytes(msgCode: Int, payload: Array[Byte], protocolVersion: Version): Message =
    msgCode match {
      case Disconnect.code => payload.toDisconnect
      case Ping.code => payload.toPing
      case Pong.code => payload.toPong
      case Hello.code => payload.toHello
      case _ => throw new RuntimeException("Unknown message type: " + msgCode)
    }

}

// scalastyle:off
object EthereumMessageDecoder extends MessageDecoder {

  override def fromBytes(msgCode: Int, payload: Array[Byte], protocolVersion: Version): Message = {
    protocolVersion match {
      case PV64 => handlePV64(msgCode, payload)
      case PV63 => handlePV63(msgCode, payload)
      case PV62 => handlePV62(msgCode, payload)
      case PV61 => handlePV61(msgCode, payload)
      case pv => throw new RuntimeException("Unknown protocol version: " + pv)
    }
  }

  private def handlePV60(msgCode: Int, payload: Array[Byte]): Message = {
    msgCode match {
      case Codes.StatusCode =>
        import io.iohk.ethereum.network.p2p.messages.PV60.Status._
        payload.toStatus
      case Codes.NewBlockCode =>
        import io.iohk.ethereum.network.p2p.messages.PV60.NewBlock._
        payload.toNewBlock
      case Codes.SignedTransactionsCode =>
        payload.toSignedTransactions
      case _ =>
        throw new RuntimeException("Unknown message type: " + msgCode)
    }
  }

  private def handlePV61(msgCode: Int, payload: Array[Byte]): Message = {
    msgCode match {
      case Codes.NewBlockHashesCode =>
        import io.iohk.ethereum.network.p2p.messages.PV61.NewBlockHashes._
        payload.toNewBlockHashes
      case Codes.BlockHashesFromNumberCode =>
        payload.toBlockHashesFromNumber
      case _ => handlePV60(msgCode, payload)
    }
  }

  private def handlePV62(msgCode: Int, payload: Array[Byte]): Message = {
    msgCode match {
      case Codes.NewBlockHashesCode => payload.toNewBlockHashes
      case Codes.GetBlockHeadersCode => payload.toGetBlockHeaders
      case Codes.BlockHeadersCode => payload.toBlockHeaders
      case Codes.GetBlockBodiesCode => payload.toGetBlockBodies
      case Codes.BlockBodiesCode => payload.toBlockBodies
      case _ => handlePV60(msgCode, payload)
    }
  }

  private def handlePV63(msgCode: Int, payload: Array[Byte]): Message = {
    msgCode match {
      case Codes.GetNodeDataCode => payload.toGetNodeData
      case Codes.NodeDataCode => payload.toNodeData
      case Codes.GetReceiptsCode => payload.toGetReceipts
      case Codes.ReceiptsCode => payload.toReceipts
      case _ => handlePV62(msgCode, payload)
    }
  }

  private def handlePV64(msgCode: Int, payload: Array[Byte]): Message = {
    msgCode match {
      case Codes.StatusCode =>
        import io.iohk.ethereum.network.p2p.messages.PV64.Status._
        payload.toStatus
      case Codes.NewBlockCode =>
        import io.iohk.ethereum.network.p2p.messages.PV64.NewBlock._
        payload.toNewBlock
      case _ => handlePV63(msgCode, payload)
    }
  }
}
