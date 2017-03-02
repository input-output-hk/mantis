package io.iohk.ethereum.network.protocol

import WireProtocol.{Disconnect, Hello, Ping, Pong}
import io.iohk.ethereum.network.protocol
import io.iohk.ethereum.network.protocol.CommonMessages.{NewBlock, SignedTransactions, Status}
import io.iohk.ethereum.network.protocol.PV61.BlockHashesFromNumber
import io.iohk.ethereum.network.protocol.PV62._
import io.iohk.ethereum.network.protocol.PV63.{GetNodeData, GetReceipts, NodeData, Receipts}
import io.iohk.ethereum.network.rlpx.Message
import io.iohk.ethereum.rlp

/**
  * Created by alan on 3/1/17.
  */
object MessageDecoder {

  type Version = Int

  val PV61: Version = 61
  val PV62: Version = 62
  val PV63: Version = 63

  val SubProtocolOffset = 0x10

  def decode(`type`: Int, payload: Array[Byte], protocolVersion: Version): Message = (protocolVersion, `type`) match {
    //wire protocol
    case (_, Hello.code) => rlp.decode(payload)(Hello.rlpEncDec)
    case (_, Disconnect.code) => rlp.decode(payload)(Disconnect.rlpEncDec)
    case (_, Ping.code) => rlp.decode(payload)(Ping.rlpEncDec)
    case (_, Pong.code) => rlp.decode(payload)(Pong.rlpEncDec)

    //common
    case (_, Status.code) => rlp.decode(payload)(Status.rlpEncDec)
    case (_, SignedTransactions.code) => rlp.decode(payload)(SignedTransactions.txsRlpEncDec)
    case (_, NewBlock.code) => rlp.decode(payload)(NewBlock.rlpEncDec)

    case (PV61, t) => handlePV61(t, payload)

    case (PV62 | PV63, NewBlockHashes.code) => rlp.decode(payload)(protocol.PV62.NewBlockHashes.rlpEncDec)
    case (PV62 | PV63, GetBlockHeaders.code) => rlp.decode(payload)(protocol.PV62.GetBlockHeaders.rlpEncDec)
    case (PV62 | PV63, BlockHeaders.code) => rlp.decode(payload)(protocol.PV62.BlockHeaders.headersRlpEncDec)
    case (PV62 | PV63, GetBlockBodies.code) => rlp.decode(payload)(protocol.PV62.GetBlockBodies.rlpEncDec)
    case (PV62 | PV63, BlockBodies.code) => rlp.decode(payload)(protocol.PV62.BlockBodies.rlpEncDec)

    case (PV63, t) => handlePV63(t, payload)

    case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
  }

  private def handlePV61(`type`: Int, payload: Array[Byte]): Message = `type` match {
    case protocol.PV61.NewBlockHashes.code => rlp.decode(payload)(protocol.PV61.NewBlockHashes.rlpEncDec)
    case BlockHashesFromNumber.code => rlp.decode(payload)(protocol.PV61.BlockHashesFromNumber.rlpEncDec)
    case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
  }

  private def handlePV63(`type`: Int, payload: Array[Byte]): Message = `type` match {
    case GetNodeData.code => rlp.decode(payload)(protocol.PV63.GetNodeData.rlpEncDec)
    case NodeData.code => rlp.decode(payload)(protocol.PV63.NodeData.rlpEncDec)
    case GetReceipts.code => rlp.decode(payload)(protocol.PV63.GetReceipts.rlpEncDec)
    case Receipts.code => rlp.decode(payload)(protocol.PV63.Receipts.rlpEncDec)
    case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
  }
}

