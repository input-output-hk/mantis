package io.iohk.ethereum.network.p2p

import io.iohk.ethereum.network.p2p.messages.{PV61 => pv61, PV62 => pv62, PV63 => pv63}
import io.iohk.ethereum.network.p2p.messages.CommonMessages._
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.rlp

object Message {

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

    case (PV62 | PV63, pv62.NewBlockHashes.code) => rlp.decode(payload)(pv62.NewBlockHashes.rlpEncDec)
    case (PV62 | PV63, pv62.GetBlockHeaders.code) => rlp.decode(payload)(pv62.GetBlockHeaders.rlpEncDec)
    case (PV62 | PV63, pv62.BlockHeaders.code) => rlp.decode(payload)(pv62.BlockHeaders.headersRlpEncDec)
    case (PV62 | PV63, pv62.GetBlockBodies.code) => rlp.decode(payload)(pv62.GetBlockBodies.rlpEncDec)
    case (PV62 | PV63, pv62.BlockBodies.code) => rlp.decode(payload)(pv62.BlockBodies.rlpEncDec)

    case (PV63, t) => handlePV63(t, payload)

    case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
  }

  private def handlePV61(`type`: Int, payload: Array[Byte]): Message = `type` match {
    case pv61.NewBlockHashes.code => rlp.decode(payload)(pv61.NewBlockHashes.rlpEncDec)
    case pv61.BlockHashesFromNumber.code => rlp.decode(payload)(pv61.BlockHashesFromNumber.rlpEncDec)
    case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
  }

  private def handlePV63(`type`: Int, payload: Array[Byte]): Message = `type` match {
    case pv63.GetNodeData.code => rlp.decode(payload)(pv63.GetNodeData.rlpEncDec)
    case pv63.NodeData.code => rlp.decode(payload)(pv63.NodeData.rlpEncDec)
    case pv63.GetReceipts.code => rlp.decode(payload)(pv63.GetReceipts.rlpEncDec)
    case pv63.Receipts.code => rlp.decode(payload)(pv63.Receipts.rlpEncDec)
    case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
  }
}

trait Message {
  def code: Int
}
