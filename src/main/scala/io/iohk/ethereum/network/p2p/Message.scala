package io.iohk.ethereum.network.p2p

import io.iohk.ethereum.network.p2p.messages.CommonMessages._
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.p2p.messages.{PV62 => pv62, PV61 => pv61, PV63 => pv63}
import io.iohk.ethereum.rlp

object Message {

  type Version = Int

  val PV61: Version = 61
  val PV62: Version = 62
  val PV63: Version = 63

  val SubProtocolOffset = 0x10

  def decode(`type`: Int, payload: Array[Byte], protocolVersion: Version): Message = (protocolVersion, `type`) match {
    //wire protocol
    case (_, Hello.code) => rlp.decode(payload)(Hello.rlpEndDec)
    case (_, Disconnect.code) => rlp.decode(payload)(Disconnect.rlpEndDec)
    case (_, Ping.code) => rlp.decode(payload)(Ping.rlpEndDec)
    case (_, Pong.code) => rlp.decode(payload)(Pong.rlpEndDec)

    //common
    case (_, Status.code) => rlp.decode(payload)(Status.rlpEndDec)
    case (_, Transactions.code) => rlp.decode(payload)(Transactions.rlpEndDec)

    case (PV61, t) => handlePV61(t, payload)

    case (PV62 | PV63, pv62.NewBlockHashes.code) => rlp.decode(payload)(pv62.NewBlockHashes.rlpEndDec)
    case (PV62 | PV63, pv62.GetBlockHeaders.code) => rlp.decode(payload)(pv62.GetBlockHeaders.rlpEndDec)
    case (PV62 | PV63, pv62.BlockHeaders.code) => rlp.decode(payload)(pv62.BlockHeaders.rlpEndDec)
    case (PV62 | PV63, pv62.GetBlockBodies.code) => rlp.decode(payload)(pv62.GetBlockBodies.rlpEndDec)
    case (PV62 | PV63, pv62.BlockBodies.code) => rlp.decode(payload)(pv62.BlockBodies.rlpEndDec)

    case (PV63, t) => handlePV63(t, payload)

    case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
  }

  private def handlePV61(`type`: Int, payload: Array[Byte]): Message = `type` match {
    case pv61.NewBlockHashes.code => rlp.decode(payload)(pv61.NewBlockHashes.rlpEndDec)
    case pv61.BlockHashesFromNumber.code => rlp.decode(payload)(pv61.BlockHashesFromNumber.rlpEndDec)
  }

  private def handlePV63(`type`: Int, payload: Array[Byte]): Message = `type` match {
    case pv63.GetNodeData.code => rlp.decode(payload)(pv63.GetNodeData.rlpEndDec)
    case pv63.NodeData.code => rlp.decode(payload)(pv63.NodeData.rlpEndDec)
    case pv63.GetReceipts.code => rlp.decode(payload)(pv63.GetReceipts.rlpEndDec)
    case pv63.Receipts.code => rlp.decode(payload)(pv63.Receipts.rlpEndDec)
  }
}

trait Message {
  def code: Int
}
