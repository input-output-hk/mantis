package io.iohk.ethereum.network.p2p

import io.iohk.ethereum.network.p2p.Message.Version
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status._
import io.iohk.ethereum.network.p2p.messages.CommonMessages._
import io.iohk.ethereum.network.p2p.messages.PV61.BlockHashesFromNumber._
import io.iohk.ethereum.network.p2p.messages.PV62.NewBlockHashes._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBodies._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaders._
import io.iohk.ethereum.network.p2p.messages.PV62.GetBlockBodies._
import io.iohk.ethereum.network.p2p.messages.PV62.GetBlockHeaders._
import io.iohk.ethereum.network.p2p.messages.PV63.GetNodeData._
import io.iohk.ethereum.network.p2p.messages.PV63.GetReceipts._
import io.iohk.ethereum.network.p2p.messages.PV63.NodeData._
import io.iohk.ethereum.network.p2p.messages.PV63.Receipts._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello._
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.p2p.messages.{PeerMessagesDecoder, PV61 => pv61, PV62 => pv62, PV63 => pv63}
import io.iohk.ethereum.network.p2p.messages.Versions._

import scala.util.{Failure, Success, Try}

object EtcMessageDecoders {

  object WireProtocolMessageDecoder extends MessageDecoder {

    override def fromBytes(`type`: Version, payload: Array[Byte], protocolVersion: Version): Try[Message] =
      PeerMessagesDecoder.fromBytes(`type`, payload, protocolVersion).orElse{
        if(`type` == Hello.code)
          Success(payload.toHello)
        else
          Failure(new RuntimeException(s"Unknown message type: ${`type`}"))
      }

  }

  object CommonMessagesDecoder extends MessageDecoder {

    override def fromBytes(`type`: Version, payload: Array[Byte], protocolVersion: Version): Try[Message] = Try {
      `type` match {
        case Status.code => payload.toStatus
        case SignedTransactions.code => payload.toSignedTransactions
        case NewBlock.code => payload.toNewBlock
        case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
      }
    }

  }

  object PV61MessageDecoder extends MessageDecoder {

    override def fromBytes(`type`: Int, payload: Array[Byte], protocolVersion: Version): Try[Message] = Try {
      import io.iohk.ethereum.network.p2p.messages.PV61.NewBlockHashes._
      (protocolVersion, `type`) match {
        case (PV61, pv61.NewBlockHashes.code) => payload.toNewBlockHashes
        case (PV61, pv61.BlockHashesFromNumber.code) => payload.toBlockHashesFromNumber
        case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
      }
    }

  }

  object PV62MessageDecoder extends MessageDecoder {

    override def fromBytes(`type`: Int, payload: Array[Byte], protocolVersion: Version): Try[Message] = {
      if(protocolVersion == PV62)
        handlePV62(`type`, payload)
      else
        Failure(throw new RuntimeException(s"Unknown message type: ${`type`}"))
    }

    private[EtcMessageDecoders] def handlePV62(`type`: Int, payload: Array[Byte]): Try[Message] = Try {
      `type` match {
        case pv62.NewBlockHashes.code => payload.toNewBlockHashes
        case pv62.GetBlockHeaders.code => payload.toGetBlockHeaders
        case pv62.BlockHeaders.code => payload.toBlockHeaders
        case pv62.GetBlockBodies.code => payload.toGetBlockBodies
        case pv62.BlockBodies.code => payload.toBlockBodies
        case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
      }
    }

  }

  object PV63MessageDecoder extends MessageDecoder {

    override def fromBytes(`type`: Int, payload: Array[Byte], protocolVersion: Version): Try[Message] = {
      if(protocolVersion == PV63)
        PV62MessageDecoder.handlePV62(`type`, payload).orElse(handlePV63(`type`, payload))
      else
        Failure(throw new RuntimeException(s"Unknown message type: ${`type`}"))
    }

    private[EtcMessageDecoders] def handlePV63(`type`: Int, payload: Array[Byte]): Try[Message] = Try {
      `type` match {
        case pv63.GetNodeData.code => payload.toGetNodeData
        case pv63.NodeData.code => payload.toNodeData
        case pv63.GetReceipts.code => payload.toGetReceipts
        case pv63.Receipts.code => payload.toReceipts
        case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
      }
    }
  }

  object EtcMessageDecoder extends MessageDecoder {

    override def fromBytes(`type`: Version, payload: Array[Byte], protocolVersion: Version): Try[Message] =
      (WireProtocolMessageDecoder orElse
        CommonMessagesDecoder orElse
        PV61MessageDecoder orElse
        PV62MessageDecoder orElse
        PV63MessageDecoder
        ).fromBytes(`type`, payload, protocolVersion)

  }
}
