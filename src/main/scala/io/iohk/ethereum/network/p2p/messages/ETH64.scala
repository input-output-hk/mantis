package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.forkid.ForkId
import io.iohk.ethereum.forkid.ForkId._
import io.iohk.ethereum.mpt.{MptNode, MptTraversals}
import io.iohk.ethereum.network.p2p.{Message, MessageSerializableImplicit}
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._
import org.bouncycastle.util.encoders.Hex

object ETH64 {

  case class Status(
      protocolVersion: Int,
      networkId: Int,
      totalDifficulty: BigInt,
      bestHash: ByteString,
      genesisHash: ByteString,
      forkId: ForkId
  ) extends Message {

    override def toString: String =
      s"Status { " +
        s"code: $code, " +
        s"protocolVersion: $protocolVersion, " +
        s"networkId: $networkId, " +
        s"totalDifficulty: $totalDifficulty, " +
        s"bestHash: ${Hex.toHexString(bestHash.toArray[Byte])}, " +
        s"genesisHash: ${Hex.toHexString(genesisHash.toArray[Byte])}," +
        s"forkId: $forkId," +
        s"}"

    override def toShortString: String = toString
    override def code: Int = Codes.StatusCode
  }

  object Status {
    implicit class StatusEnc(val underlyingMsg: Status)
        extends MessageSerializableImplicit[Status](underlyingMsg)
        with RLPSerializable {
      override def code: Int = Codes.StatusCode

      override def toRLPEncodable: RLPEncodeable = {
        import msg._
        RLPList(protocolVersion, networkId, totalDifficulty, bestHash, genesisHash, forkId.toRLPEncodable)
      }
    }

    implicit class StatusDec(val bytes: Array[Byte]) extends AnyVal {
      def toStatus: Status = rawDecode(bytes) match {
        case RLPList(
              protocolVersion,
              networkId,
              totalDifficulty,
              bestHash,
              genesisHash,
              forkId
            ) =>
          Status(
            protocolVersion,
            networkId,
            totalDifficulty,
            bestHash,
            genesisHash,
            decode[ForkId](forkId)
          )

        case _ => throw new RuntimeException("Cannot decode Status")
      }
    }
  }
}
