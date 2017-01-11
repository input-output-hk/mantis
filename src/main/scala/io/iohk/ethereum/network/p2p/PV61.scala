package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.rlp.{RLPDecoder, RLPEncodeable, RLPEncoder, RLPList}
import org.spongycastle.util.encoders.Hex

package object PV61 {
  object Status {
    implicit val rlpEndDec = new RLPEncoder[Status] with RLPDecoder[Status] {
      override def encode(obj: Status): RLPEncodeable = {
        import obj._
        RLPList(protocolVersion, networkId, totalDifficulty, bestHash.toArray[Byte], genesisHash.toArray[Byte])
      }

      override def decode(rlp: RLPEncodeable): Status = rlp match {
        case RLPList(protocolVersion, networkId, totalDifficulty, bestHash, genesisHash) =>
          Status(protocolVersion, networkId, totalDifficulty, ByteString(bestHash: Array[Byte]), ByteString(genesisHash: Array[Byte]))
        case _ => throw new RuntimeException("Cannot decode Status")
      }
    }

    val code: Int = 0x10 + 0x00
  }

  case class Status(protocolVersion: Int, networkId: Int, totalDifficulty: BigInt, bestHash: ByteString, genesisHash: ByteString) extends Message {
    override def code: Int = Status.code

    override def toString: String = {
      s"""Status {
         |protocolVersion: $protocolVersion
         |networkId: $networkId
         |totalDifficulty: $totalDifficulty
         |bestHash: ${Hex.toHexString(bestHash.toArray[Byte])}
         |genesisHash: ${Hex.toHexString(genesisHash.toArray[Byte])}
         |}""".stripMargin
    }
  }
}
