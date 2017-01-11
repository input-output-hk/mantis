package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._

object PV63 {

  object GetNodeData {
    implicit val rlpEndDec = new RLPEncoder[GetNodeData] with RLPDecoder[GetNodeData] {
      override def encode(obj: GetNodeData): RLPEncodeable = {
        import obj._
        RLPList(hashes.map(e => RLPValue(e.toArray[Byte])): _*)
      }

      override def decode(rlp: RLPEncodeable): GetNodeData = rlp match {
        case rlpList: RLPList => GetNodeData(rlpList.items.map(e => ByteString(e: Array[Byte])))
        case _ => throw new RuntimeException("Cannot decode GetNodeData")
      }
    }

    val code: Int = 0x10 + 0x0d
  }

  case class GetNodeData(hashes: Seq[ByteString]) extends Message {
    override def code: Int = GetNodeData.code
  }

}
