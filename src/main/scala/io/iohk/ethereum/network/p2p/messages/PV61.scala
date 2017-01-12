package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._

object PV61 {

  object NewBlockHashes {
    implicit val rlpEndDec = new RLPEncoder[NewBlockHashes] with RLPDecoder[NewBlockHashes] {
      override def encode(obj: NewBlockHashes): RLPEncodeable = {
        import obj._
        RLPList(hashes.map(e => RLPValue(e.toArray[Byte])): _*)
      }

      override def decode(rlp: RLPEncodeable): NewBlockHashes = rlp match {
        case rlpList: RLPList => NewBlockHashes(rlpList.items.map(e => ByteString(e: Array[Byte])))
        case _ => throw new RuntimeException("Cannot decode NewBlockHashes")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x01
  }

  case class NewBlockHashes(hashes: Seq[ByteString]) extends Message {
    override def code: Int = NewBlockHashes.code
  }

}
