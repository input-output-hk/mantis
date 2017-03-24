package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._

object PV61 {

  object NewBlockHashes {
    implicit val rlpEncDec = new RLPEncoder[NewBlockHashes] with RLPDecoder[NewBlockHashes] {
      override def encode(obj: NewBlockHashes): RLPEncodeable = {
        import obj._
        toRlpList(hashes)
      }

      override def decode(rlp: RLPEncodeable): NewBlockHashes = rlp match {
        case rlpList: RLPList => NewBlockHashes(fromRlpList[ByteString](rlpList))
        case _ => throw new RuntimeException("Cannot decode NewBlockHashes")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x01
  }

  case class NewBlockHashes(hashes: Seq[ByteString]) extends Message {
    override def code: Int = NewBlockHashes.code
  }

  object BlockHashesFromNumber {

    implicit val rlpEncDec = new RLPEncoder[BlockHashesFromNumber] with RLPDecoder[BlockHashesFromNumber] {
      override def encode(obj: BlockHashesFromNumber): RLPEncodeable = {
        import obj._
        RLPList(number, maxBlocks)
      }

      override def decode(rlp: RLPEncodeable): BlockHashesFromNumber = rlp match {
        case RLPList(number, maxBlocks) => BlockHashesFromNumber(number, maxBlocks)
        case _ => throw new RuntimeException("Cannot decode BlockHashesFromNumber")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x08
  }

  case class BlockHashesFromNumber(number: BigInt, maxBlocks: BigInt) extends Message {
    override def code: Int = BlockHashesFromNumber.code
  }

}
