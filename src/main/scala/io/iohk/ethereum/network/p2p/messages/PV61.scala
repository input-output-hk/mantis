package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.{Message, MessageSerializableImplicit}
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._

object PV61 {

  object NewBlockHashes {
    val code: Int = Versions.SubProtocolOffset + 0x01

    implicit class NewBlockHashesEnc(val underlyingMsg: NewBlockHashes)
      extends MessageSerializableImplicit[NewBlockHashes](underlyingMsg) with RLPSerializable {

      override def code: Int = NewBlockHashes.code

      override def toRLPEncodable: RLPEncodeable = RLPList(msg.hashes.map(e => RLPValue(e.toArray[Byte])): _*)
    }

    implicit class NewBlockHashesDec(val bytes: Array[Byte]) extends AnyVal {
      def toNewBlockHashes: NewBlockHashes = rawDecode(bytes) match {
        case rlpList: RLPList => NewBlockHashes(rlpList.items.map(e => ByteString(e: Array[Byte])))
        case _ => throw new RuntimeException("Cannot decode NewBlockHashes")
      }

    }
  }

  case class NewBlockHashes(hashes: Seq[ByteString]) extends Message {
    override def code: Int = NewBlockHashes.code
  }

  object BlockHashesFromNumber {

    val code: Int = Versions.SubProtocolOffset + 0x08

    implicit class BlockHashesFromNumberEnc(val underlyingMsg: BlockHashesFromNumber)
      extends MessageSerializableImplicit[BlockHashesFromNumber](underlyingMsg) with RLPSerializable {

      override def code: Int = BlockHashesFromNumber.code

      override def toRLPEncodable: RLPEncodeable = RLPList(msg.number, msg.maxBlocks)
    }

    implicit class BlockHashesFromNumberDec(val bytes: Array[Byte]) extends AnyVal {
      def toBlockHashesFromNumber: BlockHashesFromNumber = rawDecode(bytes) match {
        case RLPList(number, maxBlocks) => BlockHashesFromNumber(number, maxBlocks)
        case _ => throw new RuntimeException("Cannot decode BlockHashesFromNumber")
      }
    }
  }

  case class BlockHashesFromNumber(number: BigInt, maxBlocks: BigInt) extends Message {
    override def code: Int = BlockHashesFromNumber.code
  }

}
