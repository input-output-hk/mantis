package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.domain.{BlockHeader, BlockBody}
import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.domain.BlockBody._
import io.iohk.ethereum.network.p2p.{Message, MessageSerializableImplicit}
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{RLPList, _}
import org.bouncycastle.util.encoders.Hex

object PV62 {
  object BlockHash {

    implicit class BlockHashEnc(blockHash: BlockHash) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = RLPList(blockHash.hash, blockHash.number)
    }

    implicit class BlockHashDec(val bytes: Array[Byte]) extends AnyVal {
      def toBlockHash: BlockHash = BlockHashRLPEncodableDec(bytes).toBlockHash
    }

    implicit class BlockHashRLPEncodableDec(val rlpEncodeable: RLPEncodeable) extends AnyVal {
      def toBlockHash: BlockHash = rlpEncodeable match {
        case RLPList(hash, number) => BlockHash(hash, number)
        case _ => throw new RuntimeException("Cannot decode BlockHash")
      }
    }
  }

  case class BlockHash(hash: ByteString, number: BigInt) {
    override def toString: String =
      s"BlockHash { " +
        s"hash: ${Hex.toHexString(hash.toArray[Byte])} " +
        s"number: $number " +
        s"}"
  }

  object NewBlockHashes {

    val code: Int = Versions.SubProtocolOffset + 0x01

    implicit class NewBlockHashesEnc(val underlyingMsg: NewBlockHashes)
        extends MessageSerializableImplicit[NewBlockHashes](underlyingMsg)
        with RLPSerializable {

      import BlockHash._

      override def code: Int = NewBlockHashes.code

      override def toRLPEncodable: RLPEncodeable = RLPList(msg.hashes.map(_.toRLPEncodable): _*)
    }

    implicit class NewBlockHashesDec(val bytes: Array[Byte]) extends AnyVal {
      import BlockHash._
      def toNewBlockHashes: NewBlockHashes = rawDecode(bytes) match {
        case rlpList: RLPList => NewBlockHashes(rlpList.items.map(_.toBlockHash))
        case _ => throw new RuntimeException("Cannot decode NewBlockHashes")
      }
    }
  }

  case class NewBlockHashes(hashes: Seq[BlockHash]) extends Message {
    override def code: Int = NewBlockHashes.code
  }

  object GetBlockHeaders {
    val code: Int = Versions.SubProtocolOffset + 0x03

    implicit class GetBlockHeadersEnc(val underlyingMsg: GetBlockHeaders)
        extends MessageSerializableImplicit[GetBlockHeaders](underlyingMsg)
        with RLPSerializable {

      override def code: Int = GetBlockHeaders.code

      override def toRLPEncodable: RLPEncodeable = {
        import msg._
        block match {
          case Left(blockNumber) => RLPList(blockNumber, maxHeaders, skip, if (reverse) 1 else 0)
          case Right(blockHash) => RLPList(blockHash, maxHeaders, skip, if (reverse) 1 else 0)
        }
      }
    }

    implicit class GetBlockHeadersDec(val bytes: Array[Byte]) extends AnyVal {
      def toGetBlockHeaders: GetBlockHeaders = rawDecode(bytes) match {
        case RLPList((block: RLPValue), maxHeaders, skip, reverse) if block.bytes.length < 32 =>
          GetBlockHeaders(Left(block), maxHeaders, skip, (reverse: Int) == 1)

        case RLPList((block: RLPValue), maxHeaders, skip, reverse) =>
          GetBlockHeaders(Right(block), maxHeaders, skip, (reverse: Int) == 1)

        case _ => throw new RuntimeException("Cannot decode GetBlockHeaders")
      }
    }
  }

  case class GetBlockHeaders(block: Either[BigInt, ByteString], maxHeaders: BigInt, skip: BigInt, reverse: Boolean)
      extends Message {
    override def code: Int = GetBlockHeaders.code

    override def toString: String =
      s"GetBlockHeaders{ " +
        s"block: ${block.fold(a => a, b => Hex.toHexString(b.toArray[Byte]))} " +
        s"maxHeaders: $maxHeaders " +
        s"skip: $skip " +
        s"reverse: $reverse " +
        s"}"
  }

  object BlockBodies {

    val code: Int = Versions.SubProtocolOffset + 0x06

    implicit class BlockBodiesEnc(val underlyingMsg: BlockBodies)
        extends MessageSerializableImplicit[BlockBodies](underlyingMsg)
        with RLPSerializable {
      override def code: Int = BlockBodies.code

      override def toRLPEncodable: RLPEncodeable = RLPList(msg.bodies.map(_.toRLPEncodable): _*)
    }

    implicit class BlockBodiesDec(val bytes: Array[Byte]) extends AnyVal {
      def toBlockBodies: BlockBodies = rawDecode(bytes) match {
        case rlpList: RLPList => BlockBodies(rlpList.items.map(_.toBlockBody))
        case _ => throw new RuntimeException("Cannot decode BlockBodies")
      }
    }
  }

  case class BlockBodies(bodies: Seq[BlockBody]) extends Message {
    val code: Int = BlockBodies.code
  }

  object BlockHeaders {

    val code: Int = Versions.SubProtocolOffset + 0x04

    implicit class BlockHeadersEnc(val underlyingMsg: BlockHeaders)
        extends MessageSerializableImplicit[BlockHeaders](underlyingMsg)
        with RLPSerializable {

      override def code: Int = BlockHeaders.code

      override def toRLPEncodable: RLPEncodeable = RLPList(msg.headers.map(_.toRLPEncodable): _*)
    }

    implicit class BlockHeadersDec(val bytes: Array[Byte]) extends AnyVal {

      def toBlockHeaders: BlockHeaders = rawDecode(bytes) match {
        case rlpList: RLPList => BlockHeaders(rlpList.items.map(_.toBlockHeader))

        case _ => throw new RuntimeException("Cannot decode BlockHeaders")
      }
    }

  }

  case class BlockHeaders(headers: Seq[BlockHeader]) extends Message {
    override def code: Int = BlockHeaders.code
  }

  object GetBlockBodies {

    val code: Int = Versions.SubProtocolOffset + 0x05

    implicit class GetBlockBodiesEnc(val underlyingMsg: GetBlockBodies)
        extends MessageSerializableImplicit[GetBlockBodies](underlyingMsg)
        with RLPSerializable {

      override def code: Int = GetBlockBodies.code

      override def toRLPEncodable: RLPEncodeable = toRlpList(msg.hashes)
    }

    implicit class GetBlockBodiesDec(val bytes: Array[Byte]) extends AnyVal {
      def toGetBlockBodies: GetBlockBodies = rawDecode(bytes) match {
        case rlpList: RLPList => GetBlockBodies(fromRlpList[ByteString](rlpList))

        case _ => throw new RuntimeException("Cannot decode BlockHeaders")
      }
    }
  }

  case class GetBlockBodies(hashes: Seq[ByteString]) extends Message {
    override def code: Int = GetBlockBodies.code

    override def toString: String =
      s"GetBlockBodies { " +
        s"hashes: ${hashes.map(h => Hex.toHexString(h.toArray[Byte]))} " +
        s"}"
  }
}
