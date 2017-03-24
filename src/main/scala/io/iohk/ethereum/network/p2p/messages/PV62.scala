package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.domain.{BlockHeader, SignedTransaction}
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.txRlpEncDec
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._
import org.spongycastle.util.encoders.Hex

import scala.language.implicitConversions

object PV62 {
  object BlockHash {
    implicit val rlpEncDec = new RLPEncoder[BlockHash] with RLPDecoder[BlockHash] {
      override def encode(obj: BlockHash): RLPEncodeable = {
        import obj._
        RLPList(hash, number)
      }

      override def decode(rlp: RLPEncodeable): BlockHash = rlp match {
        case RLPList(hash, number) => BlockHash(hash, number)
        case _ => throw new RuntimeException("Cannot decode BlockHash")
      }
    }
  }

  case class BlockHash(hash: ByteString, number: BigInt) {
    override def toString: String = {
      s"""BlockHash {
         |hash: ${Hex.toHexString(hash.toArray[Byte])}
         |number: $number
         |}""".stripMargin
    }
  }

  object NewBlockHashes {
    implicit val rlpEncDec = new RLPEncoder[NewBlockHashes] with RLPDecoder[NewBlockHashes] {
      override def encode(obj: NewBlockHashes): RLPEncodeable = {
        import obj._
        toRlpList(hashes)
      }

      override def decode(rlp: RLPEncodeable): NewBlockHashes = rlp match {
        case rlpList: RLPList => NewBlockHashes(fromRlpList[BlockHash](rlpList))
        case _ => throw new RuntimeException("Cannot decode NewBlockHashes")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x01
  }

  case class NewBlockHashes(hashes: Seq[BlockHash]) extends Message {
    override def code: Int = NewBlockHashes.code
  }

  object GetBlockHeaders {
    implicit val rlpEncDec = new RLPEncoder[GetBlockHeaders] with RLPDecoder[GetBlockHeaders] {
      override def encode(obj: GetBlockHeaders): RLPEncodeable = {
        import obj._
        block match {
          case Left(blockNumber) => RLPList(blockNumber, maxHeaders, skip, if (reverse) 1 else 0)
          case Right(blockHash) => RLPList(blockHash, maxHeaders, skip, if (reverse) 1 else 0)
        }
      }

      override def decode(rlp: RLPEncodeable): GetBlockHeaders = rlp match {
        case RLPList((block: RLPValue), maxHeaders, skip, reverse) if block.bytes.length < 32 =>
          GetBlockHeaders(Left(block), maxHeaders, skip, (reverse: Int) == 1)

        case RLPList((block: RLPValue), maxHeaders, skip, reverse) =>
          GetBlockHeaders(Right(block), maxHeaders, skip, (reverse: Int) == 1)

        case _ => throw new RuntimeException("Cannot decode GetBlockHeaders")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x03
  }

  object BlockHeaderImplicits {

    implicit val headerRlpEncDec = new RLPEncoder[BlockHeader] with RLPDecoder[BlockHeader] {

      override def encode(obj: BlockHeader): RLPEncodeable = {
        import obj._
        RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
          logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce)
      }

      override def decode(rlp: RLPEncodeable): BlockHeader = rlp match {
        case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
        logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce) =>
          BlockHeader(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce)
      }
    }
  }

  object BlockBody {
    import BlockHeaderImplicits._

    implicit val rlpEncDec = new RLPEncoder[BlockBody] with RLPDecoder[BlockBody] {

      override def encode(obj: BlockBody): RLPEncodeable = {
        import obj._
        RLPList(
          toRlpList(transactionList),
          toRlpList(uncleNodesList))
      }

      override def decode(rlp: RLPEncodeable): BlockBody = rlp match {
        case RLPList((transactions: RLPList), (uncles: RLPList)) =>
          BlockBody(
            fromRlpList[SignedTransaction](transactions),
            fromRlpList[BlockHeader](uncles))
        case _ => throw new RuntimeException("Cannot decode BlockBody")
      }
    }
  }

  case class BlockBody(transactionList: Seq[SignedTransaction], uncleNodesList: Seq[BlockHeader]) {
    override def toString: String =
      s"""BlockBody{
         |transactionList: $transactionList
         |uncleNodesList: $uncleNodesList
         |}
    """.stripMargin
  }

  object BlockBodies {
    implicit val rlpEncDec = new RLPEncoder[BlockBodies] with RLPDecoder[BlockBodies] {
      override def encode(obj: BlockBodies): RLPEncodeable = {
        import obj._
        toRlpList(bodies)
      }

      override def decode(rlp: RLPEncodeable): BlockBodies = rlp match {
        case rlpList: RLPList => BlockBodies(fromRlpList[BlockBody](rlpList))
        case _ => throw new RuntimeException("Cannot decode BlockBodies")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x06
  }

  case class BlockBodies(bodies: Seq[BlockBody]) extends Message {
    val code: Int = BlockBodies.code
  }

  case class GetBlockHeaders(block: Either[BigInt, ByteString], maxHeaders: BigInt, skip: BigInt, reverse: Boolean) extends Message {
    override def code: Int = GetBlockHeaders.code

    override def toString: String = {
      s"""GetBlockHeaders{
         |block: ${block.fold(a => a, b => Hex.toHexString(b.toArray[Byte]))}
         |maxHeaders: $maxHeaders
         |skip: $skip
         |reverse: $reverse
         |}
     """.stripMargin
    }
  }

  object BlockHeaders {

    import BlockHeaderImplicits._

    implicit val headersRlpEncDec = new RLPEncoder[BlockHeaders] with RLPDecoder[BlockHeaders] {
      override def encode(obj: BlockHeaders): RLPEncodeable = {
        import obj._
        toRlpList(headers)
      }

      override def decode(rlp: RLPEncodeable): BlockHeaders = rlp match {
        case rlpList: RLPList => BlockHeaders(fromRlpList[BlockHeader](rlpList))

        case _ => throw new RuntimeException("Cannot decode BlockHeaders")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x04

  }

  case class BlockHeaders(headers: Seq[BlockHeader]) extends Message {
    override def code: Int = BlockHeaders.code
  }

  object GetBlockBodies {
    implicit val rlpEncDec = new RLPEncoder[GetBlockBodies] with RLPDecoder[GetBlockBodies] {
      override def encode(obj: GetBlockBodies): RLPEncodeable = {
        import obj._
        toRlpList(hashes)
      }

      override def decode(rlp: RLPEncodeable): GetBlockBodies = rlp match {
        case rlpList: RLPList => GetBlockBodies(fromRlpList[ByteString](rlpList))

        case _ => throw new RuntimeException("Cannot decode BlockHeaders")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x05
  }

  case class GetBlockBodies(hashes: Seq[ByteString]) extends Message {
    override def code: Int = GetBlockBodies.code

    override def toString: String = {
      s"""GetBlockBodies {
         |hashes: ${hashes.map(h => Hex.toHexString(h.toArray[Byte]))}
         |}
     """.stripMargin
    }
  }
}
