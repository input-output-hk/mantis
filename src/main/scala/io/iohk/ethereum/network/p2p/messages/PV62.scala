package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.crypto.sha3
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._
import org.spongycastle.util.encoders.Hex

object PV62 {
  object NewBlockHashes {
    implicit val rlpEndDec = new RLPEncoder[NewBlockHashes] with RLPDecoder[NewBlockHashes] {
      override def encode(obj: NewBlockHashes): RLPEncodeable = {
        import obj._
        RLPList(hashes.map(BlockHash.rlpEndDec.encode): _*)
      }

      override def decode(rlp: RLPEncodeable): NewBlockHashes = rlp match {
        case rlpList: RLPList => NewBlockHashes(rlpList.items.map(BlockHash.rlpEndDec.decode))
        case _ => throw new RuntimeException("Cannot decode NewBlockHashes")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x01
  }

  case class NewBlockHashes(hashes: Seq[BlockHash]) extends Message {
    override def code: Int = NewBlockHashes.code
  }

  object GetBlockHeaders {
    implicit val rlpEndDec = new RLPEncoder[GetBlockHeaders] with RLPDecoder[GetBlockHeaders] {
      override def encode(obj: GetBlockHeaders): RLPEncodeable = {
        import obj._
        block match {
          case Left(blockNumber) => RLPList(blockNumber, maxHeaders, skip, reverse)
          case Right(blockHash) => RLPList(blockHash.toArray[Byte], maxHeaders, skip, reverse)
        }
      }

      override def decode(rlp: RLPEncodeable): GetBlockHeaders = rlp match {
        case RLPList((block: RLPValue), maxHeaders, skip, reverse) if block.bytes.length < 32 =>
          GetBlockHeaders(Left(block), maxHeaders, skip, reverse)

        case RLPList((block: RLPValue), maxHeaders, skip, reverse) =>
          GetBlockHeaders(Right(ByteString(block: Array[Byte])), maxHeaders, skip, reverse)

        case _ => throw new RuntimeException("Cannot decode GetBlockHeaders")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x03
  }

  object BlockBodies {
    implicit val rlpEndDec = new RLPEncoder[BlockBodies] with RLPDecoder[BlockBodies] {
      override def encode(obj: BlockBodies): RLPEncodeable = {
        import obj._
        RLPList(bodies.map(BlockBody.rlpEndDec.encode): _*)
      }

      override def decode(rlp: RLPEncodeable): BlockBodies = rlp match {
        case rlpList: RLPList => BlockBodies(rlpList.items.map(BlockBody.rlpEndDec.decode))
        case _ => throw new RuntimeException("Cannot decode BlockBodies")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x06
  }

  case class BlockBodies(bodies: Seq[BlockBody]) extends Message {
    val code: Int = BlockBodies.code
  }

  object BlockBody {
    implicit val rlpEndDec = new RLPEncoder[BlockBody] with RLPDecoder[BlockBody] {
      override def encode(obj: BlockBody): RLPEncodeable = {
        import obj._
        RLPList(
          RLPList(transactionList.map(CommonMessages.Transaction.rlpEndDec.encode): _*),
          RLPList(uncleNodesList.map(BlockHeader.rlpEndDec.encode): _*))
      }

      override def decode(rlp: RLPEncodeable): BlockBody = rlp match {
        case RLPList((transactions: RLPList), (uncles: RLPList)) =>
          BlockBody(
            transactions.items.map(Transaction.rlpEndDec.decode),
            uncles.items.map(BlockHeader.rlpEndDec.decode))
        case _ => throw new RuntimeException("Cannot decode BlockBody")
      }
    }
  }

  case class BlockBody(transactionList: Seq[Transaction], uncleNodesList: Seq[BlockHeader]) {
    override def toString: String =
      s"""BlockBody{
         |transactionList: $transactionList
         |uncleNodesList: $uncleNodesList
         |}
    """.stripMargin
  }






  object BlockHash {
    implicit val rlpEndDec = new RLPEncoder[BlockHash] with RLPDecoder[BlockHash] {
      override def encode(obj: BlockHash): RLPEncodeable = {
        import obj._
        RLPList(hash.toArray[Byte], number)
      }

      override def decode(rlp: RLPEncodeable): BlockHash = rlp match {
        case RLPList(hash, number) => BlockHash(ByteString(hash: Array[Byte]), number)
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



  case class GetBlockHeaders(block: Either[BigInt, ByteString], maxHeaders: BigInt, skip: BigInt, reverse: Int) extends Message {
    override def code: Int = GetBlockHeaders.code

    override def toString: String = {
      s"""GetBlockHeaders{
         |block: ${block.fold(a => a, b => Hex.toHexString(b.toArray[Byte]))}
         |maxHeaders: $maxHeaders
         |skip: $skip
         |reverse: ${reverse == 1}
         |}
     """.stripMargin
    }
  }

  object BlockHeaders {
    implicit val rlpEndDec = new RLPEncoder[BlockHeaders] with RLPDecoder[BlockHeaders] {
      override def encode(obj: BlockHeaders): RLPEncodeable = {
        import obj._
        RLPList(headers.map(BlockHeader.rlpEndDec.encode): _*)
      }

      override def decode(rlp: RLPEncodeable): BlockHeaders = rlp match {
        case rlpList: RLPList => BlockHeaders(rlpList.items.map(BlockHeader.rlpEndDec.decode))

        case _ => throw new RuntimeException("Cannot decode BlockHeaders")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x04
  }

  case class BlockHeaders(headers: Seq[BlockHeader]) extends Message {
    override def code: Int = BlockHeaders.code
  }

  object BlockHeader {
    implicit val rlpEndDec = new RLPEncoder[BlockHeader] with RLPDecoder[BlockHeader] {
      override def encode(obj: BlockHeader): RLPEncodeable = {
        import obj._
        RLPList(
          parentHash.toArray[Byte],
          ommersHash.toArray[Byte],
          beneficiary.toArray[Byte],
          stateRoot.toArray[Byte],
          transactionsRoot.toArray[Byte],
          receiptsRoot.toArray[Byte],
          logsBloom.toArray[Byte],
          difficulty,
          number,
          gasLimit,
          gasUsed,
          unixTimestamp,
          extraData.toArray[Byte],
          mixHash.toArray[Byte],
          nonce.toArray[Byte])
      }

      override def decode(rlp: RLPEncodeable): BlockHeader = rlp match {
        case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot, logsBloom,
        difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce) =>
          BlockHeader(ByteString(parentHash: Array[Byte]),
            ByteString(ommersHash: Array[Byte]),
            ByteString(beneficiary: Array[Byte]),
            ByteString(stateRoot: Array[Byte]),
            ByteString(transactionsRoot: Array[Byte]),
            ByteString(receiptsRoot: Array[Byte]),
            ByteString(logsBloom: Array[Byte]),
            difficulty,
            number,
            gasLimit,
            gasUsed,
            unixTimestamp,
            ByteString(extraData: Array[Byte]),
            ByteString(mixHash: Array[Byte]),
            ByteString(nonce: Array[Byte]))

        case _ => throw new RuntimeException("Cannot decode BlockHeaders")
      }
    }
  }

  case class BlockHeader(
    parentHash: ByteString,
    ommersHash: ByteString,
    beneficiary: ByteString,
    stateRoot: ByteString,
    transactionsRoot: ByteString,
    receiptsRoot: ByteString,
    logsBloom: ByteString,
    difficulty: BigInt,
    number: BigInt,
    gasLimit: BigInt,
    gasUsed: BigInt,
    unixTimestamp: Long,
    extraData: ByteString,
    mixHash: ByteString,
    nonce: ByteString) {

    lazy val hash: Array[Byte] = sha3(encode[BlockHeader](this))

    override def toString: String = {
      s"""BlockHeader {
         |parentHash: ${Hex.toHexString(parentHash.toArray[Byte])}
         |ommersHash: ${Hex.toHexString(ommersHash.toArray[Byte])}
         |beneficiary: ${Hex.toHexString(beneficiary.toArray[Byte])}
         |stateRoot: ${Hex.toHexString(stateRoot.toArray[Byte])}
         |transactionsRoot: ${Hex.toHexString(transactionsRoot.toArray[Byte])}
         |receiptsRoot: ${Hex.toHexString(receiptsRoot.toArray[Byte])}
         |logsBloom: ${Hex.toHexString(logsBloom.toArray[Byte])}
         |difficulty: $difficulty,
         |number: $number,
         |gasLimit: $gasLimit,
         |gasUsed: $gasUsed,
         |unixTimestamp: $unixTimestamp,
         |extraData: ${Hex.toHexString(extraData.toArray[Byte])}
         |mixHash: ${Hex.toHexString(mixHash.toArray[Byte])}
         |nonce: ${Hex.toHexString(nonce.toArray[Byte])}
         |}""".stripMargin
    }
  }

  object GetBlockBodies {
    implicit val rlpEndDec = new RLPEncoder[GetBlockBodies] with RLPDecoder[GetBlockBodies] {
      override def encode(obj: GetBlockBodies): RLPEncodeable = {
        import obj._
        RLPList(hashes.map(e => RLPValue(e.toArray[Byte])): _*)
      }

      override def decode(rlp: RLPEncodeable): GetBlockBodies = rlp match {
        case rlpList: RLPList => GetBlockBodies(rlpList.items.map(e => ByteString(e: Array[Byte])))

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
