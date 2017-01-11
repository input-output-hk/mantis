package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.rlp.RLPImplicits._
import org.spongycastle.util.encoders.Hex

import CommonMessages._

object Message {

  type Version = Int

  val PV61: Version = 61
  val PV62: Version = 62
  val PV63: Version = 63

  def decode(`type`: Int, payload: Array[Byte], protocolVersion: Version): Message = (protocolVersion, `type`) match {
    case (_, Hello.code) => rlp.decode(payload)(Hello.rlpEndDec)
    case (_, Disconnect.code) => rlp.decode(payload)(Disconnect.rlpEndDec)
    case (_, Ping.code) => rlp.decode(payload)(Ping.rlpEndDec)
    case (_, Pong.code) => rlp.decode(payload)(Pong.rlpEndDec)
    case (_, Status.code) => rlp.decode(payload)(Status.rlpEndDec)
    case (_, Transactions.code) => rlp.decode(payload)(Transactions.rlpEndDec)
    case (PV62, NewBlockHashes.code) => rlp.decode(payload)(NewBlockHashes.rlpEndDec)
    case (PV62, GetBlockHeaders.code) => rlp.decode(payload)(GetBlockHeaders.rlpEndDec)
    case (PV62, BlockHeaders.code) => rlp.decode(payload)(BlockHeaders.rlpEndDec)
    case (PV62, GetBlockBodies.code) => rlp.decode(payload)(GetBlockBodies.rlpEndDec)
    case (PV62, BlockBodies.code) => rlp.decode(payload)(BlockBodies.rlpEndDec)
    case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
  }
}

trait Message {
  def code: Int
}

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

  val code: Int = 0x10 + 0x01
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

  val code: Int = 0x10 + 0x03
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

  val code: Int = 0x10 + 0x06
}

case class BlockBodies(bodies: Seq[BlockBody]) extends Message {
  val code: Int = BlockBodies.code
}

object BlockBody {
  implicit val rlpEndDec = new RLPEncoder[BlockBody] with RLPDecoder[BlockBody] {
    override def encode(obj: BlockBody): RLPEncodeable = {
      import obj._
      RLPList(
        RLPList(transactionList.map(Transaction.rlpEndDec.encode): _*),
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

  val code: Int = 0x10 + 0x04
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

  val code: Int = 0x10 + 0x05
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

object Transactions {
  implicit val rlpEndDec = new RLPEncoder[Transactions] with RLPDecoder[Transactions] {
    override def encode(obj: Transactions): RLPEncodeable = {
      import obj._
      RLPList(txs.map(Transaction.rlpEndDec.encode): _*)
    }

    override def decode(rlp: RLPEncodeable): Transactions = rlp match {
      case rlpList: RLPList => Transactions(rlpList.items.map(Transaction.rlpEndDec.decode))
      case _ => throw new RuntimeException("Cannot decode Transactions")
    }
  }

  val code: Int = 0x10 + 0x02
}

case class Transactions(txs: Seq[Transaction]) extends Message {
  override def code: Int = Transactions.code
}

object Transaction {
  implicit val rlpEndDec = new RLPEncoder[Transaction] with RLPDecoder[Transaction] {
    override def encode(obj: Transaction): RLPEncodeable = {
      import obj._
      RLPList(nonce, gasPrice, gasLimit, receivingAddress.toArray[Byte], value,
        payload.fold(_.byteString.toArray[Byte], _.byteString.toArray[Byte]),
        pointSign, signatureRandom.toArray[Byte], signature.toArray[Byte])
    }

    override def decode(rlp: RLPEncodeable): Transaction = rlp match {
      case RLPList(nonce, gasPrice, gasLimit, (receivingAddress: RLPValue), value, payload, pointSign, signatureRandom, signature)
        if receivingAddress.bytes.nonEmpty =>
        Transaction(nonce, gasPrice, gasLimit, ByteString(receivingAddress: Array[Byte]), value, Right(TransactionData(ByteString(payload: Array[Byte]))),
          pointSign, ByteString(signatureRandom: Array[Byte]), ByteString(signature: Array[Byte]))

      case RLPList(nonce, gasPrice, gasLimit, (receivingAddress: RLPValue), value, payload, pointSign, signatureRandom, signature)
        if receivingAddress.bytes.isEmpty =>
        Transaction(nonce, gasPrice, gasLimit, ByteString(), value, Left(ContractInit(ByteString(payload: Array[Byte]))),
          pointSign, ByteString(signatureRandom: Array[Byte]), ByteString(signature: Array[Byte]))

      case _ => throw new RuntimeException("Cannot decode Transaction")
    }
  }
}

//ETH yellow paper section 4.3
case class Transaction(
    nonce: BigInt,
    gasPrice: BigInt,
    gasLimit: BigInt,
    receivingAddress: ByteString,
    value: BigInt,
    payload: Either[ContractInit, TransactionData],
    //yellow paper appendix F
    pointSign: Byte, //v - 27 or 28 according to yellow paper, but it is 37 and 38 in ETH
    signatureRandom: ByteString, //r
    signature: ByteString /*s*/) {

  override def toString: String = {
    s"""Transaction {
       |nonce: $nonce
       |gasPrice: $gasPrice
       |gasLimit: $gasLimit
       |receivingAddress: ${Hex.toHexString(receivingAddress.toArray[Byte])}
       |value: $value wei
       |payload: ${payload.fold(init => s"ContractInit [${Hex.toHexString(init.byteString.toArray[Byte])}]", data => s"TransactionData [${Hex.toHexString(data.byteString.toArray[Byte])}]")}
       |pointSign: $pointSign
       |signatureRandom: ${Hex.toHexString(signatureRandom.toArray[Byte])}
       |signature: ${Hex.toHexString(signature.toArray[Byte])}
       |}""".stripMargin
  }
}

case class ContractInit(byteString: ByteString)

case class TransactionData(byteString: ByteString)


