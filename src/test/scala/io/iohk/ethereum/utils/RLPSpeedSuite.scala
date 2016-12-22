package io.iohk.ethereum.utils

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.utils.RLPImplicits._
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

/**
  * Tests based on
  *   - https://github.com/cryptape/ruby-rlp/blob/master/test/speed.rb
  *   - https://github.com/ethereum/pyrlp/blob/develop/tests/speed.py
  */
class RLPSpeedSuite extends FunSuite
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with ObjectGenerators {

  val rounds = 10000

  test("Main") {

    val startBlockSerialization: Long = System.currentTimeMillis
    val block = blockGen.sample.get
    val serializedBlock = doTestSerialize(block, rounds)
    val elapsedsBlockSerialization = (System.currentTimeMillis() - startBlockSerialization) / 1000f
    System.out.println(s"Block serializations / sec: (${rounds.toFloat / elapsedsBlockSerialization})")

    val blockDeserializationStart: Long = System.currentTimeMillis
    val deserializedBlock: Block = doTestDeserialize(serializedBlock, rounds)
    val elapsedBlockDeserialization = (System.currentTimeMillis() - blockDeserializationStart) / 1000f
    System.out.println(s"Block deserializations / sec: (${rounds.toFloat / elapsedBlockDeserialization})")

    val serializationTxStart: Long = System.currentTimeMillis
    val tx = txGen.sample.get
    val serializedTx = doTestSerialize(tx, rounds)
    val elapsedTxSerialization = (System.currentTimeMillis() - serializationTxStart) / 1000f
    System.out.println(s"TX serializations / sec: (${rounds.toFloat / elapsedTxSerialization})")

    val txDeserializationStart: Long = System.currentTimeMillis
    val deserializedTx: Transaction = doTestDeserialize(serializedTx, rounds)
    val elapsedTxDeserialization = (System.currentTimeMillis() - txDeserializationStart) / 1000f
    System.out.println(s"TX deserializations / sec: (${rounds.toFloat / elapsedTxDeserialization})")
  }

  def doTestSerialize[T <: RLPEncodeable](toSerialize: T, rounds: Int): Array[Byte] = {
    (1 until rounds).foreach(_ => {
      RLP.encode(toSerialize)
    })
    RLP.encode(toSerialize).get
  }

  def doTestDeserialize(serialized: Array[Byte], rounds: Int): RLPEncodeable = {
    (1 until rounds).foreach(_ => {
      RLP.decode(serialized)
    })
    RLP.decode(serialized).get
  }


  lazy val txGen = for {
    nonce: Int <- intGen
    gasprice <- intGen
    startgas <- intGen
    to <- byteArrayOfNItemsGen(20)
    value <- intGen
    data <- byteArrayOfNItemsGen(32)
    v <- intGen
    r <- bigIntGen
    s <- bigIntGen
  } yield Transaction(
    nonce = nonce,
    gasprice = gasprice,
    startgas = startgas,
    to = to,
    value = value,
    data = data,
    v = 27,
    r = r,
    s = s)

  lazy val blockHeaderGen = for {
    prevhash <- byteArrayOfNItemsGen(32)
    unclesHash <- byteArrayOfNItemsGen(32)
    coinbase <- byteArrayOfNItemsGen(20)
    stateRoot <- byteArrayOfNItemsGen(32)
    txListRoot <- byteArrayOfNItemsGen(32)
    receiptsRoot <- byteArrayOfNItemsGen(32)
    bloom <- bigIntGen
    difficulty <- intGen
    number <- intGen
    gasLimit <- intGen
    gasUsed <- intGen
    timestamp <- intGen
    extraData <- byteArrayOfNItemsGen(8)
    mixhash <- byteArrayOfNItemsGen(8)
    nonce <- byteArrayOfNItemsGen(8)
  } yield BlockHeader(
    prevhash = prevhash,
    unclesHash = unclesHash,
    coinbase = coinbase,
    stateRoot = stateRoot,
    txListRoot = txListRoot,
    receiptsRoot = receiptsRoot,
    bloom = bloom,
    difficulty = difficulty,
    number = number,
    gasLimit = gasLimit,
    gasUsed = gasUsed,
    timestamp = timestamp,
    extraData = extraData,
    mixhash = mixhash,
    nonce = nonce)

  lazy val blockGen = for {
    header <- blockHeaderGen
    transactions <- Gen.listOfN(10, txGen)
    uncles <- blockHeaderGen
  } yield Block(header = header, transactions = transactions, uncles = Seq(uncles))
}

case class Transaction(nonce: Int, gasprice: Int, startgas: Int, to: Array[Byte], value: Int,
                       data: Array[Byte], v: Int, r: BigInt, s: BigInt) extends RLPList {
  override def items: Seq[RLPEncodeable] = Seq(nonce, gasprice, startgas, to, value, data, v, r, s)
}

object Transaction {
  implicit def fromRLPEncodeable(enc: RLPEncodeable): Transaction = {
    enc match {
      case l: RLPList => {
        val fields = l.items
        Transaction(nonce = fields.head, gasprice = fields(1), startgas = fields(2), to = fields(3), value = fields(4),
          data = fields(5), v = fields(6), r = fields(7), s = fields(8))
      }
      case _ => throw new RuntimeException("Invalid TX encodeable")
    }
  }
}

case class BlockHeader(prevhash: Array[Byte], unclesHash: Array[Byte], coinbase: Array[Byte], stateRoot: Array[Byte],
                       txListRoot: Array[Byte], receiptsRoot: Array[Byte], bloom: BigInt,
                       difficulty: Int, number: Int, gasLimit: Int, gasUsed: Int, timestamp: Int, extraData: Array[Byte],
                       mixhash: Array[Byte], nonce: Array[Byte]) extends RLPList {
  override def items: Seq[RLPEncodeable] = Seq(prevhash, unclesHash, coinbase, stateRoot, txListRoot,
    receiptsRoot, bloom: BigInt, difficulty, number, gasLimit, gasUsed, timestamp, extraData,
    mixhash, nonce)
}

object BlockHeader {
  implicit def fromRLPEncodeable(encBlock: RLPEncodeable): BlockHeader = {
    encBlock match {
      case l: RLPList => {
        val fields = l.items
        BlockHeader(prevhash = fields(0), unclesHash = fields(1), coinbase = fields(2), stateRoot = fields(3),
          txListRoot = fields(4), receiptsRoot = fields(6), bloom = fields(7),
          difficulty = fields(8), number = fields(9), gasLimit = fields(10), gasUsed = fields(11), timestamp = fields(11),
          extraData = fields(12), mixhash = fields(13), nonce = fields(14)
        )
      }
      case _ => throw new RuntimeException("Invalid BlockHeader encodeable")
    }
  }
}

case class Block(header: BlockHeader, transactions: Seq[Transaction], uncles: Seq[BlockHeader]) extends RLPList {
  override def items: Seq[RLPEncodeable] = Seq(header, RLPList(transactions), RLPList(uncles))
}

object Block {
  implicit def fromRLPEncodeable(enc: RLPEncodeable): Block = {
    enc match {
      case l: RLPList => {
        val fields = l.items
        val txs = fields(1) match  {
          case encTxs: RLPList => encTxs.items.map(f => f: Transaction)
          case _ => throw new RuntimeException("Invalid Block encodeable")
        }
        val headers = fields(2) match  {
          case encHeaders: RLPList => encHeaders.items.map(f => f: BlockHeader)
          case _ => throw new RuntimeException("Invalid Block encodeable")
        }
        Block(fields.head, txs, headers)
      }
      case _ => throw new RuntimeException("Invalid Block encodeable")
    }
  }
}