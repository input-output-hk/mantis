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
    val serializedBlock = doTestSerialize[Block](block, rounds)(Block.encDec)
    val elapsedsBlockSerialization = (System.currentTimeMillis() - startBlockSerialization) / 1000f
    System.out.println(s"Block serializations / sec: (${rounds.toFloat / elapsedsBlockSerialization})")

    val blockDeserializationStart: Long = System.currentTimeMillis
    val deserializedBlock: Block = doTestDeserialize(serializedBlock, rounds)(Block.encDec)
    val elapsedBlockDeserialization = (System.currentTimeMillis() - blockDeserializationStart) / 1000f
    System.out.println(s"Block deserializations / sec: (${rounds.toFloat / elapsedBlockDeserialization})")

    val serializationTxStart: Long = System.currentTimeMillis
    val tx = txGen.sample.get
    val serializedTx = doTestSerialize(tx, rounds)(Transaction.encDec)
    val elapsedTxSerialization = (System.currentTimeMillis() - serializationTxStart) / 1000f
    System.out.println(s"TX serializations / sec: (${rounds.toFloat / elapsedTxSerialization})")

    val txDeserializationStart: Long = System.currentTimeMillis
    val deserializedTx: Transaction = doTestDeserialize(serializedTx, rounds)(Transaction.encDec)
    val elapsedTxDeserialization = (System.currentTimeMillis() - txDeserializationStart) / 1000f
    System.out.println(s"TX deserializations / sec: (${rounds.toFloat / elapsedTxDeserialization})")
  }

  def doTestSerialize[T](toSerialize: T, rounds: Int)(implicit enc: RLPEncoder[T]): Array[Byte] = {
    (1 until rounds).foreach(_ => {
      RLP.encode[T](toSerialize)
    })
    RLP.encode[T](toSerialize)
  }

  def doTestDeserialize[T](serialized: Array[Byte], rounds: Int)(implicit dec: RLPDecoder[T]): T = {
    (1 until rounds).foreach(_ => {
      RLP.decode[T](serialized)
    })
    RLP.decode[T](serialized)
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
                       data: Array[Byte], v: Int, r: BigInt, s: BigInt)

object Transaction {

  implicit val encDec = new RLPEncoder[Transaction] with RLPDecoder[Transaction] {
    override def encode(obj: Transaction): RLPEncodeable = {
      import obj._
      RLPList(nonce, gasprice, startgas, to, value, data, v, r, s)
    }

    override def decode(rlp: RLPEncodeable): Transaction = rlp match {
      case RLPList(nonce, gasprice, startgas, to, value, data, v, r, s) =>
        Transaction(nonce, gasprice, startgas, to, value, data, v, r, s)
      case _ => throw new RuntimeException("Invalid Transaction")
    }
  }

  implicit def transactionFromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[Transaction]): Transaction = dec.decode(rlp)
}

case class BlockHeader(prevhash: Array[Byte], unclesHash: Array[Byte], coinbase: Array[Byte], stateRoot: Array[Byte],
                       txListRoot: Array[Byte], receiptsRoot: Array[Byte], bloom: BigInt,
                       difficulty: Int, number: Int, gasLimit: Int, gasUsed: Int, timestamp: Int, extraData: Array[Byte],
                       mixhash: Array[Byte], nonce: Array[Byte])

object BlockHeader {
  implicit val encDec = new RLPEncoder[BlockHeader] with RLPDecoder[BlockHeader] {
    override def encode(obj: BlockHeader): RLPEncodeable = {
      import obj._
      RLPList(prevhash, unclesHash, coinbase, stateRoot, txListRoot, receiptsRoot, bloom, difficulty, number, gasLimit,
        gasUsed, timestamp, extraData, mixhash, nonce)
    }

    override def decode(rlp: RLPEncodeable): BlockHeader = rlp match {
      case RLPList(prevhash, unclesHash, coinbase, stateRoot, txListRoot, receiptsRoot, bloom, difficulty, number, gasLimit,
      gasUsed, timestamp, extraData, mixhash, nonce) => {
        BlockHeader(
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
          nonce = nonce
        )
      }
      case _ => throw new RuntimeException("Invalid BlockHeader encodeable")
    }
  }

  implicit def blockHeaderFromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[BlockHeader]): BlockHeader = dec.decode(rlp)
}

case class Block(header: BlockHeader, transactions: Seq[Transaction], uncles: Seq[BlockHeader])

object Block {
  implicit val encDec = new RLPEncoder[Block] with RLPDecoder[Block] {
    override def encode(obj: Block): RLPEncodeable = {
      RLPList(obj.header,
        RLPList(obj.transactions.map(Transaction.encDec.encode):_*),
        RLPList(obj.uncles.map(BlockHeader.encDec.encode):_*)
      )
    }

    override def decode(rlp: RLPEncodeable): Block = rlp match {
      case RLPList(header, (txs: RLPList), (uncles: RLPList))  => {
        Block(header, txs.items.map(item => item: Transaction), uncles.items.map(item => item: BlockHeader))
      }
      case _ => throw new RuntimeException("Invalid Block encodeable")
    }
  }
}
