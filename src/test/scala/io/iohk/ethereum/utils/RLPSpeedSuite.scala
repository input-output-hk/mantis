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
    RLP.encode[T](toSerialize).get
  }

  def doTestDeserialize[T](serialized: Array[Byte], rounds: Int)(implicit dec: RLPDecoder[T]): T = {
    (1 until rounds).foreach(_ => {
      RLP.decode[T](serialized)
    })
    RLP.decode[T](serialized).get
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
    override def encode(obj: Transaction): RLPEncodeable = new RLPList {
      override def items: Seq[RLPEncodeable] = intEncDec.encode(obj.nonce) ::
        intEncDec.encode(obj.gasprice) ::
        intEncDec.encode(obj.startgas) ::
        byteArrayEncDec.encode(obj.to) ::
        intEncDec.encode(obj.value) ::
        byteArrayEncDec.encode(obj.data) ::
        intEncDec.encode(obj.v) ::
        bigIntEncDec.encode(obj.r) ::
        bigIntEncDec.encode(obj.s) :: Nil
    }

    override def decode(rlp: RLPEncodeable): Transaction = rlp match {
      case l: RLPList =>
        val items = l.items
        Transaction(
          intEncDec.decode(items.head),
          intEncDec.decode(items(1)),
          intEncDec.decode(items(2)),
          byteArrayEncDec.decode(items(3)),
          intEncDec.decode(items(4)),
          byteArrayEncDec.decode(items(5)),
          intEncDec.decode(items(6)),
          bigIntEncDec.decode(items(7)),
          bigIntEncDec.decode(items(8))
        )
      case _ => throw new RuntimeException("Invalid Transaction")
    }
  }
}

case class BlockHeader(prevhash: Array[Byte], unclesHash: Array[Byte], coinbase: Array[Byte], stateRoot: Array[Byte],
                       txListRoot: Array[Byte], receiptsRoot: Array[Byte], bloom: BigInt,
                       difficulty: Int, number: Int, gasLimit: Int, gasUsed: Int, timestamp: Int, extraData: Array[Byte],
                       mixhash: Array[Byte], nonce: Array[Byte])

object BlockHeader {
  implicit val encDec = new RLPEncoder[BlockHeader] with RLPDecoder[BlockHeader] {
    override def encode(obj: BlockHeader): RLPEncodeable = new RLPList {
      override def items: Seq[RLPEncodeable] =
        byteArrayEncDec.encode(obj.prevhash) ::
          byteArrayEncDec.encode(obj.unclesHash) ::
          byteArrayEncDec.encode(obj.coinbase) ::
          byteArrayEncDec.encode(obj.stateRoot) ::
          byteArrayEncDec.encode(obj.txListRoot) ::
          byteArrayEncDec.encode(obj.receiptsRoot) ::
          bigIntEncDec.encode(obj.bloom) ::
          intEncDec.encode(obj.difficulty) ::
          intEncDec.encode(obj.number) ::
          intEncDec.encode(obj.gasLimit) ::
          intEncDec.encode(obj.gasUsed) ::
          intEncDec.encode(obj.timestamp) ::
          byteArrayEncDec.encode(obj.extraData) ::
          byteArrayEncDec.encode(obj.mixhash) ::
          byteArrayEncDec.encode(obj.nonce) :: Nil
    }

    override def decode(rlp: RLPEncodeable): BlockHeader = rlp match {
      case l: RLPList => {
        val fields = l.items
        BlockHeader(
          prevhash = byteArrayEncDec.decode(fields.head),
          unclesHash = byteArrayEncDec.decode(fields(1)),
          coinbase = byteArrayEncDec.decode(fields(2)),
          stateRoot = byteArrayEncDec.decode(fields(3)),
          txListRoot = byteArrayEncDec.decode(fields(4)),
          receiptsRoot = byteArrayEncDec.decode(fields(6)),
          bloom = bigIntEncDec.decode(fields(7)),
          difficulty = intEncDec.decode(fields(8)),
          number = intEncDec.decode(fields(9)),
          gasLimit = intEncDec.decode(fields(10)),
          gasUsed = intEncDec.decode(fields(11)),
          timestamp = intEncDec.decode(fields(11)),
          extraData = byteArrayEncDec.decode(fields(12)),
          mixhash = byteArrayEncDec.decode(fields(13)),
          nonce = byteArrayEncDec.decode(fields(14))
        )
      }
      case _ => throw new RuntimeException("Invalid BlockHeader encodeable")
    }
  }
}

case class Block(header: BlockHeader, transactions: Seq[Transaction], uncles: Seq[BlockHeader])

object Block {
  implicit val encDec = new RLPEncoder[Block] with RLPDecoder[Block] {
    override def encode(obj: Block): RLPEncodeable = new RLPList {
      override def items: Seq[RLPEncodeable] = BlockHeader.encDec.encode(obj.header) ::
        new RLPList {
          override def items: Seq[RLPEncodeable] = obj.transactions.map(Transaction.encDec.encode)
        } ::
        new RLPList {
          override def items: Seq[RLPEncodeable] = obj.uncles.map(BlockHeader.encDec.encode)
        } :: Nil
    }

    override def decode(rlp: RLPEncodeable): Block = rlp match {
      case l: RLPList => {
        val fields = l.items
        val txs = fields(1) match {
          case encTxs: RLPList => encTxs.items.map(Transaction.encDec.decode)
          case _ => throw new RuntimeException("Invalid Block encodeable")
        }
        val headers = fields(2) match {
          case encHeaders: RLPList => encHeaders.items.map(BlockHeader.encDec.decode)
          case _ => throw new RuntimeException("Invalid Block encodeable")
        }
        Block(BlockHeader.encDec.decode(fields.head), txs, headers)
      }
      case _ => throw new RuntimeException("Invalid Block encodeable")
    }
  }
}
