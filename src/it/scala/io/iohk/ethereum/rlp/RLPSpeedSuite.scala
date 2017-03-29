package io.iohk.ethereum.rlp

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.domain.{Address, BlockHeader, SignedTransaction, Transaction}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._
import io.iohk.ethereum.rlp
import io.iohk.ethereum.utils.Logger
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.spongycastle.util.encoders.Hex

/**
 * Tests based on
 *   - https://github.com/cryptape/ruby-rlp/blob/master/test/speed.rb
 *   - https://github.com/ethereum/pyrlp/blob/develop/tests/speed.py
 */
class RLPSpeedSuite extends FunSuite
    with PropertyChecks
    with GeneratorDrivenPropertyChecks
    with ObjectGenerators
    with Logger {

  val rounds = 10000

  test("Main") {
    val startBlockSerialization: Long = System.currentTimeMillis
    val block = blockGen.sample.get
    val serializedBlock = doTestSerialize[TestBlock](block, rounds)(TestBlock.encDec)
    val elapsedBlockSerialization = (System.currentTimeMillis() - startBlockSerialization) / 1000f
    log.info(s"Block serializations / sec: (${rounds.toFloat / elapsedBlockSerialization})")

    val blockDeserializationStart: Long = System.currentTimeMillis
    val deserializedBlock: TestBlock = doTestDeserialize(serializedBlock, rounds)(TestBlock.encDec)
    val elapsedBlockDeserialization = (System.currentTimeMillis() - blockDeserializationStart) / 1000f
    log.info(s"Block deserializations / sec: (${rounds.toFloat / elapsedBlockDeserialization})")

    val serializationTxStart: Long = System.currentTimeMillis
    val tx = validTransaction
    val serializedTx = doTestSerialize(tx, rounds)(SignedTransactions.txRlpEncDec)
    val elapsedTxSerialization = (System.currentTimeMillis() - serializationTxStart) / 1000f
    log.info(s"TX serializations / sec: (${rounds.toFloat / elapsedTxSerialization})")

    val txDeserializationStart: Long = System.currentTimeMillis
    val deserializedTx: SignedTransaction = doTestDeserialize(serializedTx, rounds)(SignedTransactions.txRlpEncDec)
    val elapsedTxDeserialization = (System.currentTimeMillis() - txDeserializationStart) / 1000f
    log.info(s"TX deserializations / sec: (${rounds.toFloat / elapsedTxDeserialization})")
  }

  test("Performance decode") {
    val blockRaw: String = "f8cbf8c7a00000000000000000000000000000000000000000000000000000000000000000a01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347940000000000000000000000000000000000000000a02f4399b08efe68945c1cf90ffe85bbe3ce978959da753f9e649f034015b8817da00000000000000000000000000000000000000000000000000000000000000000834000008080830f4240808080a004994f67dc55b09e814ab7ffc8df3686b4afb2bb53e60eae97ef043fe03fb829c0c0"
    val payload: Array[Byte] = Hex.decode(blockRaw)
    val ITERATIONS: Int = 10000000
    log.info("Starting " + ITERATIONS + " decoding iterations...")
    val start1: Long = System.currentTimeMillis
    (1 to ITERATIONS).foreach { _ => RLP.rawDecode(payload); Unit }
    val end1: Long = System.currentTimeMillis
    log.info("Result decode()\t: " + (end1 - start1) + "ms")
  }

  def doTestSerialize[T](toSerialize: T, rounds: Int)(implicit enc: RLPEncoder[T]): Array[Byte] = {
    (1 until rounds).foreach(_ => { encode[T](toSerialize) })
    encode[T](toSerialize)
  }

  def doTestDeserialize[T](serialized: Array[Byte], rounds: Int)(implicit dec: RLPDecoder[T]): T = {
    (1 until rounds).foreach(_ => { decode[T](serialized) })
    decode[T](serialized)
  }

  val validTransaction = SignedTransaction(
    Transaction(
      nonce = 172320,
      gasPrice = BigInt("50000000000"),
      gasLimit = 90000,
      receivingAddress = Address(Hex.decode("1c51bf013add0857c5d9cf2f71a7f15ca93d4816")),
      value = BigInt("1049756850000000000"),
      payload = ByteString.empty),
    pointSign = 28,
    signatureRandom = ByteString(Hex.decode("cfe3ad31d6612f8d787c45f115cc5b43fb22bcc210b62ae71dc7cbf0a6bea8df")),
    signature = ByteString(Hex.decode("57db8998114fae3c337e99dbd8573d4085691880f4576c6c1f6c5bbfe67d6cf0"))
  ).get

  lazy val blockGen: Gen[TestBlock] = for {
    header <- blockHeaderGen
    uncles <- blockHeaderGen
  } yield TestBlock(header = header, transactions = List.fill(10)(validTransaction), uncles = Seq(uncles))
}

// FIXME Replace with our entity once implemented in our codebase
case class TestBlock(header: BlockHeader, transactions: Seq[SignedTransaction], uncles: Seq[BlockHeader])

object TestBlock {

  implicit val encDec = new RLPEncoder[TestBlock] with RLPDecoder[TestBlock] {

    override def encode(obj: TestBlock): RLPEncodeable = {
      val rplEncodeables: Seq[RLPEncodeable] =
        obj.transactions.map(SignedTransactions.txRlpEncDec.encode)
      RLPList(headerRlpEncDec.encode(obj.header),
              RLPList(rplEncodeables: _*),
              RLPList(obj.uncles.map(headerRlpEncDec.encode(_)): _*))
    }

    override def decode(rlp: RLPEncodeable): TestBlock = rlp match {
      case RLPList(header, txs: RLPList, uncles: RLPList) =>
        TestBlock(headerRlpEncDec.decode(header),
                  txs.items.map(SignedTransactions.txRlpEncDec.decode),
                  uncles.items.map(headerRlpEncDec.decode))
      case _ => throw new RuntimeException("Invalid Block encodeable")
    }

  }
}
