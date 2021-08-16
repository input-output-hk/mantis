package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.LegacyReceipt
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.domain.Transaction
import io.iohk.ethereum.domain.TxLogEntry
import io.iohk.ethereum.domain.Type01Receipt
import io.iohk.ethereum.network.p2p.EthereumMessageDecoder
import io.iohk.ethereum.network.p2p.messages.ETH63.Receipts
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._

class ReceiptsSpec extends AnyFlatSpec with Matchers {

  val exampleHash: ByteString = ByteString(kec256((0 until 32).map(_ => 1: Byte).toArray))
  val exampleLogsBloom: ByteString = ByteString((0 until 256).map(_ => 1: Byte).toArray)

  val loggerAddress: Address = Address(0xff)
  val logData: ByteString = ByteString(Hex.decode("bb"))
  val logTopics: Seq[ByteString] = Seq(ByteString(Hex.decode("dd")), ByteString(Hex.decode("aa")))

  val exampleLog: TxLogEntry = TxLogEntry(loggerAddress, logTopics, logData)

  val cumulativeGas: BigInt = 0

  val legacyReceipt: Receipt = LegacyReceipt.withHashOutcome(
    postTransactionStateHash = exampleHash,
    cumulativeGasUsed = cumulativeGas,
    logsBloomFilter = exampleLogsBloom,
    logs = Seq(exampleLog)
  )

  val type01Receipt: Receipt = Type01Receipt(legacyReceipt.asInstanceOf[LegacyReceipt])

  val legacyReceipts: Receipts = Receipts(Seq(Seq(legacyReceipt)))

  val type01Receipts: Receipts = Receipts(Seq(Seq(type01Receipt)))

  val encodedLegacyReceipts: RLPList =
    RLPList(
      RLPList(
        RLPList(
          exampleHash,
          cumulativeGas,
          exampleLogsBloom,
          RLPList(RLPList(loggerAddress.bytes, logTopics, logData))
        )
      )
    )

  val encodedType01Receipts: RLPList =
    RLPList(
      RLPList(
        PrefixedRLPEncodable(
          Transaction.Type01,
          RLPList(
            exampleHash,
            cumulativeGas,
            exampleLogsBloom,
            RLPList(RLPList(loggerAddress.bytes, logTopics, logData))
          )
        )
      )
    )

  "Legacy Receipts" should "encode legacy receipts" in {
    (legacyReceipts.toBytes: Array[Byte]) shouldBe encode(encodedLegacyReceipts)
  }

  it should "decode legacy receipts" in {
    EthereumMessageDecoder
      .ethMessageDecoder(Capability.ETH63)
      .fromBytes(
        Codes.ReceiptsCode,
        encode(encodedLegacyReceipts)
      ) shouldBe Right(legacyReceipts)
  }

  it should "decode encoded legacy receipts" in {
    EthereumMessageDecoder
      .ethMessageDecoder(Capability.ETH63)
      .fromBytes(Codes.ReceiptsCode, legacyReceipts.toBytes) shouldBe Right(legacyReceipts)
  }

  "Type 01 Receipts" should "encode type 01 receipts" in {
    (type01Receipts.toBytes: Array[Byte]) shouldBe encode(encodedType01Receipts)
  }

  it should "decode type 01 receipts" in {
    EthereumMessageDecoder
      .ethMessageDecoder(Capability.ETH64)
      .fromBytes(
        Codes.ReceiptsCode,
        encode(encodedType01Receipts)
      ) shouldBe Right(type01Receipts)
  }

  it should "decode encoded type 01 receipts" in {
    EthereumMessageDecoder
      .ethMessageDecoder(Capability.ETH64)
      .fromBytes(Codes.ReceiptsCode, type01Receipts.toBytes) shouldBe Right(type01Receipts)
  }

}
