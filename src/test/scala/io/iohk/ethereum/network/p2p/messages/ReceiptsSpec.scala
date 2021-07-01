package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.domain.TxLogEntry
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

  val receipt: Receipt = Receipt.withHashOutcome(
    postTransactionStateHash = exampleHash,
    cumulativeGasUsed = cumulativeGas,
    logsBloomFilter = exampleLogsBloom,
    logs = Seq(exampleLog)
  )

  val receipts: Receipts = Receipts(Seq(Seq(receipt)))

  val encodedReceipts: RLPList =
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

  "Receipts" should "encode receipts" in {
    (receipts.toBytes: Array[Byte]) shouldBe encode(encodedReceipts)
  }

  it should "decode receipts" in {
    EthereumMessageDecoder
      .ethMessageDecoder(ProtocolVersions.ETH63)
      .fromBytes(
        Codes.ReceiptsCode,
        encode(encodedReceipts)
      ) shouldBe receipts
  }

  it should "decode encoded receipts" in {
    EthereumMessageDecoder
      .ethMessageDecoder(ProtocolVersions.ETH63)
      .fromBytes(Codes.ReceiptsCode, receipts.toBytes) shouldBe receipts
  }
}
