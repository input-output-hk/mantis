package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.TransactionLog
import io.iohk.ethereum.network.p2p.Message.{PV63 => constantPV63, decode => msgDecode}
import io.iohk.ethereum.network.p2p.messages.PV63.{Receipt, Receipts}
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class ReceiptsSpec extends FlatSpec with Matchers {

  val exampleHash = ByteString(kec256((0 until 32).map(_ => 1: Byte).toArray))
  val exampleLogsBloom = ByteString((0 until 256).map(_ => 1: Byte).toArray)

  val loggerAddress = ByteString(Hex.decode("ff"))
  val logData = ByteString(Hex.decode("bb"))
  val logTopics = Seq(ByteString(Hex.decode("dd")), ByteString(Hex.decode("aa")))

  val exampleLog = TransactionLog(loggerAddress, logTopics, logData)

  val cumulativeGas: BigInt = 0

  val receipt = Receipt(
    postTransactionStateHash = exampleHash,
    cumulativeGasUsed = cumulativeGas,
    logsBloomFilter = exampleLogsBloom,
    logs = Seq(exampleLog)
  )

  val receipts = Receipts(Seq(Seq(receipt)))

  val encodedReceipts =
    RLPList(RLPList(
      RLPList(
        exampleHash,
        cumulativeGas,
        exampleLogsBloom,
        RLPList(RLPList(loggerAddress, logTopics, logData))
      )))

  "Receipts" should "encode receipts" in {
    encode(receipts) shouldBe encode(encodedReceipts)
  }

  it should "decode receipts" in {
    msgDecode(Receipts.code, encode(encodedReceipts), constantPV63) shouldBe receipts
  }

  it should "decode encoded receipts" in {
    msgDecode(Receipts.code, encode(receipts), constantPV63) shouldBe receipts
  }
}
