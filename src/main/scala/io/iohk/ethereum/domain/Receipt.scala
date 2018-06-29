package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.mpt.ByteArraySerializable
import org.bouncycastle.util.encoders.Hex

object Receipt {

  val byteArraySerializable = new ByteArraySerializable[Receipt] {
    import io.iohk.ethereum.network.p2p.messages.PV63.ReceiptImplicits._

    override def fromBytes(bytes: Array[Byte]): Receipt = bytes.toReceipt

    override def toBytes(input: Receipt): Array[Byte] = input.toBytes
  }

}

/**
  * @param postTransactionStateHash For blocks where block.number >= byzantium-block-number (from config),
  *                                 the intermediate state root is replaced by a status code, 0 indicating failure
  *                                 (due to any operation that can cause the transaction or top-level call to revert)
  *                                 and 1 indicating success.
  *
  * More description: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-658.md
  **/
case class Receipt(
                    postTransactionStateHash: ByteString,
                    cumulativeGasUsed: BigInt,
                    logsBloomFilter: ByteString,
                    logs: Seq[TxLogEntry]
                  ) {
  override def toString: String = {
    s"""
       |Receipt{
       | postTransactionStateHash: ${Hex.toHexString(postTransactionStateHash.toArray[Byte])}
       | cumulativeGasUsed: $cumulativeGasUsed
       | logsBloomFilter: ${Hex.toHexString(logsBloomFilter.toArray[Byte])}
       | logs: $logs
       |}
       """.stripMargin
  }
}
