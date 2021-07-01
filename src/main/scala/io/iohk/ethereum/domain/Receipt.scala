package io.iohk.ethereum.domain

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex

import io.iohk.ethereum.mpt.ByteArraySerializable

object Receipt {

  val byteArraySerializable: ByteArraySerializable[Receipt] = new ByteArraySerializable[Receipt] {
    import io.iohk.ethereum.network.p2p.messages.ETH63.ReceiptImplicits._

    override def fromBytes(bytes: Array[Byte]): Receipt = bytes.toReceipt

    override def toBytes(input: Receipt): Array[Byte] = input.toBytes
  }

  def withHashOutcome(
      postTransactionStateHash: ByteString,
      cumulativeGasUsed: BigInt,
      logsBloomFilter: ByteString,
      logs: Seq[TxLogEntry]
  ): Receipt =
    Receipt(HashOutcome(postTransactionStateHash), cumulativeGasUsed, logsBloomFilter, logs)

}

/** @param postTransactionStateHash For blocks where block.number >= byzantium-block-number (from config),
  *                                 the intermediate state root is replaced by a status code,
  *                                 0 indicating failure [[FailureOutcome]] (due to any operation that can cause
  *                                 the transaction or top-level call to revert)
  *                                 1 indicating success [[SuccessOutcome]].
  *                                 For other blocks state root stays [[HashOutcome]].
  *
  * More description: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-658.md
  */
case class Receipt(
    postTransactionStateHash: TransactionOutcome,
    cumulativeGasUsed: BigInt,
    logsBloomFilter: ByteString,
    logs: Seq[TxLogEntry]
) {
  override def toString: String = {
    val stateHash = postTransactionStateHash match {
      case HashOutcome(hash) => hash.toArray[Byte]
      case SuccessOutcome    => Array(1.toByte)
      case _                 => Array(0.toByte)
    }

    s"Receipt{ " +
      s"postTransactionStateHash: ${Hex.toHexString(stateHash)}, " +
      s"cumulativeGasUsed: $cumulativeGasUsed, " +
      s"logsBloomFilter: ${Hex.toHexString(logsBloomFilter.toArray[Byte])}, " +
      s"logs: $logs" +
      s"}"
  }
}
