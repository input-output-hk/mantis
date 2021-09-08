package io.iohk.ethereum.domain

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex

import io.iohk.ethereum.mpt.ByteArraySerializable

sealed trait Receipt {
  def postTransactionStateHash: TransactionOutcome
  def cumulativeGasUsed: BigInt
  def logsBloomFilter: ByteString
  def logs: Seq[TxLogEntry]
}

// shared structure for EIP-2930, EIP-1559
abstract class TypedLegacyReceipt(transactionTypeId: Byte, val delegateReceipt: LegacyReceipt) extends Receipt {
  def postTransactionStateHash: TransactionOutcome = delegateReceipt.postTransactionStateHash
  def cumulativeGasUsed: BigInt = delegateReceipt.cumulativeGasUsed
  def logsBloomFilter: ByteString = delegateReceipt.logsBloomFilter
  def logs: Seq[TxLogEntry] = delegateReceipt.logs
}

object Receipt {

  val byteArraySerializable: ByteArraySerializable[Receipt] = new ByteArraySerializable[Receipt] {

    import io.iohk.ethereum.network.p2p.messages.ETH63.ReceiptImplicits._

    override def fromBytes(bytes: Array[Byte]): Receipt = bytes.toReceipt

    override def toBytes(input: Receipt): Array[Byte] = input.toBytes
  }
}

object LegacyReceipt {
  def withHashOutcome(
      postTransactionStateHash: ByteString,
      cumulativeGasUsed: BigInt,
      logsBloomFilter: ByteString,
      logs: Seq[TxLogEntry]
  ): LegacyReceipt =
    LegacyReceipt(HashOutcome(postTransactionStateHash), cumulativeGasUsed, logsBloomFilter, logs)
}

object Type01Receipt {
  def withHashOutcome(
      postTransactionStateHash: ByteString,
      cumulativeGasUsed: BigInt,
      logsBloomFilter: ByteString,
      logs: Seq[TxLogEntry]
  ): Type01Receipt =
    Type01Receipt(LegacyReceipt.withHashOutcome(postTransactionStateHash, cumulativeGasUsed, logsBloomFilter, logs))
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
case class LegacyReceipt(
    postTransactionStateHash: TransactionOutcome,
    cumulativeGasUsed: BigInt,
    logsBloomFilter: ByteString,
    logs: Seq[TxLogEntry]
) extends Receipt {
  def toPrettyString(prefix: String): String = {
    val stateHash = postTransactionStateHash match {
      case HashOutcome(hash) => hash.toArray[Byte]
      case SuccessOutcome    => Array(1.toByte)
      case _                 => Array(0.toByte)
    }

    s"${prefix}{ " +
      s"postTransactionStateHash: ${Hex.toHexString(stateHash)}, " +
      s"cumulativeGasUsed: $cumulativeGasUsed, " +
      s"logsBloomFilter: ${Hex.toHexString(logsBloomFilter.toArray[Byte])}, " +
      s"logs: $logs" +
      s"}"
  }

  override def toString: String = toPrettyString("LegacyReceipt")
}

/** EIP-2930 receipt for Transaction type 1
  * @param legacyReceipt
  */
case class Type01Receipt(legacyReceipt: LegacyReceipt) extends TypedLegacyReceipt(Transaction.Type01, legacyReceipt) {
  override def toString: String = legacyReceipt.toPrettyString("Type01Receipt")
}
