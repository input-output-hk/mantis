package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{Address, BlockHeader, FailureOutcome, HashOutcome, Receipt, SignedTransaction, SuccessOutcome, UInt256}
import io.iohk.ethereum.jsonrpc.FilterManager.TxLog
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.UInt256RLPImplicits._

/**  Params docs copied from - https://eth.wiki/json-rpc/API
  *
  *  @param transactionHash DATA, 32 Bytes - hash of the transaction.
  *  @param transactionIndex QUANTITY - integer of the transactions index position in the block.
  *  @param blockHash DATA, 32 Bytes - hash of the block where this transaction was in.
  *  @param blockNumber QUANTITY - block number where this transaction was in.
  *  @param from DATA, 20 Bytes - address of the sender.
  *  @param to DATA, 20 Bytes - address of the receiver. None when its a contract creation transaction.
  *  @param cumulativeGasUsed QUANTITY  - The total amount of gas used when this transaction was executed in the block.
  *  @param gasUsed QUANTITY  - The amount of gas used by this specific transaction alone.
  *  @param contractAddress DATA, 20 Bytes - The contract address created, if the transaction was a contract creation, otherwise None.
  *  @param logs Array - Array of log objects, which this transaction generated.
  *  @param logsBloom DATA, 256 Bytes - Bloom filter for light clients to quickly retrieve related logs.
  *  @param root DATA 32 bytes of post-transaction stateroot (pre Byzantium, otherwise None)
  *  @param status QUANTITY either 1 (success) or 0 (failure) (post Byzantium, otherwise None)
  */
case class TransactionReceiptResponse(
    transactionHash: ByteString,
    transactionIndex: BigInt,
    blockNumber: BigInt,
    blockHash: ByteString,
    from: Address,
    to: Option[Address],
    cumulativeGasUsed: BigInt,
    gasUsed: BigInt,
    contractAddress: Option[Address],
    logs: Seq[TxLog],
    logsBloom: ByteString,
    root: Option[ByteString],
    status: Option[BigInt]
)

object TransactionReceiptResponse {

  def apply(
      receipt: Receipt,
      stx: SignedTransaction,
      signedTransactionSender: Address,
      transactionIndex: Int,
      blockHeader: BlockHeader,
      gasUsedByTransaction: BigInt
  ): TransactionReceiptResponse = {
    val contractAddress = if (stx.tx.isContractInit) {
      //do not subtract 1 from nonce because in transaction we have nonce of account before transaction execution
      val hash = kec256(rlp.encode(RLPList(signedTransactionSender.bytes, UInt256(stx.tx.nonce).toRLPEncodable)))
      Some(Address(hash))
    } else {
      None
    }
    val txLogs = receipt.logs.zipWithIndex.map { case (txLog, index) =>
      TxLog(
        logIndex = index,
        transactionIndex = transactionIndex,
        transactionHash = stx.hash,
        blockHash = blockHeader.hash,
        blockNumber = blockHeader.number,
        address = txLog.loggerAddress,
        data = txLog.data,
        topics = txLog.logTopics
      )
    }

    val (root, status) = receipt.postTransactionStateHash match {
      case FailureOutcome         => (None, Some(BigInt(0)))
      case SuccessOutcome         => (None, Some(BigInt(1)))
      case HashOutcome(stateHash) => (Some(stateHash), None)
    }

    new TransactionReceiptResponse(
      transactionHash = stx.hash,
      transactionIndex = transactionIndex,
      blockNumber = blockHeader.number,
      blockHash = blockHeader.hash,
      from = signedTransactionSender,
      to = stx.tx.receivingAddress,
      cumulativeGasUsed = receipt.cumulativeGasUsed,
      gasUsed = gasUsedByTransaction,
      contractAddress = contractAddress,
      logs = txLogs,
      logsBloom = receipt.logsBloomFilter,
      root = root,
      status = status
    )
  }
}
