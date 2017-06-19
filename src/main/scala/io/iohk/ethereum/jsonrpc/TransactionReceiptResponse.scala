package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.Address

case class TransactionReceiptResponse(
  transactionHash: ByteString,
  transactionIndex: BigInt,
  blockNumber: BigInt,
  blockHash: ByteString,
  cumulativeGasUsed: BigInt,
  gasUsed: BigInt,
  contractAddress: Option[Address],
  logs: Seq[TxLog]
)

//TODO extract to top level because it is used in FilterManager.scala
case class TxLog(
  logIndex: BigInt,
  transactionIndex: Option[BigInt],
  transactionHash: Option[ByteString],
  blockHash: ByteString,
  blockNumber: BigInt,
  address: Address,
  data: ByteString,
  topics: Seq[ByteString])
