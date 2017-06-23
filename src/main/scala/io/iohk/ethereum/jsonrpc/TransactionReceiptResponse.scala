package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.FilterManager.Log

case class TransactionReceiptResponse(
  transactionHash: ByteString,
  transactionIndex: BigInt,
  blockNumber: BigInt,
  blockHash: ByteString,
  cumulativeGasUsed: BigInt,
  gasUsed: BigInt,
  contractAddress: Option[Address],
  logs: Seq[Log]
)