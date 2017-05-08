package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, Block, SignedTransaction}

case class SignedTransactionView(
                                  hash: ByteString,
                                  nonce: BigInt,
                                  blockHash: Option[ByteString],
                                  blockNumber: Option[BigInt],
                                  transactionIndex: Option[Int],
                                  senderAddress: Address,
                                  receivingAddress: Option[Address],
                                  value: BigInt,
                                  gasPrice: BigInt,
                                  gasLimit: BigInt,
                                  input: ByteString
                                )

object SignedTransactionView {
  def apply(stx: SignedTransaction, block: Option[Block] = None, transactionIndex: Option[Int] = None): SignedTransactionView =
    SignedTransactionView(
      hash = stx.hash,
      nonce = stx.tx.nonce,
      blockHash = block.map(_.header.hash),
      blockNumber = block.map(_.header.number),
      transactionIndex = transactionIndex,
      senderAddress = stx.senderAddress,
      receivingAddress = stx.tx.receivingAddress,
      value = stx.tx.value,
      gasPrice = stx.tx.gasPrice,
      gasLimit = stx.tx.gasLimit,
      input = stx.tx.payload
    )
}
