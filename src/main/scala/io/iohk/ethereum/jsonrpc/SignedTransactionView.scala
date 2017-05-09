package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.{Block, SignedTransaction}

case class SignedTransactionView(
                                  hash: ByteString,
                                  nonce: ByteString,
                                  blockHash: Option[ByteString],
                                  blockNumber: Option[ByteString],
                                  transactionIndex: Option[ByteString],
                                  from: ByteString,
                                  to: Option[ByteString],
                                  value: ByteString,
                                  gasPrice: ByteString,
                                  gas: ByteString,
                                  input: ByteString
                                )

object SignedTransactionView {

  private def bigIntAsByteString(n: BigInt): ByteString = ByteString(n.toByteArray)

  def apply(stx: SignedTransaction, block: Option[Block] = None, transactionIndex: Option[Int] = None): SignedTransactionView =
    SignedTransactionView(
      hash = stx.hash,
      nonce = bigIntAsByteString(stx.tx.nonce),
      blockHash = block.map(_.header.hash),
      blockNumber = block.map(b => bigIntAsByteString(b.header.number)),
      transactionIndex = transactionIndex.map(txIndex => bigIntAsByteString(BigInt(txIndex))),
      from = stx.senderAddress.bytes,
      to = stx.tx.receivingAddress.map(_.bytes),
      value = bigIntAsByteString(stx.tx.value),
      gasPrice = bigIntAsByteString(stx.tx.gasPrice),
      gas = bigIntAsByteString(stx.tx.gasLimit),
      input = stx.tx.payload
    )

}
