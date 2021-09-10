package io.iohk.ethereum.jsonrpc

import akka.util.ByteString

import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config

trait BaseTransactionResponse {
  def hash: ByteString
  def nonce: BigInt
  def blockHash: Option[ByteString]
  def blockNumber: Option[BigInt]
  def transactionIndex: Option[BigInt]
  def from: Option[ByteString]
  def to: Option[ByteString]
  def value: BigInt
  def gasPrice: BigInt
  def gas: BigInt
  def input: ByteString
}

final case class TransactionResponse(
    hash: ByteString,
    nonce: BigInt,
    blockHash: Option[ByteString],
    blockNumber: Option[BigInt],
    transactionIndex: Option[BigInt],
    from: Option[ByteString],
    to: Option[ByteString],
    value: BigInt,
    gasPrice: BigInt,
    gas: BigInt,
    input: ByteString
) extends BaseTransactionResponse

final case class TransactionData(
    stx: SignedTransaction,
    blockHeader: Option[BlockHeader] = None,
    transactionIndex: Option[Int] = None
)

object TransactionResponse {

  implicit val blockchainConfig: BlockchainConfig = Config.blockchains.blockchainConfig

  def apply(tx: TransactionData): TransactionResponse =
    TransactionResponse(tx.stx, tx.blockHeader, tx.transactionIndex)

  def apply(
      stx: SignedTransaction,
      blockHeader: Option[BlockHeader] = None,
      transactionIndex: Option[Int] = None
  ): TransactionResponse =
    TransactionResponse(
      hash = stx.hash,
      nonce = stx.tx.nonce,
      blockHash = blockHeader.map(_.hash),
      blockNumber = blockHeader.map(_.number),
      transactionIndex = transactionIndex.map(txIndex => BigInt(txIndex)),
      from = SignedTransaction.getSender(stx).map(_.bytes),
      to = stx.tx.receivingAddress.map(_.bytes),
      value = stx.tx.value,
      gasPrice = stx.tx.gasPrice,
      gas = stx.tx.gasLimit,
      input = stx.tx.payload
    )

}
