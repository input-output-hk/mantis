package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, Transaction}

case class TransactionRequest(
  from: Address,
  to: Option[Address],
  value: Option[BigInt],
  gasLimit: Option[BigInt],
  gasPrice: Option[BigInt],
  nonce: Option[BigInt],
  data: Option[ByteString]) {

  def toTransaction(defaultNonce: BigInt): Transaction =
    Transaction(
      nonce = nonce.getOrElse(defaultNonce),
      gasPrice = gasPrice.getOrElse(2 * BigInt(10).pow(10)),
      gasLimit = gasLimit.getOrElse(BigInt(90000)),
      receivingAddress = to,
      value = value.getOrElse(BigInt(0)),
      payload = data.getOrElse(ByteString.empty)
    )
}
