package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, Transaction}

case class TransactionRequest(
  from: Address,
  to: Option[Address] = None,
  value: Option[BigInt] = None,
  gasLimit: Option[BigInt] = None,
  gasPrice: Option[BigInt] = None,
  nonce: Option[BigInt] = None,
  data: Option[ByteString] = None) {

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
