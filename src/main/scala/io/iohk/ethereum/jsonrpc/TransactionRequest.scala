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

  private val defaultGasPrice: BigInt = 2 * BigInt(10).pow(10)
  private val defaultGasLimit: BigInt = 90000

  def toTransaction(defaultNonce: BigInt): Transaction =
    Transaction(
      nonce = nonce.getOrElse(defaultNonce),
      gasPrice = gasPrice.getOrElse(defaultGasPrice),
      gasLimit = gasLimit.getOrElse(defaultGasLimit),
      receivingAddress = to,
      value = value.getOrElse(BigInt(0)),
      payload = data.getOrElse(ByteString.empty)
    )
}
