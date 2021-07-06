package io.iohk.ethereum.jsonrpc

import akka.util.ByteString

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.LegacyTransaction
import io.iohk.ethereum.utils.Config

case class TransactionRequest(
    from: Address,
    to: Option[Address] = None,
    value: Option[BigInt] = None,
    gasLimit: Option[BigInt] = None,
    gasPrice: Option[BigInt] = None,
    nonce: Option[BigInt] = None,
    data: Option[ByteString] = None
) {

  private val defaultGasPrice: BigInt = 2 * BigInt(10).pow(10)
  private val defaultGasLimit: BigInt = 90000

  def toTransaction(defaultNonce: BigInt): LegacyTransaction =
    LegacyTransaction(
      nonce = nonce.getOrElse(defaultNonce),
      gasPrice = gasPrice.getOrElse(defaultGasPrice),
      gasLimit = gasLimit.getOrElse(defaultGasLimit),
      receivingAddress = if (Config.testmode) to.filter(_ != Address(0)) else to,
      value = value.getOrElse(BigInt(0)),
      payload = data.getOrElse(ByteString.empty)
    )
}

case class IeleTransactionRequest(
    from: Address,
    to: Option[Address] = None,
    value: Option[BigInt] = None,
    gasLimit: Option[BigInt] = None,
    gasPrice: Option[BigInt] = None,
    nonce: Option[BigInt] = None,
    function: Option[String] = None,
    arguments: Option[Seq[ByteString]] = None,
    contractCode: Option[ByteString]
)
