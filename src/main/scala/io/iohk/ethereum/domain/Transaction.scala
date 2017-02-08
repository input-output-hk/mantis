package io.iohk.ethereum.domain

import akka.util.ByteString
import org.spongycastle.util.encoders.Hex


object Transaction {

  val NonceLength = 32
  val GasLength = 32
  val ValueLength = 32

}

case class Transaction(
  nonce: BigInt,
  gasPrice: BigInt,
  gasLimit: BigInt,
  receivingAddress: Address,
  value: BigInt,
  payload: ByteString) {

  def isContractInit: Boolean = receivingAddress.isEmpty

  override def toString: String = {
    s"""Transaction {
         |nonce: $nonce
         |gasPrice: $gasPrice
         |gasLimit: $gasLimit
         |receivingAddress: ${Hex.toHexString(receivingAddress.toArray)}
         |value: $value wei
         |payload: ${if (isContractInit) "ContractInit: " else "TransactionData: "}${Hex.toHexString(payload.toArray[Byte])}
         |}""".stripMargin
  }
}
