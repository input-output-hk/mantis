package io.iohk.ethereum.domain

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex

object LegacyTransaction {

  val NonceLength = 32
  val GasLength = 32
  val ValueLength = 32

  def apply(
      nonce: BigInt,
      gasPrice: BigInt,
      gasLimit: BigInt,
      receivingAddress: Address,
      value: BigInt,
      payload: ByteString
  ): LegacyTransaction =
    LegacyTransaction(nonce, gasPrice, gasLimit, Some(receivingAddress), value, payload)

}

case class LegacyTransaction(
    nonce: BigInt,
    gasPrice: BigInt,
    gasLimit: BigInt,
    receivingAddress: Option[Address],
    value: BigInt,
    payload: ByteString
) {

  def isContractInit: Boolean = receivingAddress.isEmpty

  override def toString: String = {
    val receivingAddressString =
      receivingAddress.map(addr => Hex.toHexString(addr.toArray)).getOrElse("[Contract creation]")

    s"LegacyTransaction {" +
      s"nonce: $nonce " +
      s"gasPrice: $gasPrice " +
      s"gasLimit: $gasLimit " +
      s"receivingAddress: $receivingAddressString " +
      s"value: $value wei " +
      s"payload: ${if (isContractInit) "ContractInit: " else "TransactionData: "}${Hex.toHexString(payload.toArray[Byte])} " +
      s"}"
  }
}
