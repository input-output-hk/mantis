package io.iohk.ethereum.domain

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex

sealed trait Transaction {
  def nonce: BigInt
  def gasPrice: BigInt
  def gasLimit: BigInt
  def receivingAddress: Option[Address]
  def value: BigInt
  def payload: ByteString

  def isContractInit: Boolean = receivingAddress.isEmpty

  protected def receivingAddressString: String =
    receivingAddress.map(_.toString).getOrElse("[Contract creation]")

  protected def payloadString: String =
    s"${if (isContractInit) "ContractInit: " else "TransactionData: "}${Hex.toHexString(payload.toArray[Byte])}"
}

object Transaction {
  val Type01: Int = 1
  val LegacyThresholdLowerBound: Int = 0xc0
  val LegacyThresholdUpperBound: Int = 0xfe
}

sealed trait TypedTransaction extends Transaction

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
) extends Transaction {
  override def toString: String =
    s"LegacyTransaction {" +
      s"nonce: $nonce " +
      s"gasPrice: $gasPrice " +
      s"gasLimit: $gasLimit " +
      s"receivingAddress: $receivingAddressString " +
      s"value: $value wei " +
      s"payload: $payloadString " +
      s"}"
}

case class TransactionWithAccessList(
    nonce: BigInt,
    gasPrice: BigInt,
    gasLimit: BigInt,
    receivingAddress: Option[Address],
    value: BigInt,
    payload: ByteString,
    accessList: List[AccessListItem]
) extends TypedTransaction {
  override def toString: String =
    s"TransactionWithAccessList {" +
      s"nonce: $nonce " +
      s"gasPrice: $gasPrice " +
      s"gasLimit: $gasLimit " +
      s"receivingAddress: $receivingAddressString " +
      s"value: $value wei " +
      s"payload: $payloadString " +
      s"accessList: $accessList" +
      s"}"
}

case class AccessListItem(address: Address, storageKeys: List[BigInt]) // bytes32
