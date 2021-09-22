package io.iohk.ethereum.domain

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex

sealed trait Transaction extends Product with Serializable {
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
  val Type01: Byte = 1.toByte

  val MinAllowedType: Byte = 0
  val MaxAllowedType: Byte = 0x7f

  val LegacyThresholdLowerBound: Int = 0xc0
  val LegacyThresholdUpperBound: Int = 0xfe

  def withGasLimit(gl: BigInt): Transaction => Transaction = {
    case tx: LegacyTransaction         => tx.copy(gasLimit = gl)
    case tx: TransactionWithAccessList => tx.copy(gasLimit = gl)
  }

  def accessList(tx: Transaction): List[AccessListItem] =
    tx match {
      case transaction: TransactionWithAccessList                                         => transaction.accessList
      case LegacyTransaction(nonce, gasPrice, gasLimit, receivingAddress, value, payload) => Nil
    }

  implicit class TransactionTypeValidator(val transactionType: Byte) extends AnyVal {
    def isValidTransactionType: Boolean = transactionType >= MinAllowedType && transactionType <= MaxAllowedType
  }

  implicit class ByteArrayTransactionTypeValidator(val binaryData: Array[Byte]) extends AnyVal {
    def isValidTransactionType: Boolean = binaryData.length == 1 && binaryData.head.isValidTransactionType
  }
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

object TransactionWithAccessList {
  def apply(
      chainId: BigInt,
      nonce: BigInt,
      gasPrice: BigInt,
      gasLimit: BigInt,
      receivingAddress: Address,
      value: BigInt,
      payload: ByteString,
      accessList: List[AccessListItem]
  ): TransactionWithAccessList =
    TransactionWithAccessList(chainId, nonce, gasPrice, gasLimit, Some(receivingAddress), value, payload, accessList)
}

case class TransactionWithAccessList(
    chainId: BigInt,
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
