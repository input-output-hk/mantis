package io.iohk.ethereum.domain

import akka.util.ByteString
import org.spongycastle.util.encoders.Hex


object Transaction {

  val NonceLength = 32
  val GasLength = 32
  val ValueLength = 32

  def apply(nonce: BigInt, gasPrice: BigInt, gasLimit: BigInt, receivingAddress: Address, value: BigInt, payload: ByteString): Transaction =
    Transaction(nonce, gasPrice, gasLimit, Some(receivingAddress), value, payload)


  /**
    * v0 ≡ Tg (Tx gas limit) * Tp (Tx gas price). See YP equation number (68)
    *
    * @param tx Target transaction
    * @return Upfront cost
    */
  def calculateUpfrontGas(tx: Transaction): UInt256 = UInt256(tx.gasLimit * tx.gasPrice)

  /**
    * v0 ≡ Tg (Tx gas limit) * Tp (Tx gas price) + Tv (Tx value). See YP equation number (65)
    *
    * @param tx Target transaction
    * @return Upfront cost
    */
  def calculateUpfrontCost(tx: Transaction): UInt256 =
    UInt256(calculateUpfrontGas(tx) + tx.value)
}

case class Transaction(
  nonce: BigInt,
  gasPrice: BigInt,
  gasLimit: BigInt,
  receivingAddress: Option[Address],
  value: BigInt,
  payload: ByteString) {

  def isContractInit: Boolean = receivingAddress.isEmpty

  override def toString: String = {
    s"""Transaction {
         |nonce: $nonce
         |gasPrice: $gasPrice
         |gasLimit: $gasLimit
         |receivingAddress: ${if(receivingAddress.isDefined) Hex.toHexString(receivingAddress.get.toArray) else "[Contract creation]"}
         |value: $value wei
         |payload: ${if (isContractInit) "ContractInit: " else "TransactionData: "}${Hex.toHexString(payload.toArray[Byte])}
         |}""".stripMargin
  }
}
