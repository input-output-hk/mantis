package io.iohk.ethereum.vmrunner

import akka.util.ByteString
import akka.util.ByteString.{empty => BEmpty}
import io.iohk.ethereum.domain.{Address, BlockHeader, SignedTransaction, Transaction}

object MockVmInput {

  class MockTransaction(
    tx: Transaction,
    senderAddress: Address,
    pointSign: Byte = 0,
    signatureRandom: ByteString = BEmpty,
    signature: ByteString = BEmpty
  ) extends SignedTransaction(tx, pointSign, signatureRandom, signature, senderAddress, pointSign)

  val defaultGasPrice: BigInt = 1000

  def transaction(
    senderAddress: Address,
    payload: ByteString,
    value: BigInt,
    gasLimit: BigInt,
    gasPrice: BigInt = defaultGasPrice,
    receivingAddress: Address = Address.empty,
    nonce: BigInt = 0
  ): SignedTransaction =
    new MockTransaction(Transaction(nonce, gasPrice, gasLimit, receivingAddress, value, payload), senderAddress)


  def blockHeader: BlockHeader =
    BlockHeader(BEmpty, BEmpty, BEmpty, BEmpty, BEmpty, BEmpty, BEmpty, 0, 0, 0, 0, 0, BEmpty, BEmpty, BEmpty)

}
