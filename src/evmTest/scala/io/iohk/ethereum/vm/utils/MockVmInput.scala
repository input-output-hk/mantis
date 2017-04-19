package io.iohk.ethereum.vm.utils

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Address, BlockHeader, SignedTransaction, Transaction}
import akka.util.ByteString.{empty => BEmpty}

/**
  * Created by adam on 19.04.2017.
  */
object MockVmInput {

  class MockTransaction(
    tx: Transaction,
    senderAddress: Address,
    pointSign: Byte = 0,
    signatureRandom: BigInt = 0,
    signature: BigInt = 0
  ) extends SignedTransaction(tx, ECDSASignature(v = pointSign, r = signatureRandom.bigInteger, s = signature.bigInteger), senderAddress)

  val defaultGasPrice: BigInt = 1000

  def transaction(
    senderAddress: Address,
    payload: ByteString,
    value: BigInt,
    gasLimit: BigInt,
    gasPrice: BigInt = defaultGasPrice,
    receivingAddress: Option[Address] = None,
    nonce: BigInt = 0
  ): SignedTransaction =
    new MockTransaction(Transaction(nonce, gasPrice, gasLimit, receivingAddress, value, payload), senderAddress)


  def blockHeader: BlockHeader =
    BlockHeader(BEmpty, BEmpty, BEmpty, BEmpty, BEmpty, BEmpty, BEmpty, 0, 0, 0, 0, 0, BEmpty, BEmpty, BEmpty)

}
