package io.iohk.ethereum.vm.utils

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Address, BlockHeader, SignedTransaction, Transaction}
import io.iohk.ethereum.Fixtures.{Blocks => BlockFixtures}

object MockVmInput {

  class MockTransaction(
      tx: Transaction,
      senderAddress: Address,
      pointSign: Byte = 0,
      signatureRandom: BigInt = 0,
      signature: BigInt = 0
  ) extends SignedTransaction(
        tx,
        ECDSASignature(v = pointSign, r = signatureRandom.bigInteger, s = signature.bigInteger)
      )

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

  def blockHeader: BlockHeader = BlockFixtures.ValidBlock.header

}
