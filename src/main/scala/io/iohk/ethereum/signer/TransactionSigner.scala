package io.iohk.ethereum.signer

import akka.util.ByteString

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.domain.Transaction
import io.iohk.ethereum.utils.BlockchainConfig

trait TransactionSigner[T <: Transaction] {
  def chainId: BigInt
  def signatureFromBytes(bytes65: ByteString): Option[ECDSASignature]
  def getRAWSignature(signedTransaction: SignedTransaction): Either[SignerError, ECDSASignature]
  def payloadToSign(transaction: T): Either[SignerError, Array[Byte]]
  def calculateSender(signedTransaction: SignedTransaction): Option[Address]
}

object TransactionSigner {

  /** @param blockchainConfig
    * @param blockNumber
    * @return the proper signer corresponding to the block number
    */
  def get(blockchainConfig: BlockchainConfig, blockNumber: BigInt): Signer =
    // TODO should we explicitely handle berlin fork ?
    if (blockNumber >= blockchainConfig.forkBlockNumbers.magnetoBlockNumber) {
      new EIP2930Signer(blockchainConfig.chainId)
    } else if (blockNumber >= blockchainConfig.forkBlockNumbers.eip155BlockNumber) {
      new EIP155Signer(blockchainConfig.chainId)
    } else {
      new HomesteadSigner
    }

}
