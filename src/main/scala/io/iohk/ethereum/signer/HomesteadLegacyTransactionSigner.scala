package io.iohk.ethereum.signer

import akka.util.ByteString

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.LegacyTransaction
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.domain.Transaction

class HomesteadLegacyTransactionSigner extends TransactionSigner[LegacyTransaction] {
  override def chainId: BigInt = ???

  override def signatureFromBytes(bytes65: ByteString): Option[ECDSASignature] = ???

  override def getRAWSignature(signedTransaction: SignedTransaction): Either[SignerError, ECDSASignature] = ???

  override def payloadToSign(tx: LegacyTransaction): Either[SignerError, Array[Byte]] = ???

  override def calculateSender(signedTransaction: SignedTransaction): Option[Address] = ???
}
