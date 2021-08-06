package io.iohk.ethereum.signer

import akka.util.ByteString

import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.LegacyTransaction
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.domain.Transaction
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}

class HomesteadSigner extends Signer {

  private val yParityOffset = 27.toByte

  override def chainId: BigInt = 0

  override def signatureFromBytes(bytes65: ByteString): Option[ECDSASignature] = ???

  override def payloadToSign(tx: Transaction): Either[SignerError, Array[Byte]] =
    tx match {
      case legacy: LegacyTransaction => Right(payloadToSign(legacy))
      case unknownType =>
        Left(IncompatibleTransactionType(unknownType.getClass.getSimpleName, this.getClass.getSimpleName))
    }

  override def payloadToCheck(signedTx: SignedTransaction): Either[SignerError, Array[Byte]] = payloadToSign(
    signedTx.tx
  )

  override def getRAWSignature(signedTransaction: SignedTransaction): Either[SignerError, ECDSASignature] =
    signedTransaction.tx match {
      case _: LegacyTransaction =>
        Right(getRAWSignature(signedTransaction.signature))
      case unknownType =>
        Left(IncompatibleTransactionType(unknownType.getClass.getSimpleName, this.getClass.getSimpleName))
    }

  private def payloadToSign(legacyTransaction: LegacyTransaction): Array[Byte] = {
    val receivingAddressAsArray: Array[Byte] =
      legacyTransaction.receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray)
    crypto.kec256(
      rlpEncode(
        RLPList(
          legacyTransaction.nonce,
          legacyTransaction.gasPrice,
          legacyTransaction.gasLimit,
          receivingAddressAsArray,
          legacyTransaction.value,
          legacyTransaction.payload
        )
      )
    )
  }

  /** Extract the raw signature according to the unprotected chain id scheme
    * @param signature
    * @return the cryptographic raw signature
    */
  private def getRAWSignature(signature: ECDSASignature): ECDSASignature = {
    val ECDSASignature(r, s, v) = signature
    val yParity = v - yParityOffset
    ECDSASignature(r, s, yParity.toByte)
  }

}
