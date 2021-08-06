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

/** https://eips.ethereum.org/EIPS/eip-155
  * EIP 155 includes (optionally) the chainId in transaction.v value
  */
class EIP155Signer(val chainId: BigInt) extends Signer {

  private val homesteadSigner = new HomesteadSigner
  private val valueForEmptyR = 0
  private val valueForEmptyS = 0
  private val negativePointSign = 27
  private val positivePointSign = 28
  private val chainIdProtectionOffset = 35
  private val chainIdMultiplier = 2

  override def signatureFromBytes(bytes65: ByteString): Option[ECDSASignature] = ???

  override def payloadToSign(tx: Transaction): Either[SignerError, Array[Byte]] =
    tx match {
      case legacy: LegacyTransaction => Right(payloadToSign(legacy))
      case unknownType =>
        Left(IncompatibleTransactionType(unknownType.getClass.getSimpleName, this.getClass.getSimpleName))
    }

  override def payloadToCheck(signedTx: SignedTransaction): Either[SignerError, Array[Byte]] =
    signedTx.tx match {
      case legacyTransaction: LegacyTransaction if isChainIdProtected(signedTx) =>
        Right(payloadToSign(legacyTransaction))
      case _: LegacyTransaction => homesteadSigner.payloadToCheck(signedTx)
      case unknownType =>
        Left(IncompatibleTransactionType(unknownType.getClass.getSimpleName, this.getClass.getSimpleName))
    }

  private def payloadToSign(tx: LegacyTransaction): Array[Byte] = {
    val receivingAddressAsArray: Array[Byte] = tx.receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray)
    crypto.kec256(
      rlpEncode(
        RLPList(
          tx.nonce,
          tx.gasPrice,
          tx.gasLimit,
          receivingAddressAsArray,
          tx.value,
          tx.payload,
          chainId,
          valueForEmptyR,
          valueForEmptyS
        )
      )
    )
  }

  override def getRAWSignature(signedTransaction: SignedTransaction): Either[SignerError, ECDSASignature] =
    signedTransaction.tx match {
      case _: LegacyTransaction if isChainIdProtected(signedTransaction) =>
        Right(getRAWSignature(signedTransaction.signature))
      case _: LegacyTransaction => homesteadSigner.getRAWSignature(signedTransaction)
      case unknownType =>
        Left(IncompatibleTransactionType(unknownType.getClass.getSimpleName, this.getClass.getSimpleName))
    }

  /** @param signedTransaction
    * @return true if the chainId is protected in the given signed transaction
    */
  private def isChainIdProtected(signedTransaction: SignedTransaction): Boolean =
    signedTransaction.signature.v != negativePointSign && signedTransaction.signature.v != positivePointSign

  /** Extract the raw signature according to the new chain id protection scheme
    * @param signature
    * @return the cryptographic raw signature
    */
  private def getRAWSignature(signature: ECDSASignature): ECDSASignature = {
    val ECDSASignature(r, s, v) = signature
    val yParity = (v - chainIdProtectionOffset - chainIdMultiplier * chainId).toInt
    ECDSASignature(r, s, yParity.toByte)
  }
}
