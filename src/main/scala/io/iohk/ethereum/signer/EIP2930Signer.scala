package io.iohk.ethereum.signer

import akka.util.ByteString

import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.LegacyTransaction
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.domain.Transaction
import io.iohk.ethereum.domain.TransactionWithAccessList
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.accessListItemCodec
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}

/** https://eips.ethereum.org/EIPS/eip-2930
  * EIP 2930 introduces the new Transaction Type 1, with access list
  */
class EIP2930Signer(val chainId: BigInt) extends Signer {

  private val eip155Signer = new EIP155Signer(chainId)

  override def signatureFromBytes(bytes65: ByteString): Option[ECDSASignature] = ???

  override def payloadToCheck(signedTransaction: SignedTransaction): Either[SignerError, Array[Byte]] =
    // actually, payload to check is not required to include transaction type
    // (spec is mentioning SHOULD and not MUST)
    // but core-geth is taking the 'must include transaction type' path, so let's follow
    signedTransaction.tx match {
      case _: LegacyTransaction           => eip155Signer.payloadToCheck(signedTransaction)
      case tx1: TransactionWithAccessList => Right(payloadToSign(tx1))
      case unknownType =>
        Left(IncompatibleTransactionType(unknownType.getClass.getSimpleName, this.getClass.getSimpleName))
    }

  override def payloadToSign(tx: Transaction): Either[SignerError, Array[Byte]] =
    tx match {
      case _: LegacyTransaction           => eip155Signer.payloadToSign(tx)
      case tx1: TransactionWithAccessList => Right(payloadToSign(tx1))
      case unknownType =>
        Left(IncompatibleTransactionType(unknownType.getClass.getSimpleName, this.getClass.getSimpleName))
    }

  override def getRAWSignature(signedTransaction: SignedTransaction): Either[SignerError, ECDSASignature] =
    signedTransaction.tx match {
      case _: LegacyTransaction         => eip155Signer.getRAWSignature(signedTransaction)
      case _: TransactionWithAccessList => Right(signedTransaction.signature)
      case unknownType =>
        Left(IncompatibleTransactionType(unknownType.getClass.getSimpleName, this.getClass.getSimpleName))
    }

  private def payloadToSign(tx: TransactionWithAccessList): Array[Byte] = {
    val receivingAddressAsArray: Array[Byte] = tx.receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray)
    crypto.kec256(
      Transaction.Type01 +: rlpEncode(
        RLPList(
          chainId,
          tx.nonce,
          tx.gasPrice,
          tx.gasLimit,
          receivingAddressAsArray,
          tx.value,
          tx.payload,
          toRlpList(tx.accessList)
        )
      )
    )
  }
}
