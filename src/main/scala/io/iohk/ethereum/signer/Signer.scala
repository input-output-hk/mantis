package io.iohk.ethereum.signer

import akka.util.ByteString

import scala.util.Try

import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.domain.SignedTransaction.FirstByteOfAddress
import io.iohk.ethereum.domain.SignedTransaction.LastByteOfAddress
import io.iohk.ethereum.domain.Transaction
import io.iohk.ethereum.utils.BlockchainConfig

/** Signature import and transaction signing depends on:
  * - implementation change (EIP)
  * - transaction type
  * The Signer trait describe the expected signer's behaviour.
  */
trait Signer {
  def chainId: BigInt

  /** Import a signature from its binary representation.
    * The binary representation is R || S || V, with notably V being the cryptographic
    * parity (valid values are 0 or 1).
    * The ECDSASignature build from this are is adapted to fit the ethereum semantic for V.
    * For example, with a pre-eip155 transaction, signedTransaction.v = binary.v + 27
    * @param bytes65
    * @return
    */
  def signatureFromBytes(bytes65: ByteString): Option[ECDSASignature]

  /** A transaction signature is done on a subset of its data.
    * This method build this binary data.
    * The difference with #payloadToCheck is that payloadToSign is used when building a signed transaction,
    * whereas payloadToCheck is used when checking a existing signed transaction.
    * As such, for forks with multiple possible behavior, this method can arbitrarily
    * choose the one used.
    * @param tx the input transaction
    * @return the binary data representing the payload to sign, or a SignerError
    */
  def payloadToSign(tx: Transaction): Either[SignerError, Array[Byte]]

  /** Signature is check against a transaction payload.
    * This method build this binary data.
    * The difference with #payloadToSign is that payloadToSign is used when building a signed transaction,
    * whereas payloadToCheck is used when checking a existing signed transaction
    * As such, for forks with multiple possible behavior, this method try to detect
    * the one used by the client generating this signed transaction.
    * @param signedTx the signed transaction to check
    * @return the binary data representing the payload to check, or a SignedError
    */
  def payloadToCheck(signedTx: SignedTransaction): Either[SignerError, Array[Byte]]

  /** Extract the sender address from the signed transaction
    * @param signedTransaction the signed transaction
    * @return the sender address if possible
    */
  def calculateSender(signedTransaction: SignedTransaction): Option[Address] = Try {
    val ECDSASignature(_, _, v) = signedTransaction.signature

    for {
      bytesToSign <- payloadToCheck(signedTransaction).toOption
      key <- signedTransaction.signature.publicKey(bytesToSign, Some(chainId.toByte))
      addrBytes = crypto.kec256(key).slice(FirstByteOfAddress, LastByteOfAddress)
      if addrBytes.length == Address.Length
    } yield Address(addrBytes)
  }.toOption.flatten

  /** Return the raw cryptographic signature, independent on the SignedTransaction encoding.
    * It's allow to get the cryptographic signature from a received SignedTransaction.
    *
    * Signed transaction signature's fields are not always directly the ECDSA fields.
    * On some forks, advanced rules are used to build them.
    * It's particularly true for the v field (the recovery field).
    *
    * @param signedTransaction
    * @return the raw crypto signature if available, or an error if not
    */
  def getRAWSignature(signedTransaction: SignedTransaction): Either[SignerError, ECDSASignature]
}

object Signer {

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
