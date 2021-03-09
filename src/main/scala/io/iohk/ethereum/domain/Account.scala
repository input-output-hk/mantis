package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.mpt.ByteArraySerializable
import io.iohk.ethereum.network.p2p.messages.PV63.AccountImplicits
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicits._
import org.bouncycastle.util.encoders.Hex

import scala.util.Try

object Account {
  val EmptyStorageRootHash: ByteString = ByteString(kec256(rlp.encode(Array.empty[Byte])))
  val EmptyCodeHash: ByteString = kec256(ByteString())

  def empty(startNonce: UInt256 = UInt256.Zero): Account =
    Account(nonce = startNonce, storageRoot = EmptyStorageRootHash, codeHash = EmptyCodeHash)

  implicit val accountSerializer: ByteArraySerializable[Account] = new ByteArraySerializable[Account] {

    import AccountImplicits._

    override def fromBytes(bytes: Array[Byte]): Account = bytes.toAccount

    override def toBytes(input: Account): Array[Byte] = input.toBytes
  }

  def apply(bytes: ByteString): Try[Account] = Try(accountSerializer.fromBytes(bytes.toArray))
}

case class Account(
    nonce: UInt256 = 0,
    balance: UInt256 = 0,
    storageRoot: ByteString = Account.EmptyStorageRootHash,
    codeHash: ByteString = Account.EmptyCodeHash
) {

  def increaseBalance(value: UInt256): Account =
    copy(balance = balance + value)

  def increaseNonce(value: UInt256 = 1): Account =
    copy(nonce = nonce + value)

  def withCode(codeHash: ByteString): Account =
    copy(codeHash = codeHash)

  def withStorage(storageRoot: ByteString): Account =
    copy(storageRoot = storageRoot)

  /** According to EIP161: An account is considered empty when it has no code and zero nonce and zero balance.
    * An account's storage is not relevant when determining emptiness.
    */
  def isEmpty(startNonce: UInt256 = UInt256.Zero): Boolean =
    nonce == startNonce && balance == UInt256.Zero && codeHash == Account.EmptyCodeHash

  /** Under EIP-684 if this evaluates to true then we have a conflict when creating a new account
    */
  def nonEmptyCodeOrNonce(startNonce: UInt256 = UInt256.Zero): Boolean =
    nonce != startNonce || codeHash != Account.EmptyCodeHash

  override def toString: String =
    s"Account(nonce: $nonce, balance: $balance, " +
      s"storageRoot: ${Hex.toHexString(storageRoot.toArray[Byte])}, codeHash: ${Hex.toHexString(codeHash.toArray[Byte])})"

}
