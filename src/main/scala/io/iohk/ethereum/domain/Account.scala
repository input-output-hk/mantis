package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.mpt.ByteArraySerializable
import io.iohk.ethereum.network.p2p.messages.PV63.AccountImplicits
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicits._
import org.spongycastle.util.encoders.Hex

object Account {
  val EmptyStorageRootHash = ByteString(kec256(rlp.encode(Array.empty[Byte])))
  val EmptyCodeHash: ByteString = kec256(ByteString())

  def empty(startNonce: UInt256 = UInt256.Zero): Account = Account(nonce = startNonce, storageRoot = EmptyStorageRootHash, codeHash = EmptyCodeHash)

  implicit val accountSerializer = new ByteArraySerializable[Account] {

    import AccountImplicits._

    override def fromBytes(bytes: Array[Byte]): Account = bytes.toAccount

    override def toBytes(input: Account): Array[Byte] = input.toBytes
  }
}

case class Account(
  nonce: UInt256 = 0,
  balance: UInt256 = 0,
  storageRoot: ByteString = Account.EmptyStorageRootHash,
  codeHash: ByteString = Account.EmptyCodeHash) {

  def increaseBalance(value: UInt256): Account =
    copy(balance = balance + value)

  def increaseNonce(value: UInt256 = 1): Account =
    copy(nonce = nonce + value)

  def withCode(codeHash: ByteString): Account =
    copy(codeHash = codeHash)

  def withStorage(storageRoot: ByteString): Account =
    copy(storageRoot = storageRoot)

  def resetAccountPreservingBalance(startNonce: UInt256 = UInt256.Zero): Account =
    copy(nonce = startNonce, storageRoot = Account.EmptyStorageRootHash, codeHash = Account.EmptyCodeHash)

  /**
    * According to EIP161: An account is considered empty when it has no code and zero nonce and zero balance.
    */
  def isEmpty: Boolean =
    nonce == UInt256.Zero && balance == UInt256.Zero && codeHash == Account.EmptyCodeHash

  override def toString: String =
    s"Account(nonce: $nonce, balance: $balance, " +
      s"storageRoot: ${Hex.toHexString(storageRoot.toArray[Byte])}, codeHash: ${Hex.toHexString(codeHash.toArray[Byte])})"

}
