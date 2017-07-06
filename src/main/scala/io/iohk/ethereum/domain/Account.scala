package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.mpt.ByteArraySerializable
import io.iohk.ethereum.network.p2p.messages.PV63.AccountImplicits
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.vm.UInt256
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

  def increaseNonce: Account =
    copy(nonce = nonce + 1)

  def withCode(codeHash: ByteString): Account =
    copy(codeHash = codeHash)

  def withStorage(storageRoot: ByteString): Account =
    copy(storageRoot = storageRoot)

  override def toString: String =
    s"Account(nonce: $nonce, balance: $balance, " +
      s"storageRoot: ${Hex.toHexString(storageRoot.toArray[Byte])}, codeHash: ${Hex.toHexString(codeHash.toArray[Byte])})"

}
