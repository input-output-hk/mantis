package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.vm.UInt256

object Account {
  val EmptyStorageRootHash = ByteString(kec256(rlp.encode(Array.empty[Byte])))
  val EmptyCodeHash: ByteString = kec256(ByteString())

  val Empty = Account(0, 0, EmptyStorageRootHash, EmptyCodeHash)
}

case class Account(
  nonce: UInt256 = 0,
  balance: UInt256 = 0,
  storageRoot: ByteString = Account.EmptyStorageRootHash,
  codeHash: ByteString = Account.EmptyCodeHash) {

  def updateBalance(value: UInt256): Account =
    copy(balance = balance + value)

  def increaseNonce: Account =
    copy(nonce = nonce + 1)

  def withCode(codeHash: ByteString): Account =
    copy(codeHash = codeHash)

  def withStorage(storageRoot: ByteString): Account =
    copy(storageRoot = storageRoot)
}
