package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicits._

object Account {
  val EmptyStorageRootHash = ByteString(kec256(rlp.encode(Array.empty[Byte])))
  val EmptyCodeHash = kec256(ByteString())

  val Empty = Account(0, 0, EmptyStorageRootHash, EmptyCodeHash)
}

case class Account(
  nonce: BigInt,
  balance: BigInt,
  storageRoot: ByteString,
  codeHash: ByteString) {

  def updateBalance(value: BigInt): Account =
    copy(balance = balance + value)

  def increaseNonce: Account =
    copy(nonce = nonce + 1)

  def withCode(codeHash: ByteString): Account =
    copy(codeHash = codeHash)

  def withStorage(storageRoot: ByteString): Account =
    copy(storageRoot = storageRoot)
}
