package io.iohk.ethereum.domain

import akka.util.ByteString

object Account {
  val Empty = Account(0, 0, ByteString.empty, ByteString.empty)
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
