package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.{Account, Address, BlockHeader}

case class MockWorldState(
  accounts: Map[Address, Account] = Map(),
  codeRepo: Map[ByteString, ByteString] = Map(),
  storages: Map[ByteString, Storage] = Map(),
  headers: Map[BigInt, BlockHeader] = Map()
) extends WorldStateProxy {
  type WS = MockWorldState

  def getAccount(address: Address): Option[Account] =
    accounts.get(address)

  def getCode(codeHash: ByteString): ByteString =
    codeRepo.getOrElse(codeHash, ByteString.empty)

  def getStorage(storageRoot: ByteString): Storage =
    storages.getOrElse(storageRoot, Storage.Empty)

  def getBlockHeader(number: BigInt): Option[BlockHeader] =
    headers.get(number)

  def saveAccount(address: Address, account: Account): MockWorldState =
    copy(accounts = accounts + (address -> account))

  def saveCode(codeHash: ByteString, code: ByteString): MockWorldState =
    copy(codeRepo = codeRepo + (codeHash -> code))

  def saveStorage(storageRoot: ByteString, storage: Storage): MockWorldState =
    copy(storages = storages + (storageRoot -> storage))
}
