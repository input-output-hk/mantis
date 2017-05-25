package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{Account, Address}

object MockWorldState {
  type PS = ProgramState[MockWorldState, MockStorage]
  type PC = ProgramContext[MockWorldState, MockStorage]
  type PR = ProgramResult[MockWorldState, MockStorage]
}

case class MockWorldState(
  accounts: Map[Address, Account] = Map(),
  codeRepo: Map[Address, ByteString] = Map(),
  storages: Map[Address, MockStorage] = Map(),
  numberOfHashes: UInt256 = 0
) extends WorldStateProxy[MockWorldState, MockStorage] {

  def getAccount(address: Address): Option[Account] =
    accounts.get(address)

  override def getGuaranteedAccount(address: Address): Account =
    super.getGuaranteedAccount(address)

  def saveAccount(address: Address, account: Account): MockWorldState =
    copy(accounts = accounts + (address -> account))

  def deleteAccount(address: Address): MockWorldState =
    copy(accounts = accounts - address)

  def getCode(address: Address): ByteString =
    codeRepo.getOrElse(address, ByteString.empty)

  def getStorage(address: Address): MockStorage =
    storages.getOrElse(address, MockStorage.Empty)

  def getBlockHash(number: UInt256): Option[UInt256] =
    if (numberOfHashes >= number && number >= 0)
      Some(UInt256(kec256(number.bytes.toArray)))
    else
      None

  def saveCode(address: Address, code: ByteString): MockWorldState =
    copy(codeRepo = codeRepo + (address -> code))

  def saveStorage(address: Address, storage: MockStorage): MockWorldState =
    copy(storages = storages + (address -> storage))
}
