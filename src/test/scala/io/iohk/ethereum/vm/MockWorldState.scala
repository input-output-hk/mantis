package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256

object MockWorldState {
  type TestVM = VM[MockWorldState, MockStorage]
  type PS = ProgramState[MockWorldState, MockStorage]
  type PC = ProgramContext[MockWorldState, MockStorage]
  type PR = ProgramResult[MockWorldState, MockStorage]
}

case class MockWorldState(
    accounts: Map[Address, Account] = Map(),
    codeRepo: Map[Address, ByteString] = Map(),
    storages: Map[Address, MockStorage] = Map(),
    numberOfHashes: UInt256 = 0,
    touchedAccounts: Set[Address] = Set.empty,
    noEmptyAccountsCond: Boolean = false
) extends WorldStateProxy[MockWorldState, MockStorage] {

  def getAccount(address: Address): Option[Account] =
    accounts.get(address)

  override def getGuaranteedAccount(address: Address): Account =
    super.getGuaranteedAccount(address)

  def saveAccount(address: Address, account: Account): MockWorldState =
    copy(accounts = accounts + (address -> account))

  def deleteAccount(address: Address): MockWorldState =
    copy(accounts = accounts - address, codeRepo - address, storages - address)

  def getCode(address: Address): ByteString =
    codeRepo.getOrElse(address, ByteString.empty)

  def getStorage(address: Address): MockStorage =
    storages.getOrElse(address, MockStorage.Empty)

  def getBlockHash(number: UInt256): Option[UInt256] =
    if (numberOfHashes >= number && number >= 0)
      Some(UInt256(kec256(number.toString.getBytes)))
    else
      None

  def saveCode(address: Address, code: ByteString): MockWorldState =
    if (code.isEmpty)
      copy(codeRepo = codeRepo - address)
    else
      copy(codeRepo = codeRepo + (address -> code))

  def saveStorage(address: Address, storage: MockStorage): MockWorldState =
    if (storage.isEmpty)
      copy(storages = storages - address)
    else
      copy(storages = storages + (address -> storage))

  def getEmptyAccount: Account = Account.empty()

  override def touchAccounts(addresses: Address*): MockWorldState =
    if (noEmptyAccounts)
      copy(touchedAccounts = touchedAccounts ++ addresses.toSet)
    else
      this

  def clearTouchedAccounts: MockWorldState =
    copy(touchedAccounts = touchedAccounts.empty)

  def noEmptyAccounts: Boolean = noEmptyAccountsCond

  override def keepPrecompileTouched(world: MockWorldState): MockWorldState =
    if (world.touchedAccounts.contains(ripmdContractAddress))
      copy(touchedAccounts = touchedAccounts + ripmdContractAddress)
    else
      this
}
