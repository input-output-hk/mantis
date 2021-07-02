package io.iohk.ethereum.extvm

import akka.util.ByteString

import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.vm

object World {
  def apply(accountStartNonce: UInt256, noEmptyAccountsCond: Boolean, messageHandler: MessageHandler): World =
    World(
      accountStartNonce = accountStartNonce,
      noEmptyAccountsCond = noEmptyAccountsCond,
      accountCache = new AccountCache(messageHandler),
      storageCache = new StorageCache(messageHandler),
      codeCache = new CodeCache(messageHandler),
      blockhashCache = new BlockhashCache(messageHandler)
    )
}

case class World(
    override val accountStartNonce: UInt256,
    noEmptyAccountsCond: Boolean,
    accountCache: AccountCache,
    storageCache: StorageCache,
    codeCache: CodeCache,
    blockhashCache: BlockhashCache,
    accounts: Map[Address, Account] = Map(),
    storages: Map[Address, Storage] = Map(),
    codeRepo: Map[Address, ByteString] = Map(),
    touchedAccounts: Set[Address] = Set()
) extends vm.WorldStateProxy[World, Storage] {

  def getAccount(address: Address): Option[Account] =
    accounts.get(address).orElse(accountCache.getAccount(address))

  def saveAccount(address: Address, account: Account): World =
    copy(accounts = accounts + (address -> account))

  protected def deleteAccount(address: Address): World =
    copy(accounts = accounts - address)

  def getEmptyAccount: Account = Account.empty(accountStartNonce)

  def touchAccounts(addresses: Address*): World =
    copy(touchedAccounts = touchedAccounts ++ addresses)

  protected def clearTouchedAccounts: World =
    copy(touchedAccounts = Set.empty)

  protected def noEmptyAccounts: Boolean = noEmptyAccountsCond

  override def keepPrecompileTouched(world: World): World =
    if (world.touchedAccounts.contains(ripmdContractAddress))
      copy(touchedAccounts = touchedAccounts + ripmdContractAddress)
    else
      this
  def getCode(address: Address): ByteString =
    codeRepo.getOrElse(address, codeCache.getCode(address))

  def getStorage(address: Address): Storage =
    storages.getOrElse(address, new Storage(address, Map.empty, storageCache))

  def getBlockHash(offset: UInt256): Option[UInt256] =
    blockhashCache.getBlockhash(offset)

  def saveCode(address: Address, code: ByteString): World =
    copy(codeRepo = codeRepo + (address -> code))

  def saveStorage(address: Address, storage: Storage): World =
    copy(storages = storages + (address -> storage))
}
