package io.iohk.ethereum.vmrunner

import akka.util.ByteString
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.vm.{Storage, WorldStateProxy}

case class WorldState(
  accounts: Map[Address, XAccount] = Map(),
  codeRepo: Map[ByteString, ByteString] = Map(),
  storages: Map[ByteString, Storage] = Map(),
  nonce: Long = 0
) extends WorldStateProxy {

  type WS = WorldState

  def getAccount(address: Address): Option[Account] =
    getXAccount(address).map(_.acc)

  def getCode(codeHash: ByteString): ByteString =
    codeRepo.getOrElse(codeHash, ByteString.empty)

  def getStorage(storageRoot: ByteString): Storage =
    storages.getOrElse(storageRoot, Storage.Empty)

  def saveAccount(address: Address, account: Account): WorldState = {
    getXAccount(address) match {
      case Some(xAcc) =>
        val abis = if (account.codeHash != xAcc.acc.codeHash) Nil else xAcc.abis
        val updated = xAcc.copy(acc = account, abis = abis)
        saveXAccount(address, updated)

      case None =>
        val newXAcc = XAccount(account, newName, address, Nil)
        saveXAccount(address, newXAcc).copy(nonce = nonce + 1)
    }
  }

  def saveCode(codeHash: ByteString, code: ByteString): WorldState =
    copy(codeRepo = codeRepo + (codeHash -> code))

  def saveStorage(storageRoot: ByteString, storage: Storage): WorldState =
    copy(storages = storages + (storageRoot -> storage))


  val accountsNames: Map[String, XAccount] =
    accounts.values.map(xAcc => xAcc.name -> xAcc).toMap

  def getXAccount(address: Address): Option[XAccount] =
    accounts.get(address)

  def getXAccount(name: String): Option[XAccount] =
    accountsNames.get(name)

  def saveXAccount(address: Address, xAccount: XAccount): WorldState =
    copy(accounts = accounts + (address -> xAccount))

  def listAccounts: List[XAccount] =
    accounts.values.toList

  def deleteAccount(address: Address): WorldState = {
    val acc = getAccount(address).getOrElse(Account.Empty)
    copy(
      accounts = accounts - address,
      codeRepo = codeRepo - acc.codeHash,
      storages = storages - acc.storageRoot
    )
  }

  private def newName: String =
    f"acc$nonce%03d"
}
