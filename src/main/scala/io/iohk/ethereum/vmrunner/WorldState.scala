package io.iohk.ethereum.vmrunner

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.vm._

object WorldState {
  type PS = ProgramState[WorldState, MapStorage]
  type PC = ProgramContext[WorldState, MapStorage]
  type PR = ProgramResult[WorldState, MapStorage]
}

case class WorldState(
  accounts: Map[Address, XAccount] = Map(),
  codeRepo: Map[Address, ByteString] = Map(),
  storages: Map[Address, MapStorage] = Map(),
  nonce: Long = 0
) extends WorldStateProxy[WorldState, MapStorage] {

  type WS = WorldState

  def getAccount(address: Address): Option[Account] =
    getXAccount(address).map(_.acc)


  override def getGuaranteedAccount(address: Address): Account =
    super.getGuaranteedAccount(address)

  def getCode(address: Address): ByteString =
    codeRepo.getOrElse(address, ByteString.empty)

  def getStorage(address: Address): MapStorage =
    storages.getOrElse(address, MapStorage.Empty)

  def saveAccount(address: Address, account: Account): WorldState = {
    getXAccount(address) match {
      case Some(xAcc) =>
        //val abis = if (account.address != xAcc.acc.address) Nil else xAcc.abis
        val updated = xAcc.copy(acc = account/*, abis = abis*/)
        saveXAccount(address, updated)

      case None =>
        val newXAcc = XAccount(account, newName, address, Nil)
        saveXAccount(address, newXAcc).copy(nonce = nonce + 1)
    }
  }

  def saveCode(address: Address, code: ByteString): WorldState =
    copy(codeRepo = codeRepo + (address -> code))

  def saveStorage(address: Address, storage: MapStorage): WorldState =
    copy(storages = storages + (address -> storage))


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

  def deleteAccount(address: Address): WorldState =
    copy(
      accounts = accounts - address,
      codeRepo = codeRepo - address,
      storages = storages - address
    )

  private def newName: String =
    f"acc$nonce%03d"

  def getBlockHash(number: UInt256): Option[UInt256] =
    Some(UInt256(kec256(number.bytes)))
}
