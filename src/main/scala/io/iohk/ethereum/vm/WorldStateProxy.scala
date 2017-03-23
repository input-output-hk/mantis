package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.crypto.kec256

/**
  * This is a single entry point to all VM interactions with the persisted state. Implementations are meant to be
  * immutable so that rolling back a transaction is equivalent to discarding resulting changes. The changes to state
  * should be kept in memory and applied only after a transaction completes without errors. This does not forbid mutable
  * caches for DB retrieval operations.
  */
trait WorldStateProxy[WS <: WorldStateProxy[WS, S], S <: Storage[S]] {

  protected def getAccount(address: Address): Option[Account]
  protected def saveAccount(address: Address, account: Account): WS

  /**
    * In certain situation an account is guaranteed to exist, e.g. the account that executes the code, the account that
    * transfer value to another. There could be no input to our application that would cause this fail, so we shouldn't
    * handle account existence in such cases. If it does fail, it means there's something terribly wrong with our code
    * and throwing an exception is an appropriate response.
    */
  protected def getGuaranteedAccount(address: Address): Account =
    getAccount(address).get

  def getCode(address: Address): ByteString
  def getStorage(address: Address): S
  def getBlockHash(number: BigInt): Option[ByteString]

  def saveCode(address: Address, code: ByteString): WS
  def saveStorage(address: Address, storage: S): WS

  def newEmptyAccount(address: Address): WS =
    saveAccount(address, Account.Empty)

  def accountExists(address: Address): Boolean =
    getAccount(address).isDefined

  def getBalance(address: Address): UInt256 =
    getAccount(address).map(_.balance).getOrElse(UInt256.Zero)

  def transfer(from: Address, to: Address, value: BigInt): WS = {
    val debited = getGuaranteedAccount(from).updateBalance(-value)
    val credited = getAccount(to).getOrElse(Account.Empty).updateBalance(value)
    saveAccount(from, debited).saveAccount(to, credited)
  }

  def newAddress(creatorAddr: Address): (Address, WS) = {
    val creator = getGuaranteedAccount(creatorAddr)
    val hash = kec256(rlp.encode(RLPList(creatorAddr.bytes, creator.nonce)))
    val addr = Address(hash.takeRight(Address.Length))
    val updated = saveAccount(creatorAddr, creator.increaseNonce)
    (addr, updated)
  }
}
