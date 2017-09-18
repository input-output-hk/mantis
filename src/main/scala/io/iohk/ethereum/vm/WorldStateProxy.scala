package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{Account, Address, UInt256}
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.UInt256RLPImplicits._

/**
  * This is a single entry point to all VM interactions with the persisted state. Implementations are meant to be
  * immutable so that rolling back a transaction is equivalent to discarding resulting changes. The changes to state
  * should be kept in memory and applied only after a transaction completes without errors. This does not forbid mutable
  * caches for DB retrieval operations.
  */
trait WorldStateProxy[WS <: WorldStateProxy[WS, S], S <: Storage[S]] { self: WS =>

  protected def getAccount(address: Address): Option[Account]
  protected def saveAccount(address: Address, account: Account): WS
  protected def deleteAccount(address: Address): WS
  protected def getEmptyAccount: Account
  protected def touchAccount(address: Address): WS
  protected def touchAccounts(addresses: Set[Address]): WS
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
  def getBlockHash(number: UInt256): Option[UInt256]

  def saveCode(address: Address, code: ByteString): WS
  def saveStorage(address: Address, storage: S): WS

  def newEmptyAccount(address: Address): WS =
    saveAccount(address, getEmptyAccount)

  def accountExists(address: Address): Boolean =
    getAccount(address).isDefined

  def getBalance(address: Address): UInt256 =
    getAccount(address).map(a => UInt256(a.balance)).getOrElse(UInt256.Zero)

  def transfer(from: Address, to: Address, value: UInt256, noEmptyAccount: Boolean): WS = {
    if(from == to ||  noEmptyAccount && value == UInt256(0) && !accountExists(to))
      this
    else
      guaranteedTransfer(from, to, value, noEmptyAccount)
  }

  def guaranteedTransfer(from: Address, to: Address, value: UInt256, noEmptyAccount: Boolean): WS = {
    val debited = getGuaranteedAccount(from).increaseBalance(-value)
    val credited = getAccount(to).getOrElse(getEmptyAccount).increaseBalance(value)
    val world = saveAccount(from, debited).saveAccount(to, credited)

    if (noEmptyAccount)
      world.touchAccounts(Set(from, to))
    else
      world
  }

  def initialiseAccount(creatorAddress: Address, newAddress: Address, value: UInt256, noEmptyAccount: Boolean): WS = {
    val nonceOffset = if (noEmptyAccount) 1 else 0

    val creatorAccount = getGuaranteedAccount(creatorAddress).increaseBalance(-value)
    val newAccount = getAccount(newAddress).getOrElse(getEmptyAccount).increaseBalance(value).increaseNonce(nonceOffset)
    val world = saveAccount(creatorAddress,creatorAccount).saveAccount(newAddress, newAccount)
    if (noEmptyAccount)
      world.touchAccounts(Set(creatorAddress, newAddress))
    else
      world
  }

  /**
    * Creates a new address based on the address and nonce of the creator. YP equation 82
    *
    * @param creatorAddr, the address of the creator of the new address
    * @return the new address
    */
  def createAddress(creatorAddr: Address): Address = {
    val creatorAccount = getGuaranteedAccount(creatorAddr)
    val hash = kec256(rlp.encode(RLPList(creatorAddr.bytes, (creatorAccount.nonce - 1).toRLPEncodable)))
    Address(hash)
  }

  /**
    * Increases the creator's nonce and creates a new address based on the address and the new nonce of the creator
    *
    * @param creatorAddr, the address of the creator of the new address
    * @return the new address and the state world after the creator's nonce was increased
    */
  def createAddressWithOpCode(creatorAddr: Address): (Address, WS) = {
    val creatorAccount = getGuaranteedAccount(creatorAddr)
    val updatedWorld = saveAccount(creatorAddr, creatorAccount.increaseNonce())
    updatedWorld.createAddress(creatorAddr) -> updatedWorld
  }

  /**
    * Determines if account of provided address is dead.
    * According to EIP161: An account is considered dead when either it is non-existent or it is empty
    *
    * @param address, the address of the checked account
    * @return true if account is dead, false otherwise
    */
  def isDead(address: Address): Boolean = {
    getAccount(address).forall(_.isEmpty)
  }
}
