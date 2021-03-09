package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.UInt256RLPImplicits._

/** This is a single entry point to all VM interactions with the persisted state. Implementations are meant to be
  * immutable so that rolling back a transaction is equivalent to discarding resulting changes. The changes to state
  * should be kept in memory and applied only after a transaction completes without errors. This does not forbid mutable
  * caches for DB retrieval operations.
  */
trait WorldStateProxy[WS <: WorldStateProxy[WS, S], S <: Storage[S]] { self: WS =>

  def getAccount(address: Address): Option[Account]
  def saveAccount(address: Address, account: Account): WS
  protected def deleteAccount(address: Address): WS
  def getEmptyAccount: Account
  def touchAccounts(addresses: Address*): WS
  protected def clearTouchedAccounts: WS
  protected def noEmptyAccounts: Boolean
  protected def accountStartNonce: UInt256 = UInt256.Zero

  /*
   *  During tx: 0xcf416c536ec1a19ed1fb89e4ec7ffb3cf73aa413b3aa9b77d60e4fd81a4296ba
   *  precompiled account 0000000000000000000000000000000000000003 was deleted in block 2675119,
   *  even though the deletion should have been reverted due to an out of gas error.
   *  It was due to erroneous implementations of eip161 in parity and geth.
   *  To avoid rewinding the chain, this special case is covered in parity and geth.
   *  more details:
   *  https://github.com/ethereum/EIPs/issues/716
   *  https://github.com/ethereum/go-ethereum/pull/3341/files#r89548312
   * */
  def keepPrecompileTouched(world: WS): WS

  protected val ripmdContractAddress = Address(3)

  /** In certain situation an account is guaranteed to exist, e.g. the account that executes the code, the account that
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

  def transfer(from: Address, to: Address, value: UInt256): WS =
    if (from == to || isZeroValueTransferToNonExistentAccount(to, value))
      touchAccounts(from)
    else
      // perhaps as an optimisation we could avoid touching accounts having non-zero nonce or non-empty code
      guaranteedTransfer(from, to, value).touchAccounts(from, to)

  private def guaranteedTransfer(from: Address, to: Address, value: UInt256): WS = {
    val debited = getGuaranteedAccount(from).increaseBalance(-value)
    val credited = getAccount(to).getOrElse(getEmptyAccount).increaseBalance(value)
    saveAccount(from, debited).saveAccount(to, credited)
  }

  /** IF EIP-161 is in effect this sets new contract's account initial nonce to 1 over the default value
    * for the given network (usually zero)
    */
  def initialiseAccount(newAddress: Address): WS = {

    // Per Eq. 79 from https://ethereum.github.io/yellowpaper/paper.pdf, newly initialised account should have empty codehash
    // and empty storage. It means in event of unlikely address collision existing account will have it code and storage cleared.
    val newAccount = getAccount(newAddress)
      .getOrElse(getEmptyAccount)
      .copy(codeHash = Account.EmptyCodeHash, storageRoot = Account.EmptyStorageRootHash)
    val accountWithCorrectNonce =
      if (!noEmptyAccounts)
        newAccount.copy(nonce = accountStartNonce)
      else
        newAccount.copy(nonce = accountStartNonce + 1)

    saveAccount(newAddress, accountWithCorrectNonce)
  }

  /** In case of transfer to self, during selfdestruction the ether is actually destroyed
    * see https://github.com/ethereum/wiki/wiki/Subtleties/d5d3583e1b0a53c7c49db2fa670fdd88aa7cabaf#other-operations
    * and https://github.com/ethereum/go-ethereum/blob/ff9a8682323648266d5c73f4f4bce545d91edccb/core/state/statedb.go#L322
    */
  def removeAllEther(address: Address): WS = {
    val debited = getGuaranteedAccount(address).copy(balance = 0)
    saveAccount(address, debited).touchAccounts(address)
  }

  /** Creates a new address based on the address and nonce of the creator. YP equation 82
    *
    * @param creatorAddr, the address of the creator of the new address
    * @return the new address
    */
  def createAddress(creatorAddr: Address): Address = {
    val creatorAccount = getGuaranteedAccount(creatorAddr)
    val hash = kec256(rlp.encode(RLPList(creatorAddr.bytes, (creatorAccount.nonce - 1).toRLPEncodable)))
    Address(hash)
  }

  /** Creates a new address based on the address, salt and init code
    * see https://github.com/ethereum/EIPs/blob/master/EIPS/eip-1014.md
    *
    * @param creatorAddr the address of the creator of the new address
    * @param salt salt
    * @param code code of the contract
    * @return the new address
    */
  def create2Address(creatorAddr: Address, salt: UInt256, code: ByteString): Address = {
    val prefix = 0xff.toByte
    val hash = kec256(ByteString(prefix) ++ creatorAddr.bytes ++ salt.bytes ++ kec256(code))
    Address(hash)
  }

  /** Increase nonce for a guaranteed account - ie. throws an error if this does not exist
    */
  def increaseNonce(address: Address): WS = {
    val account = getGuaranteedAccount(address).increaseNonce()
    saveAccount(address, account)
  }

  /** Determines if account of provided address is dead.
    * According to EIP161: An account is considered dead when either it is non-existent or it is empty
    *
    * @param address, the address of the checked account
    * @return true if account is dead, false otherwise
    */
  def isAccountDead(address: Address): Boolean =
    getAccount(address).forall(_.isEmpty(accountStartNonce))

  def nonEmptyCodeOrNonceAccount(address: Address): Boolean =
    getAccount(address).exists(_.nonEmptyCodeOrNonce(accountStartNonce))

  def isZeroValueTransferToNonExistentAccount(address: Address, value: UInt256): Boolean =
    noEmptyAccounts && value == UInt256(0) && !accountExists(address)
}
