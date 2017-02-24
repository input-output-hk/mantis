package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.crypto.kec256

trait WorldStateProxy {
  type WS <: WorldStateProxy

  def getAccount(address: Address): Option[Account]
  def getCode(codeHash: ByteString): ByteString
  def getStorage(storageRoot: ByteString): Storage

  def saveAccount(address: Address, account: Account): WS
  def saveCode(codeHash: ByteString, code: ByteString): WS
  def saveStorage(storageRoot: ByteString, storage: Storage): WS


  //TODO: provide rationale
  def getGuaranteedAccount(address: Address): Account =
    getAccount(address).get

  def accountExists(address: Address): Boolean =
    getAccount(address).isDefined


  def transfer(from: Address, to: Address, value: BigInt): WS = {
    val debited = getGuaranteedAccount(from).updateBalance(-value)
    val credited = getAccount(to).getOrElse(Account.Empty).updateBalance(value)
    saveAccount(from, debited).saveAccount(to, credited).asInstanceOf[WS]
  }

  def newAddress(creatorAddr: Address): (Address, WS) = {
    val creator = getGuaranteedAccount(creatorAddr)
    val hash = kec256(rlp.encode(RLPList(creatorAddr.bytes, creator.nonce)))
    val addr = Address(hash.takeRight(Address.Length))
    addr -> saveAccount(creatorAddr, creator.increaseNonce)
  }
}
