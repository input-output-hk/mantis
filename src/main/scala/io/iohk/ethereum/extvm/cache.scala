package io.iohk.ethereum.extvm

import akka.util.ByteString
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.extvm.Implicits._
import io.iohk.ethereum.utils.Logger

import scala.collection.mutable

class AccountCache(messageHandler: MessageHandler) extends Logger {
  private val cache = mutable.Map[Address, Option[Account]]()

  // we don't the actual hash value, we only need to compare it to Account.EmptyCodeHash
  val nonEmptyCodeHash = Account.EmptyStorageRootHash
  // storage hash is irrelevant in the VM
  val defaultStorageHash = Account.EmptyStorageRootHash

  def getAccount(address: Address): Option[Account] =
    cache.getOrElse(
      address, {
        val getAccountMsg = msg.GetAccount(address)
        val query = msg.VMQuery(query = msg.VMQuery.Query.GetAccount(getAccountMsg))
        messageHandler.sendMessage(query)

        val accountMsg = messageHandler.awaitMessage[msg.Account]
        log.debug("Server received msg: Account")

        if (accountMsg.nonce.isEmpty) {
          cache += address -> None
          None
        } else {
          val codeHash = if (accountMsg.codeEmpty) Account.EmptyCodeHash else nonEmptyCodeHash
          val account = Account(accountMsg.nonce, accountMsg.balance, defaultStorageHash, codeHash)
          cache += address -> Some(account)
          Some(account)
        }
      }
    )
}

class CodeCache(messageHandler: MessageHandler) extends Logger {
  private val cache = mutable.Map[Address, ByteString]()

  def getCode(address: Address): ByteString =
    cache.getOrElse(
      address, {
        val getCodeMsg = msg.GetCode(address)
        val query = msg.VMQuery(query = msg.VMQuery.Query.GetCode(getCodeMsg))
        messageHandler.sendMessage(query)

        val codeMsg = messageHandler.awaitMessage[msg.Code]
        log.debug("Server received msg: Code")

        val code: ByteString = codeMsg.code
        cache += address -> code
        code
      }
    )
}

class BlockhashCache(messageHandler: MessageHandler) extends Logger {
  private val cache = mutable.Map[UInt256, Option[UInt256]]()

  def getBlockhash(offset: UInt256): Option[UInt256] =
    cache.getOrElse(
      offset, {
        val getBlockhashMsg = msg.GetBlockhash(if (offset > Int.MaxValue) -1 else offset.toInt)
        val query = msg.VMQuery(query = msg.VMQuery.Query.GetBlockhash(getBlockhashMsg))
        messageHandler.sendMessage(query)

        val blockhashMsg = messageHandler.awaitMessage[msg.Blockhash]
        log.debug("Server received msg: Blockhash")

        if (blockhashMsg.hash.isEmpty) {
          cache += offset -> None
          None
        } else {
          val hash: UInt256 = blockhashMsg.hash
          cache += offset -> Some(hash)
          Some(hash)
        }
      }
    )
}

class StorageCache(messageHandler: MessageHandler) extends Logger {
  private val cache = mutable.Map[(Address, BigInt), BigInt]()

  def getStorageData(address: Address, offset: BigInt): BigInt = cache.getOrElse(
    (address, offset), {
      val getStorageDataMsg = msg.GetStorageData(address = address, offset = offset)
      val query = msg.VMQuery(query = msg.VMQuery.Query.GetStorageData(getStorageDataMsg))
      messageHandler.sendMessage(query)

      val storageData = messageHandler.awaitMessage[msg.StorageData]
      log.debug("Server received msg: StorageData")
      val value: BigInt = storageData.data

      cache += (address, offset) -> value
      value
    }
  )
}
