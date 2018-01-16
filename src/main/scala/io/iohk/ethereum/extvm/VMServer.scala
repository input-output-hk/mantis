package io.iohk.ethereum.extvm

import java.io.{InputStream, OutputStream}

import akka.util.ByteString
import com.google.protobuf.{ByteString => GByteString}
import io.iohk.ethereum.domain.{Account, Address, BlockHeader, UInt256}
import io.iohk.ethereum.extvm.Implicits._
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import io.iohk.ethereum.vm
import io.iohk.ethereum.vm.ProgramResult

import scala.collection.mutable


class VMServer(blockchainConfig: BlockchainConfig, in: InputStream, out: OutputStream) extends Logger {

  class Storage(address: Address) extends vm.Storage[Storage] {
    val storage = mutable.Map[UInt256, UInt256]()

    def store(offset: UInt256, value: UInt256): Storage = {
      storage += offset -> value
      this
    }

    def load(offset: UInt256): UInt256 = storage.get(offset) match {
      case Some(value) =>
        value

      case None =>
        val getStorageDataMsg = msg.GetStorageData(address = address, offset = offset)
        val query = msg.VMQuery(query = msg.VMQuery.Query.GetStorageData(getStorageDataMsg))
        query.writeDelimitedTo(out)

        var storageData: msg.StorageData = null
        while(storageData == null) {
          storageData = msg.StorageData.parseDelimitedFrom(in).orNull
        }
        log.debug("Server received msg: StorageData")
        val value: UInt256 = storageData.data

        storage += offset -> value
        value
    }
  }

  class World(
    accountStartNonce: UInt256,
    noEmptyAccountsCond: Boolean,
    val accounts: mutable.Map[Address, Option[Account]] = mutable.Map(),
    val modifiedAccounts: mutable.Set[Address] = mutable.Set(),
    val storages: mutable.Map[Address, Storage] = mutable.Map(),
    val codeRepo: mutable.Map[Address, ByteString] = mutable.Map(),
    val blockHashes: mutable.Map[UInt256, Option[UInt256]] = mutable.Map(),
    val touchedAccounts: mutable.Set[Address] = mutable.Set()
  ) extends vm.WorldStateProxy[World, Storage] {

    override def diverge: World =
      new World(
        accountStartNonce,
        noEmptyAccountsCond,
        mutable.Map() ++ accounts,
        mutable.Set() ++ modifiedAccounts,
        mutable.Map() ++ storages,
        mutable.Map() ++ codeRepo,
        mutable.Map() ++ blockHashes,
        mutable.Set() ++ touchedAccounts)

    def getAccount(address: Address): Option[Account] = accounts.getOrElse(address, {
      val getAccountMsg = msg.GetAccount(address)
      val query = msg.VMQuery(query = msg.VMQuery.Query.GetAccount(getAccountMsg))
      query.writeDelimitedTo(out)

      var accountMsg: msg.Account = null
      while(accountMsg == null) {
        accountMsg = msg.Account.parseDelimitedFrom(in).orNull
      }
      log.debug("Server received msg: Account")

      if (accountMsg.nonce.isEmpty) {
        accounts += address -> None
        None
      } else {
        val account = Account(accountMsg.nonce, accountMsg.balance, accountMsg.storage)
        accounts += address -> Some(account)
        Some(account)
      }
    })

    def saveAccount(address: Address, account: Account): World = {
      modifiedAccounts += address
      accounts += address -> Some(account)
      this
    }

    protected def deleteAccount(address: Address): World = {
      accounts += address -> None
      this
    }

    protected def getEmptyAccount: Account = Account.empty(accountStartNonce)

    def touchAccounts(addresses: Address*): World = {
      touchedAccounts ++= addresses
      this
    }

    protected def clearTouchedAccounts: World = {
      touchedAccounts.clear()
      this
    }

    protected def noEmptyAccounts: Boolean = noEmptyAccountsCond

    def combineTouchedAccounts(world: World): World = {
      touchedAccounts ++= world.touchedAccounts
      this
    }

    def getCode(address: Address): ByteString = codeRepo.getOrElse(address, {
      val getCodeMsg = msg.GetCode(address)
      val query = msg.VMQuery(query = msg.VMQuery.Query.GetCode(getCodeMsg))
      query.writeDelimitedTo(out)

      var codeMsg: msg.Code = null
      while(codeMsg == null) {
        codeMsg = msg.Code.parseDelimitedFrom(in).orNull
      }
      log.debug("Server received msg: Code")

      val code: ByteString = codeMsg.code
      codeRepo += address -> code
      code
    })

    def getStorage(address: Address): Storage = storages.getOrElse(address, {
      val storage = new Storage(address)
      storages += address -> storage
      storage
    })

    def getBlockHash(number: UInt256): Option[UInt256] = blockHashes.getOrElse(number, {
      val getBlockhashMsg = msg.GetBlockhash(if (number > Int.MaxValue) - 1 else number.toInt)
      val query = msg.VMQuery(query = msg.VMQuery.Query.GetBlockhash(getBlockhashMsg))
      query.writeDelimitedTo(out)

      var blockhashMsg: msg.Blockhash = null
      while(blockhashMsg == null) {
        blockhashMsg = msg.Blockhash.parseDelimitedFrom(in).orNull
      }
      log.debug("Server received msg: Blockhash")

      if (blockhashMsg.hash.isEmpty) {
        blockHashes += number -> None
        None
      } else {
        val hash: UInt256 = blockhashMsg.hash
        blockHashes += number -> Some(hash)
        Some(hash)
      }
    })

    def saveCode(address: Address, code: ByteString): World = {
      modifiedAccounts += address
      codeRepo += address -> code
      this
    }

    def saveStorage(address: Address, storage: Storage): World = {
      modifiedAccounts += address
      storages += address -> storage
      this
    }
  }


  def run(): Unit = {
    new Thread(() => {
      try {

        var callContext: msg.CallContext = null
        while (callContext == null) {
          callContext = msg.CallContext.parseDelimitedFrom(in).orNull
        }
        log.debug("Server received msg: CallContext")

        val context = constructContextFromMsg(callContext)
        val result = vm.VM.run(context)

        val callResultMsg = buildResultMsg(result)
        val queryMsg = msg.VMQuery(query = msg.VMQuery.Query.CallResult(callResultMsg))
        queryMsg.writeDelimitedTo(out)

      } catch {
        case e: Exception =>
          e.printStackTrace()
      }
    }).start()
  }

  private def constructContextFromMsg(contextMsg: msg.CallContext): vm.ProgramContext[World, Storage] = {
    val blockHeader = BlockHeader(
      contextMsg.blockHeader.get.parentHash,
      contextMsg.blockHeader.get.ommersHash,
      contextMsg.blockHeader.get.beneficiary,
      contextMsg.blockHeader.get.stateRoot,
      contextMsg.blockHeader.get.transactionsRoot,
      contextMsg.blockHeader.get.receiptsRoot,
      contextMsg.blockHeader.get.logsBloom,
      contextMsg.blockHeader.get.difficulty,
      contextMsg.blockHeader.get.number,
      contextMsg.blockHeader.get.gasLimit,
      contextMsg.blockHeader.get.gasUsed,
      contextMsg.blockHeader.get.unixTimestamp,
      contextMsg.blockHeader.get.extraData,
      contextMsg.blockHeader.get.mixHash,
      contextMsg.blockHeader.get.nonce
    )

    val env = vm.ExecEnv(
      contextMsg.ownerAddr,
      contextMsg.callerAddr,
      contextMsg.originAddr,
      contextMsg.gasPrice,
      contextMsg.inputData,
      contextMsg.callValue,
      vm.Program(contextMsg.contractCode),
      blockHeader,
      contextMsg.callDepth
    )

    val vmConfig = vm.EvmConfig.forBlock(env.blockHeader.number, blockchainConfig)
    val world = new World(blockchainConfig.accountStartNonce, vmConfig.noEmptyAccounts)

    vm.ProgramContext(env, contextMsg.receivingAddr, contextMsg.gasProvided, world, vmConfig)
  }

  private def buildResultMsg(result: ProgramResult[World, Storage]): msg.CallResult = {

    val logs = result.logs.map(l =>
      msg.LogEntry(address = l.loggerAddress, topics = l.logTopics.map(t => t: GByteString), data = l.data))

    msg.CallResult(
      returnData = result.returnData,
      gasRemaining = result.gasRemaining,
      gasRefund = result.gasRefund,
      error = result.error.isDefined,
      modifiedAccounts = buildModifiedAccountsMsg(result.world),
      deletedAccounts = result.addressesToDelete.toList.map(a => a: GByteString),
      touchedAccounts = result.world.touchedAccounts.toList.map(a => a: GByteString),
      logs = logs
    )
  }

  private def buildModifiedAccountsMsg(world: World): Seq[msg.ModifiedAccount] = {
    world.modifiedAccounts.toList.map { address =>
      val acc = world.getAccount(address).get
      val storage = world.getStorage(address)
      val storageUpdates = storage.storage.map { case (key, value) => msg.StorageUpdate(key, value) }.toList
      msg.ModifiedAccount(address = address, nonce = acc.nonce, balance = acc.balance,
        storageUpdates = storageUpdates, code = world.getCode(address))
    }
  }
}
