package io.iohk.ethereum.extvm

import java.nio.ByteOrder

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Framing, Keep, Sink, SinkQueueWithCancel, Source, SourceQueueWithComplete, Tcp}
import akka.util.ByteString
import com.google.protobuf.{ByteString => GByteString}
import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.domain.{Account, Address, BlockHeader, UInt256}
import io.iohk.ethereum.extvm.Implicits._
import io.iohk.ethereum.utils._
import io.iohk.ethereum.vm
import io.iohk.ethereum.vm.ProgramResult

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object VmServerApp {

  implicit val system = ActorSystem("EVM_System")
  implicit val materializer = ActorMaterializer()

  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load()
    Tcp().bind(config.getString("mantis.extvm.host"), config.getInt("mantis.extvm.port"))
      .runForeach(connection => handleConnection(connection.flow))
  }

  def handleConnection(connection: Flow[ByteString, ByteString, NotUsed]): Unit = {
    val (out, in) = Source.queue[ByteString](QueueBufferSize, OverflowStrategy.dropTail)
      .via(connection)
      .via(Framing.lengthField(LengthPrefixSize, 0, Int.MaxValue, ByteOrder.BIG_ENDIAN))
      .map(_.drop(LengthPrefixSize))
      .toMat(Sink.queue[ByteString]())(Keep.both)
      .run()

    new VMServer(in, out).run()
  }
}

class VMServer(
    override val in: SinkQueueWithCancel[ByteString],
    override val out: SourceQueueWithComplete[ByteString])
  extends MessageUtils with Logger {

  class StorageCache {
    private val cache = mutable.Map[(Address, UInt256), UInt256]()

    def getStorageData(address: Address, offset: UInt256): UInt256 = cache.getOrElse((address, offset), {
      val getStorageDataMsg = msg.GetStorageData(address = address, offset = offset)
      val query = msg.VMQuery(query = msg.VMQuery.Query.GetStorageData(getStorageDataMsg))
      sendMessage(query)

      val storageData = awaitMessage[msg.StorageData]
      log.debug("Server received msg: StorageData")
      val value: UInt256 = storageData.data

      cache += (address, offset) -> value
      value
    })
  }

  class Storage(val address: Address, val storage: Map[UInt256, UInt256], cache: StorageCache) extends vm.Storage[Storage] {

    def store(offset: UInt256, value: UInt256): Storage =
      new Storage(address, storage + (offset -> value), cache)

    def load(offset: UInt256): UInt256 =
      storage.getOrElse(offset, cache.getStorageData(address, offset))
  }

  class AccountCache {
    private val cache = mutable.Map[Address, Option[Account]]()

    def getAccount(address: Address): Option[Account] =
      cache.getOrElse(address, {
        val getAccountMsg = msg.GetAccount(address)
        val query = msg.VMQuery(query = msg.VMQuery.Query.GetAccount(getAccountMsg))
        sendMessage(query)

        val accountMsg = awaitMessage[msg.Account]
        log.debug("Server received msg: Account")

        if (accountMsg.nonce.isEmpty) {
          cache += address -> None
          None
        } else {
          val account = Account(accountMsg.nonce, accountMsg.balance, accountMsg.storageHash, accountMsg.codeHash)
          cache += address -> Some(account)
          Some(account)
        }
      })
  }

  class CodeCache {
    private val cache = mutable.Map[Address, ByteString]()

    def getCode(address: Address): ByteString =
      cache.getOrElse(address, {
        val getCodeMsg = msg.GetCode(address)
        val query = msg.VMQuery(query = msg.VMQuery.Query.GetCode(getCodeMsg))
        sendMessage(query)

        val codeMsg = awaitMessage[msg.Code]
        log.debug("Server received msg: Code")

        val code: ByteString = codeMsg.code
        cache += address -> code
        code
      })
  }

  class BlockhashCache {
    private val cache = mutable.Map[UInt256, Option[UInt256]]()

    def getBlockhash(offset: UInt256): Option[UInt256] =
      cache.getOrElse(offset, {
        val getBlockhashMsg = msg.GetBlockhash(if (offset > Int.MaxValue) - 1 else offset.toInt)
        val query = msg.VMQuery(query = msg.VMQuery.Query.GetBlockhash(getBlockhashMsg))
        sendMessage(query)

        val blockhashMsg = awaitMessage[msg.Blockhash]
        log.debug("Server received msg: Blockhash")

        if (blockhashMsg.hash.isEmpty) {
          cache += offset -> None
          None
        } else {
          val hash: UInt256 = blockhashMsg.hash
          cache += offset -> Some(hash)
          Some(hash)
        }
      })
  }

  case class World(
    override val accountStartNonce: UInt256,
    noEmptyAccountsCond: Boolean,
    accounts: Map[Address, Account] = Map(),
    storages: Map[Address, Storage] = Map(),
    codeRepo: Map[Address, ByteString] = Map(),
    touchedAccounts: Set[Address] = Set(),
    accountCache: AccountCache = new AccountCache,
    storageCache: StorageCache = new StorageCache,
    codeCache: CodeCache = new CodeCache,
    blockhashCache: BlockhashCache = new BlockhashCache
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

    def combineTouchedAccounts(world: World): World =
      copy(touchedAccounts = touchedAccounts ++ world.touchedAccounts)

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

  @tailrec
  private def processNextCall(): Unit = {
    Try {
      val callContext = awaitMessage[msg.CallContext]

      log.debug("Server received msg: CallContext")

      val context = constructContextFromMsg(callContext)
      val result = vm.VM.run(context)

      val callResultMsg = buildResultMsg(result)
      val queryMsg = msg.VMQuery(query = msg.VMQuery.Query.CallResult(callResultMsg))
      sendMessage(queryMsg)
    } match {
      case Success(_) => processNextCall()
      case Failure(_) => close()
    }
  }

  private var defaultBlockchainConfig: msg.BlockchainConfig = _

  private def awaitDefaultBlockchainConfig(): Unit = {
    defaultBlockchainConfig = awaitMessage[msg.BlockchainConfig]
  }

  def run(): Unit = {
    new Thread(() => {
      awaitDefaultBlockchainConfig()
      processNextCall()
    }).start()
  }

  def close(): Unit = {
    log.info("Connection closed")
    Try(in.cancel())
    Try(out.complete())
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

    val blockchainConfig = constructBlockchainConfig(contextMsg.blockchainConfig.getOrElse(defaultBlockchainConfig))

    val vmConfig = vm.EvmConfig.forBlock(env.blockHeader.number, blockchainConfig)
    val world = new World(blockchainConfig.accountStartNonce, vmConfig.noEmptyAccounts)

    vm.ProgramContext(env, contextMsg.receivingAddr, contextMsg.gasProvided, world, None, vmConfig)
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
    val modifiedAddresses = world.accounts.keySet ++ world.codeRepo.keySet ++ world.storages.keySet
    modifiedAddresses.toList.map { address =>
      val acc = world.getAccount(address)
      val storage = world.getStorage(address)
      val storageUpdates = storage.storage.map { case (key, value) => msg.StorageUpdate(key, value) }.toList

      msg.ModifiedAccount(
        address = address,
        nonce = acc.map(_.nonce: GByteString).getOrElse(GByteString.EMPTY),
        balance = acc.map(_.balance: GByteString).getOrElse(GByteString.EMPTY),
        storageUpdates = storageUpdates,
        code = world.getCode(address))
    }
  }

  private def constructBlockchainConfig(conf: msg.BlockchainConfig): BlockchainConfig = {
    new BlockchainConfig {
      override val frontierBlockNumber: BigInt = conf.frontierBlockNumber
      override val homesteadBlockNumber: BigInt = conf.homesteadBlockNumber
      override val eip106BlockNumber: BigInt = conf.eip106BlockNumber
      override val eip150BlockNumber: BigInt = conf.eip150BlockNumber
      override val eip155BlockNumber: BigInt = conf.eip155BlockNumber
      override val eip160BlockNumber: BigInt = conf.eip160BlockNumber
      override val eip161BlockNumber: BigInt = conf.eip161BlockNumber
      override val maxCodeSize: Option[BigInt] = if (conf.maxCodeSize.isEmpty) None else Some(bigintFromGByteString(conf.maxCodeSize))
      override val difficultyBombPauseBlockNumber: BigInt = conf.difficultyBombPauseBlockNumber
      override val difficultyBombContinueBlockNumber: BigInt = conf.difficultyBombContinueBlockNumber

      override val customGenesisFileOpt: Option[String] = None

      override val daoForkConfig = if (conf.forkBlockHash.isEmpty) None else Some(new DaoForkConfig {
        override val blockExtraData: Option[ByteString] = if (conf.forkBlockExtraData.isEmpty) None else Some(conf.forkBlockExtraData)
        override val range: Int = conf.forkRange
        override val drainList: Seq[Address] = conf.forkDrainList.map(addressFromGByteString)
        override val forkBlockHash: ByteString = conf.forkBlockHash
        override val forkBlockNumber: BigInt = conf.forkBlockNumber
        override val refundContract: Option[Address] = if (conf.forkRefundContract.isEmpty) None else Some(conf.forkRefundContract)
      })

      override val accountStartNonce: UInt256 = conf.accountStartNonce

      override val chainId: Byte = conf.chainId.head

      override val monetaryPolicyConfig = MonetaryPolicyConfig(
        eraDuration = conf.eraDuration,
        rewardReductionRate = conf.rewardReductionRate,
        firstEraBlockReward = conf.firstEraBlockReward)

      val gasTieBreaker: Boolean = conf.gasTieBreaker
    }
  }
}
