package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.data.{GenesisAccount, GenesisData, GenesisDataLoader}
import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.blocks._
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.storage.{StateStorage, TransactionMappingStorage}
import io.iohk.ethereum.{crypto, domain, rlp}
import io.iohk.ethereum.domain.Block._
import io.iohk.ethereum.domain.{Account, Address, Block, BlockchainImpl, UInt256}
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.testmode.{SealEngineType, TestModeComponentsProvider}
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.utils.{BlockchainConfig, ByteStringUtils, ForkBlockNumbers, Logger}
import monix.eval.Task
import monix.execution.Scheduler
import org.bouncycastle.util.encoders.Hex
import io.iohk.ethereum.jsonrpc.JsonMethodsImplicits._
import io.iohk.ethereum.rlp.RLPList

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object TestService {
  case class GenesisParams(
      author: ByteString,
      difficulty: String,
      extraData: ByteString,
      gasLimit: BigInt,
      parentHash: ByteString,
      timestamp: ByteString,
      nonce: ByteString,
      mixHash: ByteString
  )
  case class BlockchainParams(
      EIP150ForkBlock: Option[BigInt],
      EIP158ForkBlock: Option[BigInt],
      accountStartNonce: BigInt,
      allowFutureBlocks: Boolean,
      blockReward: BigInt,
      byzantiumForkBlock: Option[BigInt],
      homesteadForkBlock: Option[BigInt],
      maximumExtraDataSize: BigInt,
      constantinopleForkBlock: Option[BigInt],
      istanbulForkBlock: Option[BigInt]
  )

  case class ChainParams(
      genesis: GenesisParams,
      blockchainParams: BlockchainParams,
      sealEngine: SealEngineType,
      accounts: Map[ByteString, GenesisAccount]
  )

  case class AccountsInRangeRequestParams(
      blockHashOrNumber: Either[BigInt, ByteString],
      txIndex: BigInt,
      addressHash: ByteString,
      maxResults: Int
  )

  case class AccountsInRange(
      addressMap: Map[ByteString, ByteString],
      nextKey: ByteString
  )

  case class StorageRangeParams(
      blockHashOrNumber: Either[BigInt, ByteString],
      txIndex: BigInt,
      address: ByteString,
      begin: BigInt,
      maxResults: Int
  )

  case class StorageEntry(key: String, value: String)

  case class SetChainParamsRequest(chainParams: ChainParams)
  case class SetChainParamsResponse()

  case class MineBlocksRequest(num: Int)
  case class MineBlocksResponse()

  case class ModifyTimestampRequest(timestamp: Long)
  case class ModifyTimestampResponse()

  case class RewindToBlockRequest(blockNum: Long)
  case class RewindToBlockResponse()

  case class SetEtherbaseRequest(etherbase: Address)
  case class SetEtherbaseResponse()

  case class ImportRawBlockRequest(blockRlp: String)
  case class ImportRawBlockResponse(blockHash: String)

  case class AccountsInRangeRequest(parameters: AccountsInRangeRequestParams)
  case class AccountsInRangeResponse(addressMap: Map[ByteString, ByteString], nextKey: ByteString)

  case class StorageRangeRequest(parameters: StorageRangeParams)
  case class StorageRangeResponse(
      complete: Boolean,
      storage: Map[String, StorageEntry],
      nextKey: Option[String]
  )

  case class GetLogHashRequest(transactionHash: ByteString)
  case class GetLogHashResponse(logHash: ByteString)
}

class TestService(
    blockchain: BlockchainImpl,
    stateStorage: StateStorage,
    pendingTransactionsManager: ActorRef,
    consensusConfig: ConsensusConfig,
    testModeComponentsProvider: TestModeComponentsProvider,
    initialConfig: BlockchainConfig,
    transactionMappingStorage: TransactionMappingStorage,
    preimageCache: collection.concurrent.Map[ByteString, UInt256]
)(implicit
    scheduler: Scheduler
) extends Logger {

  import TestService._
  import io.iohk.ethereum.jsonrpc.AkkaTaskOps._

  private var etherbase: Address = consensusConfig.coinbase
  private var accountHashWithAdresses: List[(ByteString, Address)] = List()
  private var currentConfig: BlockchainConfig = initialConfig
  private var blockTimestamp: Long = 0
  private var sealEngine: SealEngineType = SealEngineType.NoReward

  def setChainParams(request: SetChainParamsRequest): ServiceResponse[SetChainParamsResponse] = {
    currentConfig = buildNewConfig(request.chainParams.blockchainParams)

    val genesisData = GenesisData(
      nonce = request.chainParams.genesis.nonce,
      mixHash = Some(request.chainParams.genesis.mixHash),
      difficulty = request.chainParams.genesis.difficulty,
      extraData = request.chainParams.genesis.extraData,
      gasLimit = "0x" + request.chainParams.genesis.gasLimit.toString(16),
      coinbase = request.chainParams.genesis.author,
      timestamp = Hex.toHexString(request.chainParams.genesis.timestamp.toArray[Byte]),
      alloc = request.chainParams.accounts.map { case (addr, acc) =>
        Hex.toHexString(addr.toArray[Byte]) -> acc
      }
    )

    // set coinbase for blocks that will be tried to mine
    etherbase = Address(genesisData.coinbase)

    sealEngine = request.chainParams.sealEngine

    resetPreimages(genesisData)

    // remove current genesis (Try because it may not exist)
    Try(blockchain.removeBlock(blockchain.genesisHeader.hash, withState = false))

    // load the new genesis
    val genesisDataLoader = new GenesisDataLoader(blockchain, stateStorage, currentConfig)
    genesisDataLoader.loadGenesisData(genesisData)

    //save account codes to world state
    storeGenesisAccountCodes(genesisData.alloc)
    storeGenesisAccountStorageData(genesisData.alloc)

    accountHashWithAdresses = (etherbase.toUnprefixedString :: genesisData.alloc.keys.toList)
      .map(hexAddress => {
        val address = Address(hexAddress)
        crypto.kec256(address.bytes) -> address
      })
      .sortBy(v => UInt256(v._1))

    SetChainParamsResponse().rightNow
  }

  val neverOccuringBlock: Int = Int.MaxValue
  private def buildNewConfig(blockchainParams: BlockchainParams) = {
    val byzantiumBlockNumber: BigInt = blockchainParams.byzantiumForkBlock.getOrElse(neverOccuringBlock)
    val istanbulForkBlockNumber: BigInt = blockchainParams.istanbulForkBlock.getOrElse(neverOccuringBlock)

    // For block number which are not specified by retesteth, we try to align the number to another fork
    currentConfig.copy(
      forkBlockNumbers = ForkBlockNumbers(
        frontierBlockNumber = 0,
        homesteadBlockNumber = blockchainParams.homesteadForkBlock.getOrElse(neverOccuringBlock),
        eip106BlockNumber = neverOccuringBlock,
        eip150BlockNumber = blockchainParams.EIP150ForkBlock.getOrElse(neverOccuringBlock),
        eip155BlockNumber = byzantiumBlockNumber,
        eip160BlockNumber = byzantiumBlockNumber,
        eip161BlockNumber = byzantiumBlockNumber,
        difficultyBombPauseBlockNumber = neverOccuringBlock,
        difficultyBombContinueBlockNumber = neverOccuringBlock,
        difficultyBombRemovalBlockNumber = neverOccuringBlock,
        byzantiumBlockNumber = byzantiumBlockNumber,
        ecip1049BlockNumber = None,
        ecip1097BlockNumber = neverOccuringBlock,
        ecip1098BlockNumber = neverOccuringBlock,
        constantinopleBlockNumber = blockchainParams.constantinopleForkBlock.getOrElse(neverOccuringBlock),
        petersburgBlockNumber = istanbulForkBlockNumber,
        aghartaBlockNumber = istanbulForkBlockNumber,
        istanbulBlockNumber = istanbulForkBlockNumber,
        atlantisBlockNumber = istanbulForkBlockNumber,
        phoenixBlockNumber = istanbulForkBlockNumber,
        ecip1099BlockNumber = neverOccuringBlock
      ),
      accountStartNonce = UInt256(blockchainParams.accountStartNonce),
      networkId = 1,
      bootstrapNodes = Set()
    )
  }

  private def storeGenesisAccountCodes(accounts: Map[String, GenesisAccount]): Unit =
    accounts
      .collect { case (_, GenesisAccount(_, _, Some(code), _, _)) => code }
      .foreach { code => blockchain.storeEvmCode(kec256(code), code).commit() }

  private def storeGenesisAccountStorageData(accounts: Map[String, GenesisAccount]): Unit = {
    val emptyStorage = domain.EthereumUInt256Mpt.storageMpt(
      Account.EmptyStorageRootHash,
      stateStorage.getBackingStorage(0)
    )
    val storagesToPersist = accounts
      .flatMap(pair => pair._2.storage)
      .map(accountStorage => accountStorage.filterNot { case (_, v) => v.isZero })
      .filter(_.nonEmpty)

    val toBigInts: ((UInt256, UInt256)) => (BigInt, BigInt) = { case (a, b) => (a, b) }
    storagesToPersist.foreach(storage => emptyStorage.update(Nil, storage.toSeq.map(toBigInts)))
  }

  def mineBlocks(request: MineBlocksRequest): ServiceResponse[MineBlocksResponse] = {
    def mineBlock(): Task[Unit] = {
      getBlockForMining(blockchain.getBestBlock().get)
        .flatMap(blockForMining =>
          testModeComponentsProvider.ledger(currentConfig, sealEngine).importBlock(blockForMining.block)
        )
        .map { res =>
          log.info("Block mining result: " + res)
          pendingTransactionsManager ! PendingTransactionsManager.ClearPendingTransactions
          blockTimestamp += 1
        }
    }

    def doNTimesF(n: Int)(fn: Task[Unit]): Task[Unit] = fn.flatMap { _ =>
      if (n <= 1) Task.unit
      else doNTimesF(n - 1)(fn)
    }

    doNTimesF(request.num)(mineBlock()).as(Right(MineBlocksResponse()))
  }

  def modifyTimestamp(request: ModifyTimestampRequest): ServiceResponse[ModifyTimestampResponse] = {
    blockTimestamp = request.timestamp
    ModifyTimestampResponse().rightNow
  }

  def rewindToBlock(request: RewindToBlockRequest): ServiceResponse[RewindToBlockResponse] = {
    pendingTransactionsManager ! PendingTransactionsManager.ClearPendingTransactions
    (blockchain.getBestBlockNumber() until request.blockNum by -1).foreach { n =>
      blockchain.removeBlock(blockchain.getBlockHeaderByNumber(n).get.hash, withState = false)
    }
    RewindToBlockResponse().rightNow
  }

  def importRawBlock(request: ImportRawBlockRequest): ServiceResponse[ImportRawBlockResponse] = {
    Try(decode(request.blockRlp).toBlock) match {
      case Failure(_) =>
        Task.now(Left(JsonRpcError(-1, "block validation failed!", None)))
      case Success(value) =>
        testModeComponentsProvider
          .ledger(currentConfig, sealEngine)
          .importBlock(value)
          .flatMap(handleResult)
    }
  }

  private def handleResult(blockImportResult: BlockImportResult): ServiceResponse[ImportRawBlockResponse] = {
    blockImportResult match {
      case BlockImportedToTop(blockImportData) =>
        val blockHash = s"0x${ByteStringUtils.hash2string(blockImportData.head.block.header.hash)}"
        ImportRawBlockResponse(blockHash).rightNow
      case e =>
        log.warn("Block import failed with {}", e)
        Task.now(Left(JsonRpcError(-1, "block validation failed!", None)))
    }
  }

  def setEtherbase(req: SetEtherbaseRequest): ServiceResponse[SetEtherbaseResponse] = {
    etherbase = req.etherbase
    SetEtherbaseResponse().rightNow
  }

  private def resetPreimages(genesisData: GenesisData): Unit = {
    preimageCache.clear()
    for {
      (_, account) <- genesisData.alloc
      storage <- account.storage
      storageKey <- storage.keys
    } {
      preimageCache.put(crypto.kec256(storageKey.bytes), storageKey)
    }
  }

  private def getBlockForMining(parentBlock: Block): Task[PendingBlock] = {
    implicit val timeout: Timeout = Timeout(20.seconds)
    pendingTransactionsManager
      .askFor[PendingTransactionsResponse](PendingTransactionsManager.GetPendingTransactions)
      .timeout(timeout.duration)
      .onErrorRecover { case _ => PendingTransactionsResponse(Nil) }
      .map { pendingTxs =>
        testModeComponentsProvider
          .consensus(currentConfig, sealEngine, blockTimestamp)
          .blockGenerator
          .generateBlock(
            parentBlock,
            pendingTxs.pendingTransactions.map(_.stx.tx),
            etherbase,
            Nil,
            None
          )
          .pendingBlock
      }
      .timeout(timeout.duration)
  }

  /** Get the list of accounts of size _maxResults in the given _blockHashOrNumber after given _txIndex.
    * In response AddressMap contains addressHash - > address starting from given _addressHash.
    * nexKey field is the next addressHash (if any addresses left in the state).
    * @see https://github.com/ethereum/retesteth/wiki/RPC-Methods#debug_accountrange
    */
  def getAccountsInRange(request: AccountsInRangeRequest): ServiceResponse[AccountsInRangeResponse] = {
    // This implementation works by keeping a list of know account from the genesis state
    // It might not cover all the cases as an account created inside a transaction won't be there.

    val blockOpt = request.parameters.blockHashOrNumber
      .fold(number => blockchain.getBlockByNumber(number), blockHash => blockchain.getBlockByHash(blockHash))

    if (blockOpt.isEmpty) {
      AccountsInRangeResponse(Map(), ByteString(0)).rightNow
    } else {
      val accountBatch: Seq[(ByteString, Address)] = accountHashWithAdresses.view
        .dropWhile { case (hash, _) => UInt256(hash) < UInt256(request.parameters.addressHash) }
        .filter { case (_, address) => blockchain.getAccount(address, blockOpt.get.header.number).isDefined }
        .take(request.parameters.maxResults + 1)
        .to(Seq)

      val addressMap: Map[ByteString, ByteString] = accountBatch
        .take(request.parameters.maxResults)
        .map { case (hash, address) => hash -> address.bytes }
        .to(Map)

      AccountsInRangeResponse(
        addressMap = addressMap,
        nextKey =
          if (accountBatch.size > request.parameters.maxResults)
            accountBatch.last._1
          else UInt256(0).bytes
      ).rightNow
    }
  }

  /** Get the list of storage values starting from _begin and up to _begin + _maxResults at given block.
    * nexKey field is the next key hash if any key left in the state, or 0x00 otherwise.
    *
    * Normally, this RPC method is supposed to also be able to look up the state after after transaction
    * _txIndex is executed. This is currently not supported in mantis.
    * @see https://github.com/ethereum/retesteth/wiki/RPC-Methods#debug_storagerangeat
    */
  // TODO ETCM-784, ETCM-758: see how we can get a state after an arbitrary transation
  def storageRangeAt(request: StorageRangeRequest): ServiceResponse[StorageRangeResponse] = {

    val blockOpt = request.parameters.blockHashOrNumber
      .fold(number => blockchain.getBlockByNumber(number), hash => blockchain.getBlockByHash(hash))

    (for {
      block <- blockOpt.toRight(StorageRangeResponse(complete = false, Map.empty, None))
      accountOpt = blockchain.getAccount(Address(request.parameters.address), block.header.number)
      account <- accountOpt.toRight(StorageRangeResponse(complete = false, Map.empty, None))

    } yield {
      // This implementation might be improved. It is working for most tests in ETS but might be
      // not really efficient and would not work outside of a test context. We simply iterate over
      // every key known by the preimage cache.
      val (valueBatch, next) = preimageCache.toSeq
        .sortBy(v => UInt256(v._1))
        .view
        .dropWhile { case (hash, _) => UInt256(hash) < request.parameters.begin }
        .map { case (keyHash, keyValue) =>
          (keyHash.toArray, keyValue, blockchain.getAccountStorageAt(account.storageRoot, keyValue, true))
        }
        .filterNot { case (_, _, storageValue) => storageValue == ByteString(0) }
        .take(request.parameters.maxResults + 1)
        .splitAt(request.parameters.maxResults)

      val storage = valueBatch
        .map { case (keyHash, keyValue, value) =>
          UInt256(keyHash).toHexString -> StorageEntry(keyValue.toHexString, UInt256(value).toHexString)
        }
        .to(Map)

      StorageRangeResponse(
        complete = next.isEmpty,
        storage = storage,
        nextKey = next.headOption.map { case (hash, _, _) => UInt256(hash).toHexString }
      )
    }).fold(identity, identity).rightNow
  }

  def getLogHash(request: GetLogHashRequest): ServiceResponse[GetLogHashResponse] = {
    import io.iohk.ethereum.network.p2p.messages.ETH63.TxLogEntryImplicits.TxLogEntryEnc

    val result = for {
      transactionLocation <- transactionMappingStorage.get(request.transactionHash)
      block <- blockchain.getBlockByHash(transactionLocation.blockHash)
      _ <- block.body.transactionList.lift(transactionLocation.txIndex)
      receipts <- blockchain.getReceiptsByHash(block.header.hash)
      logs = receipts.flatMap(receipt => receipt.logs)
      rlpList: RLPList = RLPList(logs.map(_.toRLPEncodable).toList: _*)
    } yield ByteString(crypto.kec256(rlp.encode(rlpList)))

    result.fold(GetLogHashResponse(emptyLogRlpHash))(rlpHash => GetLogHashResponse(rlpHash)).rightNow
  }

  private val emptyLogRlpHash: ByteString = ByteString(crypto.kec256(rlp.encode(RLPList())))

  private implicit class RichResponse[A](response: A) {
    def rightNow: Task[Either[JsonRpcError, A]] = Task.now(Right(response))
  }
}
