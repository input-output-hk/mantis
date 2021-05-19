package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.data.{GenesisAccount, GenesisData, GenesisDataLoader}
import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.blocks._
import io.iohk.ethereum.{crypto, rlp}
import io.iohk.ethereum.domain.Block._
import io.iohk.ethereum.domain.{Address, Block, BlockchainImpl, UInt256}
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.testmode.{TestLedgerWrapper, TestmodeConsensus}
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.utils.{BlockchainConfig, ByteStringUtils, Logger}
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
      EIP150ForkBlock: BigInt,
      EIP158ForkBlock: BigInt,
      accountStartNonce: BigInt,
      allowFutureBlocks: Boolean,
      blockReward: BigInt,
      byzantiumForkBlock: BigInt,
      homesteadForkBlock: BigInt,
      maximumExtraDataSize: BigInt,
      constantinopleForkBlock: BigInt,
      istanbulForkBlock: BigInt
  )

  case class ChainParams(
      genesis: GenesisParams,
      blockchainParams: BlockchainParams,
      sealEngine: String,
      accounts: Map[ByteString, GenesisAccount]
  )

  case class AccountsInRangeRequestParams(
      blockHashOrNumber: Either[BigInt, ByteString],
      txIndex: BigInt,
      addressHash: ByteString,
      maxResults: BigInt
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
      maxResults: BigInt
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
  case class StorageRangeResponse(complete: Boolean, storage: Map[String, StorageEntry])

  case class GetLogHashRequest(transactionHash: ByteString)
  case class GetLogHashResponse(logHash: ByteString)
}

class TestService(
    blockchain: BlockchainImpl,
    pendingTransactionsManager: ActorRef,
    consensusConfig: ConsensusConfig,
    consensus: TestmodeConsensus,
    testLedgerWrapper: TestLedgerWrapper
)(implicit
    scheduler: Scheduler
) extends Logger {

  import TestService._
  import io.iohk.ethereum.jsonrpc.AkkaTaskOps._

  private var etherbase: Address = consensusConfig.coinbase
  private var accountAddresses: List[String] = List()
  private var accountRangeOffset = 0

  def setChainParams(request: SetChainParamsRequest): ServiceResponse[SetChainParamsResponse] = {
    val newBlockchainConfig = testLedgerWrapper.blockchainConfig.copy(
      homesteadBlockNumber = request.chainParams.blockchainParams.homesteadForkBlock,
      eip150BlockNumber = request.chainParams.blockchainParams.EIP150ForkBlock,
      byzantiumBlockNumber = request.chainParams.blockchainParams.byzantiumForkBlock,
      constantinopleBlockNumber = request.chainParams.blockchainParams.constantinopleForkBlock,
      istanbulBlockNumber = request.chainParams.blockchainParams.istanbulForkBlock,
      accountStartNonce = UInt256(request.chainParams.blockchainParams.accountStartNonce),
      networkId = 1,
      bootstrapNodes = Set()
    )

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

    // remove current genesis (Try because it may not exist)
    Try(blockchain.removeBlock(blockchain.genesisHeader.hash, withState = false))

    // load the new genesis
    val genesisDataLoader = new GenesisDataLoader(blockchain, newBlockchainConfig)
    genesisDataLoader.loadGenesisData(genesisData)

    //save account codes to world state
    storeGenesisAccountCodes(newBlockchainConfig, genesisData.alloc)
    val storesRootHash = storeGenesisAccountStorageData(newBlockchainConfig, genesisData.alloc)
    // update test ledger with new config
    testLedgerWrapper.blockchainConfig = newBlockchainConfig

    // remove current genesis (Try because it may not exist)
    Try(blockchain.removeBlock(blockchain.genesisHeader.hash, withState = false))
    genesisDataLoader.loadGenesisData(genesisData, storesRootHash)

    accountAddresses = genesisData.alloc.keys.toList
    accountRangeOffset = 0

    SetChainParamsResponse().rightNow
  }

  private def storeGenesisAccountCodes(config: BlockchainConfig, accounts: Map[String, GenesisAccount]): Unit = {
    val genesisBlock = blockchain.getBlockByNumber(0).get
    val world =
      blockchain.getWorldStateProxy(0, UInt256.Zero, genesisBlock.header.stateRoot, false, config.ethCompatibleStorage)

    val accountsWithCodes = accounts.filter(pair => pair._2.code.isDefined)

    val worldToPersist = accountsWithCodes.foldLeft(world)((world, addressAccountPair) => {
      world.saveCode(Address(addressAccountPair._1), addressAccountPair._2.code.get)
    })

    InMemoryWorldStateProxy.persistState(worldToPersist)
  }

  private def storeGenesisAccountStorageData(config: BlockchainConfig, accounts: Map[String, GenesisAccount]) = {
    val genesisBlock = blockchain.getBlockByNumber(0).get
    val world =
      blockchain.getWorldStateProxy(0, UInt256.Zero, genesisBlock.header.stateRoot, false, config.ethCompatibleStorage)

    val accountsWithStorageData = accounts.filter(pair => pair._2.storage.isDefined && pair._2.storage.get.nonEmpty)

    val worldToPersist = accountsWithStorageData.foldLeft(world)((world, addressAccountPair) => {
      val address = Address(addressAccountPair._1)
      val emptyStorage = world.getStorage(address)
      val updatedStorage = addressAccountPair._2.storage.get.foldLeft(emptyStorage) { case (storage, (key, value)) =>
        storage.store(key, value)
      }
      val updatedWorld = world.saveStorage(Address(addressAccountPair._1), updatedStorage)
      updatedWorld.contractStorages.values.foreach(cs => cs.inner.nodeStorage.persist())
      updatedWorld
    })

    InMemoryWorldStateProxy.persistState(worldToPersist).contractStorages.map { case (address, values) =>
      address -> ByteString(values.inner.getRootHash)
    }
  }

  def mineBlocks(request: MineBlocksRequest): ServiceResponse[MineBlocksResponse] = {
    def mineBlock(): Task[Unit] = {
      getBlockForMining(blockchain.getBestBlock().get)
        .flatMap(blockForMining => testLedgerWrapper.ledger.importBlock(blockForMining.block))
        .map { res =>
          log.info("Block mining result: " + res)
          pendingTransactionsManager ! PendingTransactionsManager.ClearPendingTransactions
          consensus.blockTimestamp += 1
        }
    }

    def doNTimesF(n: Int)(fn: Task[Unit]): Task[Unit] = fn.flatMap { _ =>
      if (n <= 1) Task.unit
      else doNTimesF(n - 1)(fn)
    }

    doNTimesF(request.num)(mineBlock()).as(Right(MineBlocksResponse()))
  }

  def modifyTimestamp(request: ModifyTimestampRequest): ServiceResponse[ModifyTimestampResponse] = {
    consensus.blockTimestamp = request.timestamp
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
      case Failure(_) => Task.now(Left(JsonRpcError(-1, "block validation failed!", None)))
      case Success(value) =>
        testLedgerWrapper.ledger
          .importBlock(value)
          .flatMap(handleResult)
    }
  }

  private def handleResult(blockImportResult: BlockImportResult): ServiceResponse[ImportRawBlockResponse] = {
    blockImportResult match {
      case BlockImportedToTop(blockImportData) =>
        val blockHash = s"0x${ByteStringUtils.hash2string(blockImportData.head.block.header.hash)}"
        ImportRawBlockResponse(blockHash).rightNow
      case _ => Task.now(Left(JsonRpcError(-1, "block validation failed!", None)))
    }
  }

  def setEtherbase(req: SetEtherbaseRequest): ServiceResponse[SetEtherbaseResponse] = {
    etherbase = req.etherbase
    SetEtherbaseResponse().rightNow
  }

  private def getBlockForMining(parentBlock: Block): Task[PendingBlock] = {
    implicit val timeout: Timeout = Timeout(5.seconds)
    pendingTransactionsManager
      .askFor[PendingTransactionsResponse](PendingTransactionsManager.GetPendingTransactions)
      .timeout(timeout.duration)
      .onErrorRecover { case _ => PendingTransactionsResponse(Nil) }
      .map { pendingTxs =>
        consensus.blockGenerator
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

  def getAccountsInRange(request: AccountsInRangeRequest): ServiceResponse[AccountsInRangeResponse] = {
    val blockOpt = request.parameters.blockHashOrNumber
      .fold(number => blockchain.getBlockByNumber(number), blockHash => blockchain.getBlockByHash(blockHash))

    if (blockOpt.isEmpty) {
      AccountsInRangeResponse(Map(), ByteString(0)).rightNow
    } else {
      val accountBatch = accountAddresses
        .slice(accountRangeOffset, accountRangeOffset + request.parameters.maxResults.toInt + 1)

      val addressesForExistingAccounts = accountBatch
        .filter(key => blockchain.getAccount(Address(key), blockOpt.get.header.number).isDefined)
        .map(key => (key, Address(crypto.kec256(Hex.decode(key)))))

      AccountsInRangeResponse(
        addressMap = addressesForExistingAccounts
          .take(request.parameters.maxResults.toInt)
          .foldLeft(Map[ByteString, ByteString]())((el, addressPair) =>
            el + (addressPair._2.bytes -> ByteStringUtils.string2hash(addressPair._1))
          ),
        nextKey =
          if (accountBatch.size > request.parameters.maxResults)
            ByteStringUtils.string2hash(addressesForExistingAccounts.last._1)
          else UInt256(0).bytes
      ).rightNow
    }
  }

  def storageRangeAt(request: StorageRangeRequest): ServiceResponse[StorageRangeResponse] = {

    val blockOpt = request.parameters.blockHashOrNumber
      .fold(number => blockchain.getBlockByNumber(number), hash => blockchain.getBlockByHash(hash))

    (for {
      block <- blockOpt.toRight(StorageRangeResponse(complete = false, Map.empty))
      accountOpt = blockchain.getAccount(Address(request.parameters.address), block.header.number)
      account <- accountOpt.toRight(StorageRangeResponse(complete = false, Map.empty))
      storage = blockchain.getAccountStorageAt(
        account.storageRoot,
        request.parameters.begin,
        ethCompatibleStorage = true
      )
    } yield StorageRangeResponse(
      complete = true,
      storage = Map(
        encodeAsHex(request.parameters.address).values -> StorageEntry(
          encodeAsHex(request.parameters.begin).values,
          encodeAsHex(storage).values
        )
      )
    )).fold(identity, identity).rightNow
  }

  def getLogHash(request: GetLogHashRequest): ServiceResponse[GetLogHashResponse] = {
    import io.iohk.ethereum.network.p2p.messages.PV63.TxLogEntryImplicits.TxLogEntryEnc

    val result = for {
      transactionLocation <- blockchain.getTransactionLocation(request.transactionHash)
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
