package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.data.{GenesisAccount, GenesisData, GenesisDataLoader}
import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.blocks._
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.Block._
import io.iohk.ethereum.domain.{Address, Block, BlockchainImpl, UInt256}
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.testmode.{TestLedgerWrapper, TestmodeConsensus}
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.utils.{ByteStringUtils, Logger}
import monix.eval.Task
import monix.execution.Scheduler
import org.bouncycastle.util.encoders.Hex
import io.iohk.ethereum.jsonrpc.JsonMethodsImplicits._

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
      maximumExtraDataSize: BigInt
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

    // remove current genesis (Try because it may not exist)
    Try(blockchain.removeBlock(blockchain.genesisHeader.hash, withState = false))

    // load the new genesis
    val genesisDataLoader = new GenesisDataLoader(blockchain, newBlockchainConfig)
    genesisDataLoader.loadGenesisData(genesisData)

    // update test ledger with new config
    testLedgerWrapper.blockchainConfig = newBlockchainConfig

    accountAddresses = genesisData.alloc.keys.toList
    accountRangeOffset = 0

    Task.now(Right(SetChainParamsResponse()))
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
    Task.now(Right(ModifyTimestampResponse()))
  }

  def rewindToBlock(request: RewindToBlockRequest): ServiceResponse[RewindToBlockResponse] = {
    pendingTransactionsManager ! PendingTransactionsManager.ClearPendingTransactions
    (blockchain.getBestBlockNumber() until request.blockNum by -1).foreach { n =>
      blockchain.removeBlock(blockchain.getBlockHeaderByNumber(n).get.hash, withState = false)
    }
    Task.now(Right(RewindToBlockResponse()))
  }

  def importRawBlock(request: ImportRawBlockRequest): ServiceResponse[ImportRawBlockResponse] = {
    Try(decode(request.blockRlp).toBlock) match {
      case Failure(_) => Task.now(Left(JsonRpcError(-1, "block validation failed!", None)))
      case Success(value) => {
        testLedgerWrapper.ledger
          .importBlock(value)
          .flatMap(handleResult)
      }
    }
  }

  private def handleResult(blockImportResult: BlockImportResult): ServiceResponse[ImportRawBlockResponse] = {
    blockImportResult match {
      case BlockImportedToTop(blockImportData) => {
        val blockHash = s"0x${ByteStringUtils.hash2string(blockImportData.head.block.header.hash)}"
        Task.now(Right(ImportRawBlockResponse(blockHash)))
      }
      case _ => Task.now(Left(JsonRpcError(-1, "block validation failed!", None)))
    }
  }

  def setEtherbase(req: SetEtherbaseRequest): ServiceResponse[SetEtherbaseResponse] = {
    etherbase = req.etherbase
    Task.now(Right(SetEtherbaseResponse()))
  }

  private def getBlockForMining(parentBlock: Block): Task[PendingBlock] = {
    implicit val timeout = Timeout(5.seconds)
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
      Task.now(Right(AccountsInRangeResponse(Map(), ByteString(0))))
    }

    val accountBatch = accountAddresses
      .slice(accountRangeOffset, accountRangeOffset + request.parameters.maxResults.toInt + 1)

    val addressesForExistingAccounts = accountBatch
      .filter(key => {
        val accountOpt = blockchain.getAccount(Address(key), blockOpt.get.header.number)
        accountOpt.isDefined
      })
      .map(key => (key, Address(crypto.kec256(Hex.decode(key)))))

    Task.now(
      Right(
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
        )
      )
    )
  }
}
