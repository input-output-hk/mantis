package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.data.{AllocAccount, GenesisData, GenesisDataLoader}
import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.blocks._
import io.iohk.ethereum.domain.{Address, Block, BlockchainImpl, UInt256}
import io.iohk.ethereum.testmode.{TestLedgerWrapper, TestmodeConsensus}
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.utils.Logger
import monix.eval.Task
import monix.execution.Scheduler
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.duration._
import scala.util.Try

object TestService {
  case class GenesisParams(
      author: ByteString,
      extraData: ByteString,
      gasLimit: BigInt,
      parentHash: ByteString,
      timestamp: ByteString
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
  case class PrecompiledAccountConfig(name: String)
  case class AccountConfig(precompiled: Option[PrecompiledAccountConfig], wei: BigInt)
  case class ChainParams(
      genesis: GenesisParams,
      blockchainParams: BlockchainParams,
      sealEngine: String,
      accounts: Map[ByteString, AccountConfig]
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

  def setChainParams(request: SetChainParamsRequest): ServiceResponse[SetChainParamsResponse] = {
    val newBlockchainConfig = testLedgerWrapper.blockchainConfig.copy(
      homesteadBlockNumber = request.chainParams.blockchainParams.homesteadForkBlock,
      eip150BlockNumber = request.chainParams.blockchainParams.EIP150ForkBlock,
      accountStartNonce = UInt256(request.chainParams.blockchainParams.accountStartNonce),
      networkId = 1,
      bootstrapNodes = Set()
    )

    val genesisData = GenesisData(
      nonce = ByteString(Hex.decode("00")),
      mixHash = None,
      difficulty = "0",
      extraData = request.chainParams.genesis.extraData,
      gasLimit = "0x" + request.chainParams.genesis.gasLimit.toString(16),
      coinbase = request.chainParams.genesis.author,
      timestamp = Hex.toHexString(request.chainParams.genesis.timestamp.toArray[Byte]),
      alloc = request.chainParams.accounts.map { case (addr, acc) =>
        Hex.toHexString(addr.toArray[Byte]) -> AllocAccount(acc.wei.toString)
      }
    )

    // remove current genesis (Try because it may not exist)
    Try(blockchain.removeBlock(blockchain.genesisHeader.hash, withState = false))

    // load the new genesis
    val genesisDataLoader = new GenesisDataLoader(blockchain, newBlockchainConfig)
    genesisDataLoader.loadGenesisData(genesisData)

    // update test ledger with new config
    testLedgerWrapper.blockchainConfig = newBlockchainConfig

    Task.now(Right(SetChainParamsResponse()))
  }

  def mineBlocks(request: MineBlocksRequest): ServiceResponse[MineBlocksResponse] = {
    def mineBlock(): Task[Unit] =
      getBlockForMining(blockchain.getBestBlock().get).map { blockForMining =>
        val res = testLedgerWrapper.ledger.importBlock(blockForMining.block)
        log.info("Block mining result: " + res)
        pendingTransactionsManager ! PendingTransactionsManager.ClearPendingTransactions
        consensus.blockTimestamp += 1
      }

    def doNTimesF(n: Int)(fn: Task[Unit]): Task[Unit] = fn.flatMap { res =>
      if (n <= 1) Task.now(res)
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
      .flatMap { pendingTxs =>
        val pb = consensus.blockGenerator.generateBlock(
          parentBlock,
          pendingTxs.pendingTransactions.map(_.stx.tx),
          etherbase,
          Nil,
          None
        )
        Task.now(pb.pendingBlock)
      }
      .timeout(timeout.duration)
  }
}
