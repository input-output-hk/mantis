package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.agent.Agent
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.data.{AllocAccount, GenesisData, GenesisDataLoader}
import io.iohk.ethereum.consensus.{Consensus, ConsensusConfig, FullConsensusConfig, Protocol}
import io.iohk.ethereum.consensus.blocks.{BlockGenerator, BlockTimestampProvider, PendingBlock}
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.{Address, Block, BlockchainImpl, UInt256}
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig, MonetaryPolicyConfig}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Try

object TestService {
  case class GenesisParams(author: ByteString, extraData: ByteString, gasLimit: BigInt, parentHash: ByteString, timestamp: ByteString)
  case class BlockchainParams(
      EIP150ForkBlock: BigInt,
      EIP158ForkBlock: BigInt,
      accountStartNonce: BigInt,
      allowFutureBlocks: Boolean,
      blockReward: BigInt,
      byzantiumForkBlock: BigInt,
      homesteadForkBlock: BigInt,
      maximumExtraDataSize: BigInt)
  case class PrecompiledAccountConfig(name: String)
  case class AccountConfig(precompiled: Option[PrecompiledAccountConfig], wei: BigInt)
  case class ChainParams(genesis: GenesisParams, blockchainParams: BlockchainParams, sealEngine: String, accounts: Map[ByteString, AccountConfig])

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
    vm: VMImpl,
    blockchain: BlockchainImpl,
    pendingTransactionsManager: ActorRef,
    consensusConfig: ConsensusConfig,
    initialBlockchainConfig: BlockchainConfig,
    validators: Validators,
                 consensus: Consensus) {

  import TestService._
  import akka.pattern.ask

  private var blockchainConfig: BlockchainConfig = initialBlockchainConfig
  private var etherbase: Address = consensusConfig.coinbase
  private var testBlockTimestamp: Long = System.currentTimeMillis()

  def setChainParams(request: SetChainParamsRequest): ServiceResponse[SetChainParamsResponse] = {
    val newBlockchainConfig = new BlockchainConfig {
      override val frontierBlockNumber: BigInt = initialBlockchainConfig.frontierBlockNumber
      override val homesteadBlockNumber: BigInt = request.chainParams.blockchainParams.homesteadForkBlock
      override val eip106BlockNumber: BigInt = initialBlockchainConfig.eip106BlockNumber
      override val eip150BlockNumber: BigInt = request.chainParams.blockchainParams.EIP150ForkBlock
      override val eip155BlockNumber: BigInt = initialBlockchainConfig.eip155BlockNumber
      override val eip160BlockNumber: BigInt = initialBlockchainConfig.eip160BlockNumber
      override val eip161BlockNumber: BigInt = initialBlockchainConfig.eip161BlockNumber
      override val maxCodeSize: Option[BigInt] = initialBlockchainConfig.maxCodeSize
      override val difficultyBombPauseBlockNumber: BigInt = initialBlockchainConfig.difficultyBombPauseBlockNumber
      override val difficultyBombContinueBlockNumber: BigInt = initialBlockchainConfig.difficultyBombContinueBlockNumber
      override val customGenesisFileOpt: Option[String] = initialBlockchainConfig.customGenesisFileOpt
      override val daoForkConfig: Option[DaoForkConfig] = initialBlockchainConfig.daoForkConfig
      override val accountStartNonce: UInt256 = UInt256(request.chainParams.blockchainParams.accountStartNonce)
      override val chainId: Byte = initialBlockchainConfig.chainId
      override val monetaryPolicyConfig: MonetaryPolicyConfig = initialBlockchainConfig.monetaryPolicyConfig
      override val gasTieBreaker: Boolean = initialBlockchainConfig.gasTieBreaker
      override val ethCompatibleStorage: Boolean = initialBlockchainConfig.ethCompatibleStorage
    }

    val genesisData = GenesisData(
      nonce = ByteString(Hex.decode("00")),
      mixHash = None,
      difficulty = "0",
      extraData = request.chainParams.genesis.extraData,
      gasLimit = "0x" + request.chainParams.genesis.gasLimit.toString(16),
      coinbase = request.chainParams.genesis.author,
      timestamp = Hex.toHexString(request.chainParams.genesis.timestamp.toArray[Byte]),
      alloc = request.chainParams.accounts.map { case (addr, acc) => Hex.toHexString(addr.toArray[Byte]) -> AllocAccount(acc.wei.toString) })

    // remove current genesis (Try because it may not exist)
    Try(blockchain.removeBlock(blockchain.genesisHeader.hash, saveParentAsBestBlock = false))

    // load the new genesis
    val genesisDataLoader = new GenesisDataLoader(blockchain, newBlockchainConfig)
    genesisDataLoader.loadGenesisData(genesisData)

    // update ledger with new config
    setupLedger(newBlockchainConfig)

    Future.successful(Right(SetChainParamsResponse()))
  }

  def mineBlocks(request: MineBlocksRequest): ServiceResponse[MineBlocksResponse] = {
    def mineBlock(): Future[Unit] = {
      getBlockForMining(blockchain.getBestBlock()).map { blockForMining =>
        ledgerHolder().importBlock(blockForMining.block) // TODO: check result?
        pendingTransactionsManager ! PendingTransactionsManager.ClearPendingTransactions
        testBlockTimestamp += 1
      }
    }

    def doNTimesF(n: Int)(fn: () => Future[Unit]): Future[Unit] = fn().flatMap { res =>
      if (n <= 1) Future.successful(res)
      else doNTimesF(n - 1)(fn)
    }

    doNTimesF(request.num)(mineBlock _).map(_ => Right(MineBlocksResponse()))
  }

  def modifyTimestamp(request: ModifyTimestampRequest): ServiceResponse[ModifyTimestampResponse] = {
    testBlockTimestamp = request.timestamp
    Future.successful(Right(ModifyTimestampResponse()))
  }

  def rewindToBlock(request: RewindToBlockRequest): ServiceResponse[RewindToBlockResponse] = {
    pendingTransactionsManager ! PendingTransactionsManager.ClearPendingTransactions
    (blockchain.getBestBlockNumber() until request.blockNum by -1).foreach { n =>
      blockchain.removeBlock(blockchain.getBlockHeaderByNumber(n).get.hash, saveParentAsBestBlock = false)
    }
    blockchain.saveBestBlockNumber(request.blockNum)
    Future.successful(Right(RewindToBlockResponse()))
  }

  def setEtherbase(req: SetEtherbaseRequest): ServiceResponse[SetEtherbaseResponse] = {
    etherbase = req.etherbase
    Future.successful(Right(SetEtherbaseResponse()))
  }

  private def getBlockForMining(parentBlock: Block): Future[PendingBlock] = {
    val blockGenerator = new BlockGenerator(
      blockchain, blockchainConfig, consensusConfig.headerExtraData, 5, ledgerHolder, validators, new BlockTimestampProvider {
        override def getEpochSecond: Long = testBlockTimestamp
      }
    )

    implicit val timeout = Timeout(5.seconds)
    (pendingTransactionsManager ? PendingTransactionsManager.GetPendingTransactions)
      .mapTo[PendingTransactionsResponse]
      .flatMap { pendingTxs =>
        blockGenerator.generateBlockForMining(parentBlock, pendingTxs.pendingTransactions.map(_.stx), Nil, etherbase) match {
          case Right(pb) => Future.successful(pb)
          case Left(err) => Future.failed(new RuntimeException(s"Error while generating block for mining: $err"))
        }
      }
  }

  private def setupLedger(newBlockchainConfig: BlockchainConfig): Unit = {
    blockchainConfig = newBlockchainConfig
    ledgerHolder.send(new LedgerImpl(blockchain, new BlockQueue(blockchain, 5, 5), blockchainConfig, prepareConsensus()))
  }

  private def testCnsensus(): Consensus = {
    new Consensus {
      /**
        * The type of configuration [[io.iohk.ethereum.consensus.FullConsensusConfig#specific specific]]
        * to this consensus protocol implementation.
        */
      override type Config = this.type

      override def protocol: Protocol = ???

      override def config: FullConsensusConfig[this.type] = ???

      /**
        * This is the VM used while preparing and generating blocks.
        */
      override def vm: VMImpl = ???

      /**
        * Provides the set of validators specific to this consensus protocol.
        */
      override def validators: Validators = ???
      override def blockPreparator: BlockPreparator = consensus.blockPreparator
      override def blockGenerator: BlockGenerator = consensus.blockGenerator
      override def startProtocol(node: Node): Unit = consensus.startProtocol(node)
      override def stopProtocol(): Unit = consensus.stopProtocol()
    }
  }
}
