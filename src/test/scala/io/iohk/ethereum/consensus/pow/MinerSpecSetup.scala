package io.iohk.ethereum.consensus.pow

import akka.actor.{ActorRef, ActorSystem => ClassicSystem}
import akka.testkit.{TestActor, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.consensus.Protocol.NoAdditionalPoWData
import io.iohk.ethereum.consensus.{ConsensusConfigBuilder, FullConsensusConfig}
import io.iohk.ethereum.consensus.blocks.{PendingBlock, PendingBlockAndState}
import io.iohk.ethereum.consensus.pow.blocks.PoWBlockGenerator
import io.iohk.ethereum.consensus.pow.difficulty.EthashDifficultyCalculator
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthMiningService
import io.iohk.ethereum.jsonrpc.EthMiningService.SubmitHashRateResponse
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.utils.Config
import monix.eval.Task
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory

import scala.concurrent.duration.{Duration, FiniteDuration}

trait MinerSpecSetup extends ConsensusConfigBuilder with MockFactory {
  implicit val classicSystem = ClassicSystem()
  val parentActor = TestProbe()
  val sync = TestProbe()
  val ommersPool: TestProbe = TestProbe()
  val pendingTransactionsManager: TestProbe = TestProbe()

  val origin = Block(Fixtures.Blocks.Genesis.header, Fixtures.Blocks.Genesis.body)

  val blockchain: BlockchainImpl = mock[BlockchainImpl]
  val blockCreator = mock[PoWBlockCreator]
  val fakeWorld = mock[InMemoryWorldStateProxy]
  val blockGenerator: PoWBlockGenerator = mock[PoWBlockGenerator]
  val ethMiningService: EthMiningService = mock[EthMiningService]

  lazy val vm: VMImpl = new VMImpl

  val txToMine = SignedTransaction(
    tx = Transaction(
      nonce = BigInt("438553"),
      gasPrice = BigInt("20000000000"),
      gasLimit = BigInt("50000"),
      receivingAddress = Address(ByteString(Hex.decode("3435be928d783b7c48a2c3109cba0d97d680747a"))),
      value = BigInt("108516826677274384"),
      payload = ByteString.empty
    ),
    pointSign = 0x9d.toByte,
    signatureRandom = ByteString(Hex.decode("beb8226bdb90216ca29967871a6663b56bdd7b86cf3788796b52fd1ea3606698")),
    signature = ByteString(Hex.decode("2446994156bc1780cb5806e730b171b38307d5de5b9b0d9ad1f9de82e00316b5")),
    chainId = 0x3d.toByte
  )

  lazy val consensus: PoWConsensus = buildPoWConsensus().withBlockGenerator(blockGenerator)
  lazy val blockchainConfig = Config.blockchains.blockchainConfig
  lazy val difficultyCalc = new EthashDifficultyCalculator(blockchainConfig)
  val blockForMiningTimestamp = System.currentTimeMillis()

  protected def getParentBlock(parentBlockNumber: Int) =
    origin.copy(header = origin.header.copy(number = parentBlockNumber))

  def buildPoWConsensus(): PoWConsensus = {
    val mantisConfig = Config.config
    val specificConfig = EthashConfig(mantisConfig)

    val fullConfig = FullConsensusConfig(consensusConfig, specificConfig)

    val validators = ValidatorsExecutor(blockchainConfig, consensusConfig.protocol)

    val additionalPoWData = NoAdditionalPoWData
    PoWConsensus(vm, blockchain, blockchainConfig, fullConfig, validators, additionalPoWData)
  }

  protected def setBlockForMining(parentBlock: Block, transactions: Seq[SignedTransaction] = Seq(txToMine)): Block = {
    val parentHeader: BlockHeader = parentBlock.header

    val block = Block(
      BlockHeader(
        parentHash = parentHeader.hash,
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = consensusConfig.coinbase.bytes,
        stateRoot = parentHeader.stateRoot,
        transactionsRoot = parentHeader.transactionsRoot,
        receiptsRoot = parentHeader.receiptsRoot,
        logsBloom = parentHeader.logsBloom,
        difficulty = difficultyCalc.calculateDifficulty(1, blockForMiningTimestamp, parentHeader),
        number = parentHeader.number + 1,
        gasLimit = calculateGasLimit(UInt256(parentHeader.gasLimit)),
        gasUsed = BigInt(0),
        unixTimestamp = blockForMiningTimestamp,
        extraData = consensusConfig.headerExtraData,
        mixHash = ByteString.empty,
        nonce = ByteString.empty
      ),
      BlockBody(transactions, Nil)
    )

    (blockGenerator.generateBlock _)
      .expects(parentBlock, Nil, consensusConfig.coinbase, Nil, None)
      .returning(PendingBlockAndState(PendingBlock(block, Nil), fakeWorld))
      .atLeastOnce()

    block
  }

  private def calculateGasLimit(parentGas: UInt256): UInt256 = {
    val GasLimitBoundDivisor: Int = 1024

    val gasLimitDifference = parentGas / GasLimitBoundDivisor
    parentGas + gasLimitDifference - 1
  }

  protected def blockCreatorBehaviour(parentBlock: Block, withTransactions: Boolean, resultBlock: Block) = {
    (blockCreator
      .getBlockForMining(_: Block, _: Boolean, _: Option[InMemoryWorldStateProxy]))
      .expects(parentBlock, withTransactions, *)
      .returning(
        Task.now(PendingBlockAndState(PendingBlock(resultBlock, Nil), fakeWorld))
      )
      .atLeastOnce()
  }

  protected def blockCreatorBehaviourExpectingInitialWorld(
      parentBlock: Block,
      withTransactions: Boolean,
      resultBlock: Block
  ) = {
    (blockCreator
      .getBlockForMining(_: Block, _: Boolean, _: Option[InMemoryWorldStateProxy]))
      .expects(where { (parent, withTxs, _) =>
        parent == parentBlock && withTxs == withTransactions
      })
      .returning(
        Task.now(PendingBlockAndState(PendingBlock(resultBlock, Nil), fakeWorld))
      )
      .atLeastOnce()
  }

  protected def prepareMocks(): Unit = {
    (ethMiningService.submitHashRate _)
      .expects(*)
      .returns(Task.now(Right(SubmitHashRateResponse(true))))
      .atLeastOnce()

    ommersPool.setAutoPilot((sender: ActorRef, _: Any) => {
      sender ! OmmersPool.Ommers(Nil)
      TestActor.KeepRunning
    })

    pendingTransactionsManager.setAutoPilot((sender: ActorRef, _: Any) => {
      sender ! PendingTransactionsManager.PendingTransactionsResponse(Nil)
      TestActor.KeepRunning
    })
  }

  protected def waitForMinedBlock(implicit timeout: Duration): Block = {
    sync.expectMsgPF[Block](timeout) { case m: SyncProtocol.MinedBlock =>
      m.block
    }
  }

  protected def expectNoNewBlockMsg(timeout: FiniteDuration): Unit = {
    sync.expectNoMessage(timeout)
  }
}
