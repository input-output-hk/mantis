package io.iohk.ethereum.consensus.ethash

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.blockchain.sync.{ScenarioSetup, SyncProtocol}
import io.iohk.ethereum.consensus.blocks.{PendingBlock, PendingBlockAndState}
import io.iohk.ethereum.consensus.ethash.blocks.EthashBlockGenerator
import io.iohk.ethereum.consensus.ethash.difficulty.EthashDifficultyCalculator
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ledger.Ledger.VMImpl
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}

abstract class MinerSpecSetup(implicit system: ActorSystem) extends ScenarioSetup with MockFactory {

  def miner: TestActorRef[Nothing]

  val origin =
    Block(Fixtures.Blocks.Genesis.header, Fixtures.Blocks.Genesis.body)

  val sync = TestProbe()

  def waitForMinedBlock(implicit timeout: Duration): Block = {
    sync.expectMsgPF[Block](timeout) { case m: SyncProtocol.MinedBlock =>
      m.block
    }
  }

  def expectNoNewBlockMsg(timeout: FiniteDuration): Unit = {
    sync.expectNoMessage(timeout)
  }

  private def calculateGasLimit(parentGas: UInt256): UInt256 = {
    val GasLimitBoundDivisor: Int = 1024

    val gasLimitDifference = parentGas / GasLimitBoundDivisor
    parentGas + gasLimitDifference - 1
  }

  val blockGenerator: EthashBlockGenerator = mock[EthashBlockGenerator]

  override lazy val blockchain: BlockchainImpl = mock[BlockchainImpl]
  override lazy val vm: VMImpl = new VMImpl
  override lazy val consensus: EthashConsensus = buildEthashConsensus().withBlockGenerator(blockGenerator)

  val blockCreator = mock[EthashBlockCreator]

  val difficultyCalc = new EthashDifficultyCalculator(blockchainConfig)

  val blockForMiningTimestamp = System.currentTimeMillis()

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

  def blockForMining(parentHeader: BlockHeader, transactions: Seq[SignedTransaction] = Seq(txToMine)): Block = {
    Block(
      BlockHeader(
        parentHash = parentHeader.hash,
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = consensusConfig.coinbase.bytes,
        stateRoot = parentHeader.stateRoot,
        transactionsRoot = parentHeader.transactionsRoot,
        receiptsRoot = parentHeader.receiptsRoot,
        logsBloom = parentHeader.logsBloom,
        difficulty = difficultyCalc.calculateDifficulty(1, blockForMiningTimestamp, parentHeader),
        number = BigInt(1),
        gasLimit = calculateGasLimit(UInt256(parentHeader.gasLimit)),
        gasUsed = BigInt(0),
        unixTimestamp = blockForMiningTimestamp,
        extraData = consensusConfig.headerExtraData,
        mixHash = ByteString.empty,
        nonce = ByteString.empty
      ),
      BlockBody(transactions, Nil)
    )
  }

  val parentActor = TestProbe()

  val fakeWorld = mock[InMemoryWorldStateProxy]

  def blockCreatorBehaviour(parentBlock: Block, withTransactions: Boolean, resultBlock: Block) = {
    (blockCreator
      .getBlockForMining(_: Block, _: Boolean, _: Option[InMemoryWorldStateProxy]))
      .expects(parentBlock, withTransactions, *)
      .returning(
        Future
          .successful(PendingBlockAndState(PendingBlock(resultBlock, Nil), fakeWorld))
      )
      .atLeastOnce()
  }

  def blockCreatorBehaviourExpectingInitialWorld(parentBlock: Block, withTransactions: Boolean, resultBlock: Block) = {
    (blockCreator
      .getBlockForMining(_: Block, _: Boolean, _: Option[InMemoryWorldStateProxy]))
      .expects(where { (parent, withTxs, _) =>
        parent == parentBlock && withTxs == withTransactions
      })
      .returning(
        Future
          .successful(PendingBlockAndState(PendingBlock(resultBlock, Nil), fakeWorld))
      )
      .atLeastOnce()
  }

  def withStartedMiner(behaviour: => Unit) = {
    miner ! MinerProtocol.StartMining

    behaviour

    miner ! MinerProtocol.StopMining
  }

  def sendToMiner(msg: MinerProtocol) = {
    miner.tell(msg, parentActor.ref)
  }

}
