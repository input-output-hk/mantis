package io.iohk.ethereum.consensus.pow.miners

import akka.actor.testkit.typed.scaladsl.TestInbox
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActor, TestActorRef, TestKit, TestProbe}
import io.iohk.ethereum.consensus.blocks.{PendingBlock, PendingBlockAndState}
import io.iohk.ethereum.consensus.pow.PoWMinerCoordinator.CoordinatorProtocol
import io.iohk.ethereum.consensus.pow.validators.PoWBlockHeaderValidator
import io.iohk.ethereum.consensus.pow.{EthashUtils, PoWBlockCreator, PoWMinerCoordinator, PoWMinerCoordinatorSpec}
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthMiningService.SubmitHashRateResponse
import io.iohk.ethereum.jsonrpc.{EthInfoService, EthMiningService}
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.{Fixtures, WithActorSystemShutDown}
import monix.eval.Task
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class EthashMinerSpec
    extends TestKit(ActorSystem("EthashMinerSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers {

  private implicit val timeout: Duration = 10.minutes

  final val PoWMinerSpecTag = Tag("PowMinerSpec") // TODO Add tag // taggedAs PoWMinerSpecTag

  "EthashMiner" should "mine valid blocks" in new TestSetup {
    val parentBlock: Block = origin
    val bfm: Block = blockForMining(parentBlock.header)

    executeTest(parentBlock, bfm)
  }

  it should "mine valid block on the beginning of the new epoch" in new TestSetup {
    val epochLength: Int = EthashUtils.EPOCH_LENGTH_BEFORE_ECIP_1099
    val parentBlockNumber: Int = epochLength - 1 // 29999, mined block will be 30000 (first block of the new epoch)
    val parentBlock: Block = origin.copy(header = origin.header.copy(number = parentBlockNumber))
    val bfm: Block = blockForMining(parentBlock.header)

    executeTest(parentBlock, bfm)
  }

  it should "mine valid blocks on the end of the epoch" in new TestSetup {
    val epochLength: Int = EthashUtils.EPOCH_LENGTH_BEFORE_ECIP_1099
    val parentBlockNumber: Int =
      2 * epochLength - 2 // 59998, mined block will be 59999 (last block of the current epoch)
    val parentBlock: Block = origin.copy(header = origin.header.copy(number = parentBlockNumber))
    val bfm: Block = blockForMining(parentBlock.header)

    executeTest(parentBlock, bfm)
  }

  // TODO handle duplicated code with KeccakMinerSpec
  trait TestSetup extends MinerSpecSetup with MockFactory {

    override val origin: Block = Block(
      Fixtures.Blocks.Genesis.header.copy(
        difficulty = UInt256(Hex.decode("0400")).toBigInt,
        number = 0,
        gasUsed = 0,
        unixTimestamp = 0
      ),
      Fixtures.Blocks.ValidBlock.body
    )

    val powBlockHeaderValidator = new PoWBlockHeaderValidator(blockchainConfig)

    val coordinatorRef = TestInbox[CoordinatorProtocol]("coordinator")
    val ommersPool: TestProbe = TestProbe()
    val pendingTransactionsManager: TestProbe = TestProbe()

    val ethService: EthInfoService = mock[EthInfoService]
    val ethMiningService: EthMiningService = mock[EthMiningService]
    val getTransactionFromPoolTimeout: FiniteDuration = 5.seconds

    override val blockCreator = new PoWBlockCreator(
      pendingTransactionsManager = pendingTransactionsManager.ref,
      getTransactionFromPoolTimeout = getTransactionFromPoolTimeout,
      consensus = consensus,
      ommersPool = ommersPool.ref
    )

    val miner: TestActorRef[Nothing] = TestActorRef(
      EthashMiner.props(
        blockCreator,
        sync.ref,
        ethService,
        ethMiningService
      )
    )

    protected def executeTest(parentBlock: Block, blockForMining: Block): Unit = {
      prepareMocks(parentBlock, blockForMining)
      val minedBlock = startMining(parentBlock, coordinatorRef.ref)
      checkAssertions(minedBlock, parentBlock)
    }

    private def prepareMocks(parentBlock: Block, blockForMining: Block): Unit = {
      (ethMiningService.submitHashRate _)
        .expects(*)
        .returns(Task.now(Right(SubmitHashRateResponse(true))))
        .atLeastOnce()
      (blockGenerator.generateBlock _)
        .expects(parentBlock, Nil, consensusConfig.coinbase, Nil, None)
        .returning(PendingBlockAndState(PendingBlock(blockForMining, Nil), fakeWorld))
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

    private def startMining(parentBlock: Block, replyTo: akka.actor.typed.ActorRef[CoordinatorProtocol]): Block = {
      miner ! MinerProtocol.ProcessMining(parentBlock, replyTo)
      val block = waitForMinedBlock
      block
    }

    private def checkAssertions(minedBlock: Block, parentBlock: Block): Unit = {
      minedBlock.body.transactionList shouldBe Seq(txToMine)
      minedBlock.header.nonce.length shouldBe 8
      powBlockHeaderValidator.validate(minedBlock.header, parentBlock.header) shouldBe Right(BlockHeaderValid)
      coordinatorRef.expectMessage(PoWMinerCoordinator.MiningCompleted)
    }
  }
}
