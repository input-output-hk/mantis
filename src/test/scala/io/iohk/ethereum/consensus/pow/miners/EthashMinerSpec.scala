package io.iohk.ethereum.consensus.pow.miners

import akka.actor.{ActorSystem => ClassicSystem}
import akka.actor.typed.scaladsl.adapter._
import akka.testkit.{TestActorRef, TestKit, TestProbe}
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.CoordinatorProtocol
import io.iohk.ethereum.consensus.pow.validators.PoWBlockHeaderValidator
import io.iohk.ethereum.consensus.pow.{EthashUtils, MinerSpecSetup, PoWBlockCreator, PoWMiningCoordinator}
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.domain._
import io.iohk.ethereum.{Fixtures, MiningPatience, Timeouts, WithActorSystemShutDown}
import org.bouncycastle.util.encoders.Hex
import org.scalatest.Tag
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class EthashMinerSpec
    extends TestKit(ClassicSystem("EthashMinerSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers {
  final val PoWMinerSpecTag = Tag("EthashMinerSpec")

  "EthashMiner actor" should "mine valid blocks" taggedAs PoWMinerSpecTag in new TestSetup {
    val parentBlock: Block = origin
    setBlockForMining(origin)

    executeTest(parentBlock)
  }

  it should "mine valid block on the beginning of the new epoch" taggedAs PoWMinerSpecTag in new TestSetup {
    val epochLength: Int = EthashUtils.EPOCH_LENGTH_BEFORE_ECIP_1099
    val parentBlockNumber: Int = epochLength - 1 // 29999, mined block will be 30000 (first block of the new epoch)
    val parentBlock: Block = origin.copy(header = origin.header.copy(number = parentBlockNumber))
    setBlockForMining(parentBlock)

    executeTest(parentBlock)
  }

  it should "mine valid blocks on the end of the epoch" taggedAs PoWMinerSpecTag in new TestSetup {
    val epochLength: Int = EthashUtils.EPOCH_LENGTH_BEFORE_ECIP_1099
    val parentBlockNumber: Int =
      2 * epochLength - 2 // 59998, mined block will be 59999 (last block of the current epoch)
    val parentBlock: Block = origin.copy(header = origin.header.copy(number = parentBlockNumber))
    setBlockForMining(parentBlock)

    executeTest(parentBlock)
  }

  it should "shutdown itself after mining" taggedAs PoWMinerSpecTag in new TestSetup {
    probe.watch(miner.ref)

    val parentBlock: Block = origin
    setBlockForMining(origin)
    prepareMocks()
    miner ! MinerProtocol.ProcessMining(parentBlock, coordinator.ref.toTyped[CoordinatorProtocol])
    coordinator.expectMsgPF(Timeouts.miningTimeout) {
      case PoWMiningCoordinator.MiningSuccessful => true
      case PoWMiningCoordinator.MiningUnsuccessful => false
    }
    probe.expectTerminated(miner.ref)
  }

  class TestSetup(implicit system: ClassicSystem) extends MinerSpecSetup with Eventually with MiningPatience {
    val probe = TestProbe()
    val coordinator = TestProbe()

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
    val getTransactionFromPoolTimeout: FiniteDuration = 5.seconds

    override val blockCreator = new PoWBlockCreator(
      pendingTransactionsManager = pendingTransactionsManager.ref,
      getTransactionFromPoolTimeout = getTransactionFromPoolTimeout,
      consensus = consensus,
      ommersPool = ommersPool.ref
    )

    val dagManager = new EthashDAGManager(blockCreator)
    val miner: TestActorRef[Nothing] = createNewMiner()
    private def createNewMiner(): TestActorRef[Nothing] = TestActorRef(
      EthashMiner.props(
        dagManager,
        blockCreator,
        sync.ref,
        ethMiningService
      )
    )

    protected def executeTest(parentBlock: Block): Unit = {
      prepareMocks()
      val minedBlock = startMining(parentBlock, coordinator.ref.toTyped[CoordinatorProtocol])
      checkAssertions(minedBlock, parentBlock)
    }

    private def startMining(parentBlock: Block, replyTo: akka.actor.typed.ActorRef[CoordinatorProtocol]): Block = {
      miner ! MinerProtocol.ProcessMining(parentBlock, replyTo)
      eventually {
        waitForMiningCompleted(parentBlock) shouldBe true
      }
      val minedBlock = waitForMinedBlock
      minedBlock
    }

    def waitForMiningCompleted(parentBlock: Block)(implicit timeout: Duration): Boolean = {
      // previous miner was stopped
      val miner = createNewMiner()
      coordinator.expectMsgPF(timeout) {
        case PoWMiningCoordinator.MiningSuccessful =>
          true
        case PoWMiningCoordinator.MiningUnsuccessful =>
          miner ! MinerProtocol.ProcessMining(parentBlock, coordinator.ref.toTyped[CoordinatorProtocol])
          false
      }
    }

    private def checkAssertions(minedBlock: Block, parentBlock: Block): Unit = {
      minedBlock.body.transactionList shouldBe Seq(txToMine)
      minedBlock.header.nonce.length shouldBe 8
      powBlockHeaderValidator.validate(minedBlock.header, parentBlock.header) shouldBe Right(BlockHeaderValid)
    }
  }
}
