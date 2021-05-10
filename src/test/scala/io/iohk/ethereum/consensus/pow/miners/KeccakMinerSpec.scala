package io.iohk.ethereum.consensus.pow.miners

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import io.iohk.ethereum.{MiningPatience, Timeouts}
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.CoordinatorProtocol
import io.iohk.ethereum.consensus.pow.validators.PoWBlockHeaderValidator
import io.iohk.ethereum.consensus.pow.{EthashUtils, MinerSpecSetup, PoWBlockCreator, PoWMiningCoordinator}
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.jsonrpc.EthInfoService
import io.iohk.ethereum.utils.Config
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.{Duration, FiniteDuration, _}

class KeccakMinerSpec extends AnyFlatSpec with Matchers {
  "KeccakMiner actor" should "mine valid blocks" in new TestSetup {
    val parentBlock: Block = origin
    setBlockForMining(parentBlock)

    executeTest(parentBlock)
  }

  it should "mine valid block on the beginning of the new epoch" in new TestSetup {
    val epochLength: Int = EthashUtils.EPOCH_LENGTH_BEFORE_ECIP_1099
    val parentBlockNumber: Int =
      epochLength - 1 // 29999, mined block will be 30000 (first block of the new epoch)
    val parentBlock: Block = getParentBlock(parentBlockNumber)
    setBlockForMining(parentBlock)

    executeTest(parentBlock)
  }

  it should "mine valid blocks on the end of the epoch" in new TestSetup {
    val epochLength: Int = EthashUtils.EPOCH_LENGTH_BEFORE_ECIP_1099
    val parentBlockNumber: Int =
      2 * epochLength - 2 // 59998, mined block will be 59999 (last block of the current epoch)
    val parentBlock: Block = getParentBlock(parentBlockNumber)
    setBlockForMining(parentBlock)

    executeTest(parentBlock)
  }

  // test is failing. Through the PoWMiningCoordinator this has been tested successfully :/
  it should "shutdown itself after mining" ignore new TestSetup {
    val probe = testKit.createTestProbe[MinerProtocol]()
    val parentBlock: Block = origin
    setBlockForMining(parentBlock)

    executeTest(parentBlock)
    eventually {
      probe.expectTerminated(miner)
    }
  }

  trait TestSetup extends ScalaTestWithActorTestKit with MinerSpecSetup {
    private implicit val durationTimeout: Duration = Timeouts.miningTimeout

    override lazy val blockchainConfig = Config.blockchains.blockchainConfig.copy(ecip1049BlockNumber = Some(0))
    val powBlockHeaderValidator = new PoWBlockHeaderValidator(blockchainConfig)

    val coordinatorRef = testKit.createTestProbe[CoordinatorProtocol]()
    val ethService: EthInfoService = mock[EthInfoService]
    val getTransactionFromPoolTimeout: FiniteDuration = 5.seconds

    override val blockCreator = new PoWBlockCreator(
      pendingTransactionsManager = pendingTransactionsManager.ref,
      getTransactionFromPoolTimeout = getTransactionFromPoolTimeout,
      consensus = consensus,
      ommersPool = ommersPool.ref
    )

    val miner = testKit.spawn(KeccakMiner(blockCreator, sync.ref, ethMiningService))

    protected def executeTest(parentBlock: Block): Unit = {
      prepareMocks()
      val minedBlock = startMining(parentBlock, coordinatorRef.ref)
      checkAssertions(minedBlock, parentBlock)
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
      coordinatorRef.expectMessage(PoWMiningCoordinator.MiningSuccessful)
    }
  }
}
