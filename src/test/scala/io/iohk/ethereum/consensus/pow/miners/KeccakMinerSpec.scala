package io.iohk.ethereum.consensus.pow.miners

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Timeouts
import io.iohk.ethereum.consensus.pow.EthashUtils
import io.iohk.ethereum.consensus.pow.MinerSpecSetup
import io.iohk.ethereum.consensus.pow.PoWBlockCreator
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.MiningSuccessful
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.MiningUnsuccessful
import io.iohk.ethereum.consensus.pow.validators.PoWBlockHeaderValidator
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.jsonrpc.EthInfoService
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config

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

  trait TestSetup extends ScalaTestWithActorTestKit with MinerSpecSetup {
    implicit private val durationTimeout: Duration = Timeouts.miningTimeout

    implicit override lazy val blockchainConfig: BlockchainConfig = Config.blockchains.blockchainConfig
      .withUpdatedForkBlocks(_.copy(ecip1049BlockNumber = Some(0)))

    val ethService: EthInfoService = mock[EthInfoService]
    val getTransactionFromPoolTimeout: FiniteDuration = 5.seconds

    override val blockCreator = new PoWBlockCreator(
      pendingTransactionsManager = pendingTransactionsManager.ref,
      getTransactionFromPoolTimeout = getTransactionFromPoolTimeout,
      mining = mining,
      ommersPool = ommersPool.ref
    )

    val miner = new KeccakMiner(blockCreator, sync.ref, ethMiningService)

    protected def executeTest(parentBlock: Block): Unit = {
      prepareMocks()
      val minedBlock = startMining(parentBlock)
      checkAssertions(minedBlock, parentBlock)
    }

    def startMining(parentBlock: Block): Block =
      eventually {
        miner.processMining(parentBlock).map {
          case MiningSuccessful   => true
          case MiningUnsuccessful => startMining(parentBlock)
        }
        val minedBlock = waitForMinedBlock
        minedBlock
      }

    private def checkAssertions(minedBlock: Block, parentBlock: Block): Unit = {
      minedBlock.body.transactionList shouldBe Seq(txToMine)
      minedBlock.header.nonce.length shouldBe 8
      PoWBlockHeaderValidator.validate(minedBlock.header, parentBlock.header) shouldBe Right(BlockHeaderValid)
    }
  }
}
