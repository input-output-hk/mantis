package io.iohk.ethereum.consensus.pow.miners

import scala.concurrent.duration._

import org.bouncycastle.util.encoders.Hex
import org.scalatest.Tag
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.MiningPatience
import io.iohk.ethereum.consensus.pow.EthashUtils
import io.iohk.ethereum.consensus.pow.MinerSpecSetup
import io.iohk.ethereum.consensus.pow.PoWBlockCreator
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.MiningSuccessful
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.MiningUnsuccessful
import io.iohk.ethereum.consensus.pow.validators.PoWBlockHeaderValidator
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.domain._

class EthashMinerSpec extends AnyFlatSpec with Matchers {
  final val PoWMinerSpecTag: Tag = Tag("EthashMinerSpec")

  "EthashMiner actor" should "mine valid blocks" taggedAs PoWMinerSpecTag in new TestSetup {
    val parentBlock: Block = origin
    setBlockForMining(origin)

    executeTest(parentBlock)
  }

  it should "mine valid block on the end and beginning of the new epoch" taggedAs PoWMinerSpecTag in new TestSetup {
    val epochLength: Int = EthashUtils.EPOCH_LENGTH_BEFORE_ECIP_1099
    val parent29998: Int = epochLength - 2 // 29998, mined block will be 29999 (last block of the epoch)
    val parentBlock29998: Block = origin.copy(header = origin.header.copy(number = parent29998))
    setBlockForMining(parentBlock29998)
    executeTest(parentBlock29998)

    val parent29999: Int = epochLength - 1 // 29999, mined block will be 30000 (first block of the new epoch)
    val parentBlock29999: Block = origin.copy(header = origin.header.copy(number = parent29999))
    setBlockForMining(parentBlock29999)
    executeTest(parentBlock29999)
  }

  it should "mine valid blocks on the end of the epoch" taggedAs PoWMinerSpecTag in new TestSetup {
    val epochLength: Int = EthashUtils.EPOCH_LENGTH_BEFORE_ECIP_1099
    val parentBlockNumber: Int =
      2 * epochLength - 2 // 59998, mined block will be 59999 (last block of the current epoch)
    val parentBlock: Block = origin.copy(header = origin.header.copy(number = parentBlockNumber))
    setBlockForMining(parentBlock)

    executeTest(parentBlock)
  }

  class TestSetup extends MinerSpecSetup with Eventually with MiningPatience {
    override val origin: Block = Block(
      Fixtures.Blocks.Genesis.header.copy(
        difficulty = UInt256(Hex.decode("0400")).toBigInt,
        number = 0,
        gasUsed = 0,
        unixTimestamp = 0
      ),
      Fixtures.Blocks.ValidBlock.body
    )

    val powBlockHeaderValidator = new PoWBlockHeaderValidator()
    val getTransactionFromPoolTimeout: FiniteDuration = 5.seconds

    override val blockCreator = new PoWBlockCreator(
      pendingTransactionsManager = pendingTransactionsManager.ref,
      getTransactionFromPoolTimeout = getTransactionFromPoolTimeout,
      consensus = consensus,
      ommersPool = ommersPool.ref
    )

    val dagManager = new EthashDAGManager(blockCreator)
    val miner = new EthashMiner(
      dagManager,
      blockCreator,
      sync.ref,
      ethMiningService
    )

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
      powBlockHeaderValidator.validate(minedBlock.header, parentBlock.header) shouldBe Right(BlockHeaderValid)
    }
  }
}
