package io.iohk.ethereum.consensus
package ethash

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActor, TestActorRef, TestKit, TestProbe}
import io.iohk.ethereum.{Fixtures, WithActorSystemShutDown}
import io.iohk.ethereum.consensus.blocks.{PendingBlock, PendingBlockAndState}
import io.iohk.ethereum.consensus.ethash.validators.EthashBlockHeaderValidator
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.{EthService, EthMiningService}
import io.iohk.ethereum.jsonrpc.EthMiningService.SubmitHashRateResponse
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager
import monix.eval.Task
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.Tag

import scala.concurrent.duration._
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class EthashMinerSpec
    extends TestKit(ActorSystem("EthashMinerSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers {

  final val EthashMinerSpecTag = Tag("EthashMinerSpec")

  private implicit val timeout: Duration = 10.minutes

  "EthashMiner" should "mine valid blocks" taggedAs (EthashMinerSpecTag) in new TestSetup {
    val parent = origin
    val bfm = blockForMining(parent.header)

    (blockchain.getBestBlock _).expects().returns(parent).anyNumberOfTimes()
    (ethMiningService.submitHashRate _)
      .expects(*)
      .returns(Task.now(Right(SubmitHashRateResponse(true))))
      .atLeastOnce()
    (blockGenerator.generateBlock _)
      .expects(parent, Nil, consensusConfig.coinbase, Nil, None)
      .returning(PendingBlockAndState(PendingBlock(bfm, Nil), fakeWorld))
      .atLeastOnce()

    ommersPool.setAutoPilot((sender: ActorRef, _: Any) => {
      sender ! OmmersPool.Ommers(Nil)
      TestActor.KeepRunning
    })

    pendingTransactionsManager.setAutoPilot((sender: ActorRef, _: Any) => {
      sender ! PendingTransactionsManager.PendingTransactionsResponse(Nil)
      TestActor.KeepRunning
    })

    miner ! MinerProtocol.StartMining

    val block = waitForMinedBlock

    miner ! MinerProtocol.StopMining

    block.body.transactionList shouldBe Seq(txToMine)
    block.header.nonce.length shouldBe 8
    blockHeaderValidator.validate(block.header, parent.header) shouldBe Right(BlockHeaderValid)
  }

  trait TestSetup extends MinerSpecSetup with MockFactory {

    override val origin = Block(
      Fixtures.Blocks.Genesis.header.copy(
        difficulty = UInt256(Hex.decode("0400")).toBigInt,
        number = 0,
        gasUsed = 0,
        unixTimestamp = 0
      ),
      Fixtures.Blocks.ValidBlock.body
    )

    val blockHeaderValidator = new EthashBlockHeaderValidator(blockchainConfig)

    val ommersPool = TestProbe()
    val pendingTransactionsManager = TestProbe()

    val ethService = mock[EthService]
    val ethMiningService = mock[EthMiningService]
    val getTransactionFromPoolTimeout: FiniteDuration = 5.seconds

    override val blockCreator = new EthashBlockCreator(
      pendingTransactionsManager = pendingTransactionsManager.ref,
      getTransactionFromPoolTimeout = getTransactionFromPoolTimeout,
      consensus = consensus,
      ommersPool = ommersPool.ref
    )

    val miner = TestActorRef(EthashMiner.props(blockchain, blockCreator, sync.ref, ethService, ethMiningService))

  }
}
