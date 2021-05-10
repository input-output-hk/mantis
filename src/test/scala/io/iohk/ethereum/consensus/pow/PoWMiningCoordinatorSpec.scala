package io.iohk.ethereum.consensus.pow

import akka.actor.ActorRef
import akka.actor.testkit.typed.LoggingEvent
import akka.actor.testkit.typed.scaladsl.{LoggingTestKit, ScalaTestWithActorTestKit}
import akka.actor.typed.scaladsl.adapter._
import akka.testkit.{TestActor, TestProbe}
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.blockchain.sync.SyncProtocol.MinedBlock
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator._
import io.iohk.ethereum.domain.{Block, UInt256}
import io.iohk.ethereum.jsonrpc.EthMiningService.SubmitHashRateResponse
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager
import monix.eval.Task
import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class PoWMiningCoordinatorSpec extends ScalaTestWithActorTestKit with AnyFlatSpecLike with Matchers {

  "PoWMinerCoordinator actor" should "throw exception when starting with other message than StartMining(mode)" in new TestSetup(
    "FailedCoordinator"
  ) {
    LoggingTestKit.error("StopMining").expect {
      coordinator ! StopMining
    }
  }

  it should "start recurrent mining when receiving message StartMining(RecurrentMining)" in new TestSetup(
    "RecurrentMining"
  ) {
    setBlockForMining(parentBlock)
    LoggingTestKit.info("Received message StartMining(RecurrentMining)").expect {
      coordinator ! StartMining(RecurrentMining)
    }
    coordinator ! StopMining
  }

  it should "start on demand mining when receiving message StartMining(OnDemandMining)" in new TestSetup(
    "OnDemandMining"
  ) {
    LoggingTestKit.info("Received message StartMining(OnDemandMining)").expect {
      coordinator ! StartMining(OnDemandMining)
    }
    coordinator ! StopMining
  }

  it should "[Recurrent Mining] ProcessMining starts EthashMiner if mineWithKeccak is false" in new TestSetup(
    "EthashMining"
  ) {
    (blockchain.getBestBlock _).expects().returns(Some(parentBlock)).anyNumberOfTimes()
    setBlockForMining(parentBlock)
    LoggingTestKit.info("Spawning an EthashMiner").expect {
      coordinator ! StartMining(RecurrentMining)
    }

    coordinator ! StopMining
  }

  it should "[Recurrent Mining] ProcessMining starts KeccakMiner if mineWithKeccak is true" in new TestSetup(
    "KeccakMining"
  ) {
    override val coordinator = system.systemActorOf(
      PoWMiningCoordinator(
        sync.ref,
        ethMiningService,
        blockCreator,
        blockchain,
        Some(0)
      ),
      "KeccakMining"
    )
    (blockchain.getBestBlock _).expects().returns(Some(parentBlock)).anyNumberOfTimes()
    setBlockForMining(parentBlock)

    LoggingTestKit
      .info("Spawning a KeccakMiner")
      .withCustom { case msg: LoggingEvent =>
        coordinator ! StopMining
        true
      }
      .expect {
        coordinator ! StartMining(RecurrentMining)
      }
  }

  it should "[Recurrent Mining] Miners mine recurrently" in new TestSetup(
    "RecurrentMining"
  ) {
    override val coordinator = testKit.spawn(
      PoWMiningCoordinator(
        sync.ref,
        ethMiningService,
        blockCreator,
        blockchain,
        Some(0)
      ),
      "AutomaticMining"
    )

    (blockchain.getBestBlock _).expects().returns(Some(parentBlock)).anyNumberOfTimes()
    setBlockForMining(parentBlock)
    coordinator ! StartMining(RecurrentMining)

    sync.expectMsgType[MinedBlock]
    sync.expectMsgType[MinedBlock]
    sync.expectMsgType[MinedBlock]

    coordinator ! StopMining
  }

  it should "[Recurrent Mining] Continue to attempt to mine if blockchain.getBestBlock() return None" in new TestSetup(
    "AlwaysMine"
  ) {
    override val coordinator = testKit.spawn(
      PoWMiningCoordinator(
        sync.ref,
        ethMiningService,
        blockCreator,
        blockchain,
        Some(0)
      ),
      "AlwaysAttemptToMine"
    )

    (blockchain.getBestBlock _).expects().returns(None).twice()
    (blockchain.getBestBlock _).expects().returns(Some(parentBlock)).anyNumberOfTimes()

    setBlockForMining(parentBlock)
    coordinator ! StartMining(RecurrentMining)

    sync.expectMsgType[MinedBlock]
    sync.expectMsgType[MinedBlock]
    sync.expectMsgType[MinedBlock]

    coordinator ! StopMining
  }

  it should "[Recurrent Mining] StopMining stops PoWMinerCoordinator" in new TestSetup("StoppingMining") {
    val probe = TestProbe()
    override val coordinator = testKit.spawn(
      PoWMiningCoordinator(
        sync.ref,
        ethMiningService,
        blockCreator,
        blockchain,
        Some(0)
      ),
      "StoppingMining"
    )
    probe.watch(coordinator.ref.toClassic)

    (blockchain.getBestBlock _).expects().returns(Some(parentBlock)).anyNumberOfTimes()
    setBlockForMining(parentBlock)
    coordinator ! StartMining(RecurrentMining)
    coordinator ! StopMining

    probe.expectTerminated(coordinator.ref.toClassic)
  }

  class TestSetup(coordinatorName: String) extends MinerSpecSetup {
    override lazy val consensus: PoWConsensus = buildPoWConsensus().withBlockGenerator(blockGenerator)

    val parentBlockNumber: Int = 23499
    override val origin: Block = Block(
      Fixtures.Blocks.Genesis.header.copy(
        difficulty = UInt256(Hex.decode("0400")).toBigInt,
        number = 0,
        gasUsed = 0,
        unixTimestamp = 0
      ),
      Fixtures.Blocks.ValidBlock.body
    )

    val parentBlock: Block = origin.copy(header = origin.header.copy(number = parentBlockNumber))

    val getTransactionFromPoolTimeout: FiniteDuration = 5.seconds

    override val blockCreator = new PoWBlockCreator(
      pendingTransactionsManager = pendingTransactionsManager.ref,
      getTransactionFromPoolTimeout = getTransactionFromPoolTimeout,
      consensus = consensus,
      ommersPool = ommersPool.ref
    )

    val coordinator = testKit.spawn(
      PoWMiningCoordinator(
        sync.ref,
        ethMiningService,
        blockCreator,
        blockchain,
        None
      ),
      coordinatorName
    )

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
}
