package io.iohk.ethereum.consensus.pow.miners

import akka.actor.{ActorSystem => ClassicSystem}
import akka.testkit.TestActorRef
import akka.testkit.TestKit

import monix.eval.Task

import scala.concurrent.duration._

import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import io.iohk.ethereum.WithActorSystemShutDown
import io.iohk.ethereum.consensus.pow.MinerSpecSetup
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MineBlocks
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponses._
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.ByteStringUtils

class MockedMinerSpec
    extends TestKit(ClassicSystem("MockedPowMinerSpec_System"))
    with AnyWordSpecLike
    with Matchers
    with WithActorSystemShutDown {

  implicit private val timeout: Duration = 1.minute

  "MockedPowMiner actor" should {
    "not mine blocks" when {
      "there is no request" in new TestSetup {
        expectNoNewBlockMsg(noMessageTimeOut)
      }
    }

    "not mine block and return MinerNotSupport msg" when {
      "the request comes before miner started" in new TestSetup {
        val msg = MineBlocks(1, false, None)
        sendToMiner(msg)
        expectNoNewBlockMsg(noMessageTimeOut)
        parentActor.expectMsg(MinerNotSupported(msg))
      }
    }

    "stop mining in case of error" when {
      "Unable to get block for mining" in new TestSetup {
        val parent = origin
        val bfm1 = setBlockForMining(parent, Seq.empty)

        blockCreatorBehaviour(parent, withTransactions = false, bfm1)

        (blockCreator
          .getBlockForMining(_: Block, _: Boolean, _: Option[InMemoryWorldStateProxy])(_: BlockchainConfig))
          .expects(bfm1, false, *, *)
          .returning(
            Task.raiseError(new RuntimeException("error"))
          )
          .atLeastOnce()

        withStartedMiner {
          sendToMiner(MineBlocks(2, withTransactions = false, None))

          parentActor.expectMsg(MiningOrdered)

          val block1 = waitForMinedBlock

          expectNoNewBlockMsg(noMessageTimeOut)

          parentActor.expectNoMessage(noMessageTimeOut)

          validateBlock(block1, parent)
        }
      }

      "Unable to get parent block for mining" in new TestSetup {
        val parentHash = origin.hash

        val errorMsg = s"Unable to get parent block with hash ${ByteStringUtils.hash2string(parentHash)} for mining"

        (blockchainReader.getBlockByHash _).expects(parentHash).returns(None)

        withStartedMiner {
          sendToMiner(MineBlocks(2, withTransactions = false, Some(parentHash)))

          expectNoNewBlockMsg(noMessageTimeOut)

          parentActor.expectMsg(MiningError(errorMsg))
        }
      }
    }

    "return MinerIsWorking to requester" when {
      "miner is working during next mine request" in new TestSetup {
        val parent = origin
        val bfm = setBlockForMining(parent, Seq.empty)

        blockCreatorBehaviour(parent, withTransactions = false, bfm)

        withStartedMiner {
          sendToMiner(MineBlocks(1, withTransactions = false, None))
          parentActor.expectMsg(MiningOrdered)
          sendToMiner(MineBlocks(1, withTransactions = false, None))
          parentActor.expectMsg(MinerIsWorking)

          val block = waitForMinedBlock

          expectNoNewBlockMsg(noMessageTimeOut)

          validateBlock(block, parent)
        }
      }
    }

    "mine valid blocks" when {
      "there is request for block with other parent than best block" in new TestSetup {
        val parent = origin
        val parentHash = origin.hash
        val bfm = setBlockForMining(parent, Seq.empty)

        (blockchainReader.getBlockByHash _).expects(parentHash).returns(Some(parent))

        blockCreatorBehaviour(parent, withTransactions = false, bfm)

        withStartedMiner {
          sendToMiner(MineBlocks(1, withTransactions = false, Some(parentHash)))

          parentActor.expectMsg(MiningOrdered)

          val block = waitForMinedBlock

          validateBlock(block, parent)
        }
      }

      "there is request for one block without transactions" in new TestSetup {
        val parent = origin
        val bfm = setBlockForMining(parent, Seq.empty)

        blockCreatorBehaviour(parent, withTransactions = false, bfm)

        withStartedMiner {
          sendToMiner(MineBlocks(1, withTransactions = false, None))

          val block = waitForMinedBlock

          parentActor.expectMsg(MiningOrdered)

          validateBlock(block, parent)
        }
      }

      "there is request for one block with transactions" in new TestSetup {
        val parent = origin
        val bfm = setBlockForMining(parent)

        blockCreatorBehaviour(parent, withTransactions = true, bfm)

        withStartedMiner {
          sendToMiner(MineBlocks(1, withTransactions = true, None))

          val block = waitForMinedBlock

          parentActor.expectMsg(MiningOrdered)

          validateBlock(block, parent, Seq(txToMine))
        }
      }

      "there is request for few blocks without transactions" in new TestSetup {
        val parent = origin
        val bfm1 = setBlockForMining(parent, Seq.empty)
        val bfm2 = setBlockForMining(bfm1, Seq.empty)

        blockCreatorBehaviour(parent, withTransactions = false, bfm1)

        blockCreatorBehaviourExpectingInitialWorld(bfm1, withTransactions = false, bfm2)

        withStartedMiner {
          sendToMiner(MineBlocks(2, withTransactions = false, None))

          val block1 = waitForMinedBlock
          val block2 = waitForMinedBlock

          parentActor.expectMsg(MiningOrdered)

          validateBlock(block1, parent)
          validateBlock(block2, block1)
        }
      }

      "there is request for few blocks with transactions" in new TestSetup {
        val parent = origin
        val bfm1 = setBlockForMining(parent)
        val bfm2 = setBlockForMining(bfm1, Seq.empty)

        blockCreatorBehaviour(parent, withTransactions = true, bfm1)

        blockCreatorBehaviourExpectingInitialWorld(bfm1, withTransactions = true, bfm2)

        withStartedMiner {
          sendToMiner(MineBlocks(2, withTransactions = true, None))

          val block1 = waitForMinedBlock

          val block2 = waitForMinedBlock

          parentActor.expectMsg(MiningOrdered)

          validateBlock(block1, parent, Seq(txToMine))
          validateBlock(block2, block1)
        }
      }
    }
  }

  class TestSetup(implicit system: ClassicSystem) extends MinerSpecSetup {
    val noMessageTimeOut: FiniteDuration = 3.seconds

    val miner: TestActorRef[Nothing] = TestActorRef(
      MockedMiner.props(
        blockchain,
        blockchainReader,
        blockCreator,
        sync.ref,
        this
      )
    )

    (blockchainReader.getBestBlock _).expects().returns(Some(origin))

    def validateBlock(block: Block, parent: Block, txs: Seq[SignedTransaction] = Seq.empty): Assertion = {
      block.body.transactionList shouldBe txs
      block.header.nonce.length shouldBe 0
      block.header.parentHash shouldBe parent.hash
    }

    protected def withStartedMiner(behaviour: => Unit): Unit = {
      miner ! MinerProtocol.StartMining
      behaviour
      miner ! MinerProtocol.StopMining
    }

    protected def sendToMiner(msg: MinerProtocol): Unit =
      miner.tell(msg, parentActor.ref)
  }
}
