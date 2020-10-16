package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestKit
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.consensus.ethash.EthashConfig
import io.iohk.ethereum.consensus.ethash.MinerResponses.MiningOrdered
import io.iohk.ethereum.consensus.ethash.MockedMinerProtocol.MineBlocks
import io.iohk.ethereum.jsonrpc.QAService.{MineBlocksRequest, MineBlocksResponse}
import io.iohk.ethereum.{ByteGenerators, FlatSpecBase, SpecFixtures}
import org.scalamock.scalatest.AsyncMockFactory

import scala.concurrent.Future

class QAServiceSpec
    extends TestKit(ActorSystem("QAServiceSpec_System"))
    with FlatSpecBase
    with SpecFixtures
    with ByteGenerators
    with AsyncMockFactory {

  def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "QAService" should "send msg to miner and return miner's response" in testCaseF { fixture =>
    import fixture._
    (testConsensus.sendMiner _)
      .expects(mineBlocksMsg)
      .returning(Future.successful(MiningOrdered))
      .atLeastOnce()

    qaService.mineBlocks(mineBlocksReq).map(_ shouldBe Right(MineBlocksResponse(MiningOrdered)))
  }

  it should "send msg to miner and return InternalError in case of problems" in testCaseF { fixture =>
    import fixture._
    (testConsensus.sendMiner _)
      .expects(mineBlocksMsg)
      .returning(Future.failed(new ClassCastException("error")))
      .atLeastOnce()

    qaService.mineBlocks(mineBlocksReq).map(_ shouldBe Left(JsonRpcErrors.InternalError))
  }

  class Fixture {
    protected trait TestConsensus extends Consensus {
      override type Config = EthashConfig
    }

    lazy val testConsensus: TestConsensus = mock[TestConsensus]
    lazy val qaService = new QAService(testConsensus)

    lazy val mineBlocksReq = MineBlocksRequest(1, true, None)
    lazy val mineBlocksMsg =
      MineBlocks(mineBlocksReq.numBlocks, mineBlocksReq.withTransactions, mineBlocksReq.parentBlock)
    val fakeChainId: Byte = 42.toByte
  }

  def createFixture(): Fixture = new Fixture
}
