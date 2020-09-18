package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.consensus.ethash.EthashConfig
import io.iohk.ethereum.consensus.ethash.MinerResponses.MiningOrdered
import io.iohk.ethereum.consensus.ethash.MockedMinerProtocol.MineBlocks
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Address, SignedTransactionWithSender, Transaction}
import io.iohk.ethereum.jsonrpc.QAService.{
  GetPendingTransactionsRequest,
  GetPendingTransactionsResponse,
  MineBlocksRequest,
  MineBlocksResponse
}
import io.iohk.ethereum.transactions.PendingTransactionsManager.{
  GetPendingTransactions,
  PendingTransaction,
  PendingTransactionsResponse
}
import io.iohk.ethereum.{ByteGenerators, FlatSpecBase, SpecFixtures, Timeouts}
import org.scalamock.scalatest.AsyncMockFactory
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

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

  it should "send message to pendingTransactionsManager and return an empty GetPendingTransactionsResponse" in testCaseF {
    fixture =>
      import fixture._
      val res = qaService.getPendingTransactions(GetPendingTransactionsRequest())

      pendingTransactionsManager.expectMsg(GetPendingTransactions)
      pendingTransactionsManager.reply(PendingTransactionsResponse(Nil))

      res.map(_ shouldBe Right(GetPendingTransactionsResponse(Nil)))
  }

  it should "send message to pendingTransactionsManager and return GetPendingTransactionsResponse with two transactions" in testCaseF {
    fixture =>
      import fixture._
      val transactions = (0 to 1)
        .map(_ => {
          val fakeTransaction = SignedTransactionWithSender(
            Transaction(
              nonce = 0,
              gasPrice = 123,
              gasLimit = 123,
              receivingAddress = Address("0x1234"),
              value = 0,
              payload = ByteString()
            ),
            signature = ECDSASignature(0, 0, 0.toByte),
            sender = Address("0x1234")
          )
          PendingTransaction(fakeTransaction, System.currentTimeMillis)
        })
        .toList

      val res = qaService.getPendingTransactions(GetPendingTransactionsRequest())

      pendingTransactionsManager.expectMsg(GetPendingTransactions)
      pendingTransactionsManager.reply(PendingTransactionsResponse(transactions))

      res.map(_ shouldBe Right(GetPendingTransactionsResponse(transactions)))
  }

  it should "send message to pendingTransactionsManager and return an empty GetPendingTransactionsResponse in case of error" in testCaseF {
    fixture =>
      import fixture._
      val res = qaService.getPendingTransactions(GetPendingTransactionsRequest())

      pendingTransactionsManager.expectMsg(GetPendingTransactions)
      pendingTransactionsManager.reply(new ClassCastException("error"))

      res.map(_ shouldBe Right(GetPendingTransactionsResponse(Nil)))
  }

  class Fixture {
    protected trait TestConsensus extends Consensus {
      override type Config = EthashConfig
    }

    lazy val testConsensus: TestConsensus = mock[TestConsensus]
    lazy val pendingTransactionsManager = TestProbe()
    lazy val getTransactionFromPoolTimeout: FiniteDuration = Timeouts.normalTimeout
    lazy val qaService = new QAService(
      testConsensus,
      pendingTransactionsManager.ref,
      getTransactionFromPoolTimeout
    )

    lazy val mineBlocksReq = MineBlocksRequest(1, true, None)
    lazy val mineBlocksMsg =
      MineBlocks(mineBlocksReq.numBlocks, mineBlocksReq.withTransactions, mineBlocksReq.parentBlock)
    val fakeChainId: Byte = 42.toByte
  }

  def createFixture(): Fixture = new Fixture
}
