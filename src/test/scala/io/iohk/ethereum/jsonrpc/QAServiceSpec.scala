package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestProbe
import io.iohk.ethereum._
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.NewCheckpoint
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.consensus.ethash.EthashConfig
import io.iohk.ethereum.consensus.ethash.MinerResponses.MiningOrdered
import io.iohk.ethereum.consensus.ethash.MockedMinerProtocol.MineBlocks
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.QAService._
import io.iohk.ethereum.nodebuilder.BlockchainConfigBuilder
import monix.eval.Task
import org.scalamock.scalatest.AsyncMockFactory

class QAServiceSpec
    extends TestKit(ActorSystem("QAServiceSpec_ActorSystem"))
    with FlatSpecBase
    with WithActorSystemShutDown
    with SpecFixtures
    with ByteGenerators
    with AsyncMockFactory {

  "QAService" should "send msg to miner and return miner's response" in testCaseM { fixture =>
    import fixture._
    (testConsensus.askMiner _)
      .expects(mineBlocksMsg)
      .returning(Task.now(MiningOrdered))
      .atLeastOnce()

    qaService.mineBlocks(mineBlocksReq).map(_ shouldBe Right(MineBlocksResponse(MiningOrdered)))
  }

  it should "send msg to miner and return InternalError in case of problems" in testCaseM { fixture =>
    import fixture._
    (testConsensus.askMiner _)
      .expects(mineBlocksMsg)
      .returning(Task.raiseError(new ClassCastException("error")))
      .atLeastOnce()

    qaService.mineBlocks(mineBlocksReq).map(_ shouldBe Left(JsonRpcError.InternalError))
  }

  it should "generate checkpoint for block with given blockHash and send it to sync" in customTestCaseM(
    new Fixture with CheckpointsGenerationFixture
  ) { fixture =>
    import fixture._

    val result = qaService.generateCheckpoint(req)

    result.map { r =>
      syncController.expectMsg(NewCheckpoint(block.hash, signatures))
      r shouldBe Right(GenerateCheckpointResponse(checkpoint))
    }
  }

  it should "generate checkpoint for best block when no block hash given and send it to sync" in customTestCaseM(
    new Fixture with CheckpointsGenerationFixture
  ) { fixture =>
    import fixture._
    val reqWithoutBlockHash = req.copy(blockHash = None)
    (blockchain.getBestBlock _)
      .expects()
      .returning(Some(block))
      .once()

    val result: ServiceResponse[GenerateCheckpointResponse] =
      qaService.generateCheckpoint(reqWithoutBlockHash)

    result.map { r =>
      syncController.expectMsg(NewCheckpoint(block.hash, signatures))
      r shouldBe Right(GenerateCheckpointResponse(checkpoint))
    }
  }

  it should "return federation public keys when requesting federation members info" in testCaseM { fixture =>
    import fixture._
    val result: ServiceResponse[GetFederationMembersInfoResponse] =
      qaService.getFederationMembersInfo(GetFederationMembersInfoRequest())

    result.map(_ shouldBe Right(GetFederationMembersInfoResponse(blockchainConfig.checkpointPubKeys.toList)))
  }

  class Fixture extends BlockchainConfigBuilder {
    protected trait TestConsensus extends Consensus {
      override type Config = EthashConfig
    }

    lazy val testConsensus: TestConsensus = mock[TestConsensus]
    lazy val blockchain = mock[BlockchainImpl]
    lazy val syncController = TestProbe()

    lazy val qaService = new QAService(
      testConsensus,
      blockchain,
      blockchainConfig,
      syncController.ref
    )

    lazy val mineBlocksReq = MineBlocksRequest(1, true, None)
    lazy val mineBlocksMsg =
      MineBlocks(mineBlocksReq.numBlocks, mineBlocksReq.withTransactions, mineBlocksReq.parentBlock)
    val fakeChainId: Byte = 42.toByte
  }

  trait CheckpointsGenerationFixture {
    val block = Fixtures.Blocks.ValidBlock.block
    val privateKeys = seqByteStringOfNItemsOfLengthMGen(3, 32).sample.get
    val signatures = privateKeys.map(ECDSASignature.sign(block.hash, _))
    val checkpoint = Checkpoint(signatures)
    val req = GenerateCheckpointRequest(privateKeys, Some(block.hash))
  }

  def createFixture(): Fixture = new Fixture
}
