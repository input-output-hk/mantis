package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestProbe
import akka.util.ByteString

import monix.eval.Task

import org.scalamock.scalatest.AsyncMockFactory

import io.iohk.ethereum._
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.NewCheckpoint
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.consensus.pow.EthashConfig
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MineBlocks
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponses.MiningOrdered
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.QAService._
import io.iohk.ethereum.nodebuilder.BlockchainConfigBuilder

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

    (blockchainReader.getBlockByHash _)
      .expects(req.blockHash.get)
      .returning(Some(block))
      .noMoreThanOnce()

    val result = qaService.generateCheckpoint(req)
    val checkpointBlock = checkpointBlockGenerator.generate(block, Checkpoint(signatures))

    result.map { r =>
      syncController.expectMsg(NewCheckpoint(checkpointBlock))
      r shouldBe Right(GenerateCheckpointResponse(checkpoint))
    }
  }

  it should "generate checkpoint for best block when no block hash given and send it to sync" in customTestCaseM(
    new Fixture with CheckpointsGenerationFixture
  ) { fixture =>
    import fixture._
    val reqWithoutBlockHash = req.copy(blockHash = None)

    (blockchainReader.getBlockByHash _)
      .expects(req.blockHash.get)
      .returning(Some(block))
      .noMoreThanOnce()

    (blockchainReader.getBestBlock _)
      .expects()
      .returning(Some(block))
      .once()

    val result: ServiceResponse[GenerateCheckpointResponse] =
      qaService.generateCheckpoint(reqWithoutBlockHash)
    val checkpointBlock = checkpointBlockGenerator.generate(block, Checkpoint(signatures))

    result.map { r =>
      syncController.expectMsg(NewCheckpoint(checkpointBlock))
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
    lazy val blockchainReader: BlockchainReader = mock[BlockchainReader]
    lazy val blockchain: BlockchainImpl = mock[BlockchainImpl]
    lazy val syncController: TestProbe = TestProbe()
    lazy val checkpointBlockGenerator = new CheckpointBlockGenerator()

    lazy val qaService = new QAService(
      testConsensus,
      blockchain,
      blockchainReader,
      checkpointBlockGenerator,
      blockchainConfig,
      syncController.ref
    )

    lazy val mineBlocksReq: MineBlocksRequest = MineBlocksRequest(1, true, None)
    lazy val mineBlocksMsg: MineBlocks =
      MineBlocks(mineBlocksReq.numBlocks, mineBlocksReq.withTransactions, mineBlocksReq.parentBlock)
    val fakeChainId: Byte = 42.toByte
  }

  trait CheckpointsGenerationFixture {
    val block = Fixtures.Blocks.ValidBlock.block
    val privateKeys: Seq[ByteString] = seqByteStringOfNItemsOfLengthMGen(3, 32).sample.get
    val signatures: Seq[ECDSASignature] = privateKeys.map(ECDSASignature.sign(block.hash, _))
    val checkpoint: Checkpoint = Checkpoint(signatures)
    val req: GenerateCheckpointRequest = GenerateCheckpointRequest(privateKeys, Some(block.hash))
  }

  def createFixture(): Fixture = new Fixture
}
