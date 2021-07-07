package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestProbe
import akka.util.ByteString

import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.WithActorSystemShutDown
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.ConsensusConfigs
import io.iohk.ethereum.consensus.TestConsensus
import io.iohk.ethereum.consensus.blocks.PendingBlock
import io.iohk.ethereum.consensus.blocks.PendingBlockAndState
import io.iohk.ethereum.consensus.pow.blocks.PoWBlockGenerator
import io.iohk.ethereum.consensus.pow.blocks.RestrictedPoWBlockGeneratorImpl
import io.iohk.ethereum.consensus.pow.difficulty.EthashDifficultyCalculator
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockHeader.getEncodedWithoutNonce
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.jsonrpc.EthMiningService._
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.nodebuilder.ApisBuilder
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config

class EthMiningServiceSpec
    extends TestKit(ActorSystem("EthMiningServiceSpec_ActorSystem"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with ScalaFutures
    with NormalPatience {

  "MiningServiceSpec" should "return if node is mining base on getWork" in new TestSetup {

    ethMiningService.getMining(GetMiningRequest()).runSyncUnsafe() shouldEqual Right(GetMiningResponse(false))

    (blockGenerator.generateBlock _)
      .expects(parentBlock, *, *, *, *)
      .returning(PendingBlockAndState(PendingBlock(block, Nil), fakeWorld))
    blockchainWriter.storeBlock(parentBlock).commit()
    ethMiningService.getWork(GetWorkRequest())

    val response = ethMiningService.getMining(GetMiningRequest())

    response.runSyncUnsafe() shouldEqual Right(GetMiningResponse(true))
  }

  it should "return if node is mining base on submitWork" in new TestSetup {

    ethMiningService.getMining(GetMiningRequest()).runSyncUnsafe() shouldEqual Right(GetMiningResponse(false))

    (blockGenerator.getPrepared _).expects(*).returning(Some(PendingBlock(block, Nil)))
    (appStateStorage.getBestBlockNumber _).expects().returning(0)
    ethMiningService.submitWork(
      SubmitWorkRequest(ByteString("nonce"), ByteString(Hex.decode("01" * 32)), ByteString(Hex.decode("01" * 32)))
    )

    val response = ethMiningService.getMining(GetMiningRequest())

    response.runSyncUnsafe() shouldEqual Right(GetMiningResponse(true))
  }

  it should "return if node is mining base on submitHashRate" in new TestSetup {

    ethMiningService.getMining(GetMiningRequest()).runSyncUnsafe() shouldEqual Right(GetMiningResponse(false))
    ethMiningService.submitHashRate(SubmitHashRateRequest(42, ByteString("id")))

    val response = ethMiningService.getMining(GetMiningRequest())

    response.runSyncUnsafe() shouldEqual Right(GetMiningResponse(true))
  }

  it should "return if node is mining after time out" in new TestSetup {

    (blockGenerator.generateBlock _)
      .expects(parentBlock, *, *, *, *)
      .returning(PendingBlockAndState(PendingBlock(block, Nil), fakeWorld))
    blockchainWriter.storeBlock(parentBlock).commit()
    ethMiningService.getWork(GetWorkRequest())

    Thread.sleep(minerActiveTimeout.toMillis)

    val response = ethMiningService.getMining(GetMiningRequest())

    response.runSyncUnsafe() shouldEqual Right(GetMiningResponse(false))
  }

  it should "return requested work" in new TestSetup {

    (blockGenerator.generateBlock _)
      .expects(parentBlock, Nil, *, *, *)
      .returning(PendingBlockAndState(PendingBlock(block, Nil), fakeWorld))
    blockchainWriter.save(parentBlock, Nil, ChainWeight.totalDifficultyOnly(parentBlock.header.difficulty), true)

    val response = ethMiningService.getWork(GetWorkRequest()).runSyncUnsafe()
    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsManager.PendingTransactionsResponse(Nil))

    ommersPool.expectMsg(OmmersPool.GetOmmers(parentBlock.hash))
    ommersPool.reply(OmmersPool.Ommers(Nil))

    response shouldEqual Right(GetWorkResponse(powHash, seedHash, target))
  }

  it should "generate and submit work when generating block for mining with restricted ethash generator" in new TestSetup {
    val testConsensus = buildTestConsensus()
    override lazy val restrictedGenerator = new RestrictedPoWBlockGeneratorImpl(
      evmCodeStorage = storagesInstance.storages.evmCodeStorage,
      validators = MockValidatorsAlwaysSucceed,
      blockchainReader = blockchainReader,
      consensusConfig = consensusConfig,
      blockPreparator = testConsensus.blockPreparator,
      difficultyCalc,
      minerKey
    )
    override lazy val consensus: TestConsensus = testConsensus.withBlockGenerator(restrictedGenerator)

    blockchainWriter.save(parentBlock, Nil, ChainWeight.totalDifficultyOnly(parentBlock.header.difficulty), true)

    val response = ethMiningService.getWork(GetWorkRequest()).runSyncUnsafe()
    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsManager.PendingTransactionsResponse(Nil))

    ommersPool.expectMsg(OmmersPool.GetOmmers(parentBlock.hash))
    ommersPool.reply(OmmersPool.Ommers(Nil))

    assert(response.isRight)
    val responseData = response.toOption.get

    val submitRequest =
      SubmitWorkRequest(ByteString("nonce"), responseData.powHeaderHash, ByteString(Hex.decode("01" * 32)))
    val response1 = ethMiningService.submitWork(submitRequest).runSyncUnsafe()
    response1 shouldEqual Right(SubmitWorkResponse(true))
  }

  it should "accept submitted correct PoW" in new TestSetup {

    val headerHash = ByteString(Hex.decode("01" * 32))

    (blockGenerator.getPrepared _).expects(headerHash).returning(Some(PendingBlock(block, Nil)))
    (appStateStorage.getBestBlockNumber _).expects().returning(0)

    val req = SubmitWorkRequest(ByteString("nonce"), headerHash, ByteString(Hex.decode("01" * 32)))

    val response = ethMiningService.submitWork(req)
    response.runSyncUnsafe() shouldEqual Right(SubmitWorkResponse(true))
  }

  it should "reject submitted correct PoW when header is no longer in cache" in new TestSetup {

    val headerHash = ByteString(Hex.decode("01" * 32))

    (blockGenerator.getPrepared _).expects(headerHash).returning(None)
    (appStateStorage.getBestBlockNumber _).expects().returning(0)

    val req = SubmitWorkRequest(ByteString("nonce"), headerHash, ByteString(Hex.decode("01" * 32)))

    val response = ethMiningService.submitWork(req)
    response.runSyncUnsafe() shouldEqual Right(SubmitWorkResponse(false))
  }

  it should "return correct coinbase" in new TestSetup {

    val response = ethMiningService.getCoinbase(GetCoinbaseRequest())
    response.runSyncUnsafe() shouldEqual Right(GetCoinbaseResponse(consensusConfig.coinbase))
  }

  it should "accept and report hashrate" in new TestSetup {

    val rate: BigInt = 42
    val id = ByteString("id")

    ethMiningService.submitHashRate(SubmitHashRateRequest(12, id)).runSyncUnsafe() shouldEqual Right(
      SubmitHashRateResponse(true)
    )
    ethMiningService.submitHashRate(SubmitHashRateRequest(rate, id)).runSyncUnsafe() shouldEqual Right(
      SubmitHashRateResponse(true)
    )

    val response = ethMiningService.getHashRate(GetHashRateRequest())
    response.runSyncUnsafe() shouldEqual Right(GetHashRateResponse(rate))
  }

  it should "combine hashrates from many miners and remove timed out rates" in new TestSetup {

    val rate: BigInt = 42
    val id1 = ByteString("id1")
    val id2 = ByteString("id2")

    ethMiningService.submitHashRate(SubmitHashRateRequest(rate, id1)).runSyncUnsafe() shouldEqual Right(
      SubmitHashRateResponse(true)
    )
    Thread.sleep(minerActiveTimeout.toMillis / 2)
    ethMiningService.submitHashRate(SubmitHashRateRequest(rate, id2)).runSyncUnsafe() shouldEqual Right(
      SubmitHashRateResponse(true)
    )

    val response1 = ethMiningService.getHashRate(GetHashRateRequest())
    response1.runSyncUnsafe() shouldEqual Right(GetHashRateResponse(rate * 2))

    Thread.sleep(minerActiveTimeout.toMillis / 2)
    val response2 = ethMiningService.getHashRate(GetHashRateRequest())
    response2.runSyncUnsafe() shouldEqual Right(GetHashRateResponse(rate))
  }

  // NOTE TestSetup uses Ethash consensus; check `consensusConfig`.
  class TestSetup(implicit system: ActorSystem) extends MockFactory with EphemBlockchainTestSetup with ApisBuilder {
    val blockGenerator: PoWBlockGenerator = mock[PoWBlockGenerator]
    val appStateStorage: AppStateStorage = mock[AppStateStorage]
    override lazy val consensus: TestConsensus = buildTestConsensus().withBlockGenerator(blockGenerator)
    override lazy val consensusConfig = ConsensusConfigs.consensusConfig

    val syncingController: TestProbe = TestProbe()
    val pendingTransactionsManager: TestProbe = TestProbe()
    val ommersPool: TestProbe = TestProbe()

    val minerActiveTimeout: FiniteDuration = 5.seconds
    val getTransactionFromPoolTimeout: FiniteDuration = 5.seconds

    lazy val minerKey: AsymmetricCipherKeyPair = crypto.keyPairFromPrvKey(
      ByteStringUtils.string2hash("00f7500a7178548b8a4488f78477660b548c9363e16b584c21e0208b3f1e0dc61f")
    )

    lazy val difficultyCalc = new EthashDifficultyCalculator()

    lazy val restrictedGenerator = new RestrictedPoWBlockGeneratorImpl(
      evmCodeStorage = storagesInstance.storages.evmCodeStorage,
      validators = MockValidatorsAlwaysSucceed,
      blockchainReader = blockchainReader,
      consensusConfig = consensusConfig,
      blockPreparator = consensus.blockPreparator,
      difficultyCalc,
      minerKey
    )

    val jsonRpcConfig: JsonRpcConfig = JsonRpcConfig(Config.config, available)

    lazy val ethMiningService = new EthMiningService(
      blockchainReader,
      consensus,
      jsonRpcConfig,
      ommersPool.ref,
      syncingController.ref,
      pendingTransactionsManager.ref,
      getTransactionFromPoolTimeout,
      this
    )

    val difficulty = 131072
    val parentBlock: Block = Block(
      header = BlockHeader(
        parentHash = ByteString.empty,
        ommersHash = ByteString.empty,
        beneficiary = ByteString.empty,
        stateRoot = ByteString(MerklePatriciaTrie.EmptyRootHash),
        transactionsRoot = ByteString.empty,
        receiptsRoot = ByteString.empty,
        logsBloom = ByteString.empty,
        difficulty = difficulty,
        number = 0,
        gasLimit = 16733003,
        gasUsed = 0,
        unixTimestamp = 1494604900,
        extraData = ByteString.empty,
        mixHash = ByteString.empty,
        nonce = ByteString.empty
      ),
      body = BlockBody.empty
    )
    val block: Block = Block(
      header = BlockHeader(
        parentHash = parentBlock.header.hash,
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = ByteString(Hex.decode("000000000000000000000000000000000000002a")),
        stateRoot = ByteString(Hex.decode("2627314387b135a548040d3ca99dbf308265a3f9bd9246bee3e34d12ea9ff0dc")),
        transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        logsBloom = ByteString(Hex.decode("00" * 256)),
        difficulty = difficulty,
        number = 1,
        gasLimit = 16733003,
        gasUsed = 0,
        unixTimestamp = 1494604913,
        extraData = ByteString(Hex.decode("6d696e6564207769746820657463207363616c61")),
        mixHash = ByteString.empty,
        nonce = ByteString.empty
      ),
      body = BlockBody.empty
    )
    val seedHash: ByteString = ByteString(Hex.decode("00" * 32))
    val powHash: ByteString = ByteString(kec256(getEncodedWithoutNonce(block.header)))
    val target: ByteString = ByteString((BigInt(2).pow(256) / difficulty).toByteArray)

    val fakeWorld: InMemoryWorldStateProxy = InMemoryWorldStateProxy(
      storagesInstance.storages.evmCodeStorage,
      blockchain.getReadOnlyMptStorage(),
      (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      UInt256.Zero,
      ByteString.empty,
      noEmptyAccounts = false,
      ethCompatibleStorage = true
    )
  }
}
