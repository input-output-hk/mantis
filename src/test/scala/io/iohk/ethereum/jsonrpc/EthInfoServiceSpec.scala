package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestProbe
import akka.util.ByteString

import monix.execution.Scheduler.Implicits.global

import org.bouncycastle.util.encoders.Hex
import org.scalactic.TypeCheckedTripleEquals
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum._
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status.Progress
import io.iohk.ethereum.consensus.mining.MiningConfigs
import io.iohk.ethereum.consensus.mining.TestMining
import io.iohk.ethereum.consensus.pow.blocks.PoWBlockGenerator
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthInfoService.ProtocolVersionRequest
import io.iohk.ethereum.jsonrpc.EthInfoService._
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ledger.StxLedger
import io.iohk.ethereum.ledger.TxResult
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.testing.ActorsTesting.simpleAutoPilot

class EthServiceSpec
    extends TestKit(ActorSystem("EthInfoServiceSpec_ActorSystem"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with ScalaFutures
    with OptionValues
    with MockFactory
    with NormalPatience
    with TypeCheckedTripleEquals {

  "EthInfoService" should "return ethereum protocol version" in new TestSetup {
    val response = ethService.protocolVersion(ProtocolVersionRequest()).runSyncUnsafe()
    val protocolVersion = response.toOption.get.value

    Integer.parseInt(protocolVersion.drop(2), 16) shouldEqual currentProtocolVersion
  }

  it should "return configured chain id" in new TestSetup {
    val response = ethService.chainId(ChainIdRequest()).runSyncUnsafe().toOption.get

    assert(response === ChainIdResponse(blockchainConfig.chainId))
  }

  it should "return syncing info if the peer is syncing" in new TestSetup {
    syncingController.setAutoPilot(simpleAutoPilot { case SyncProtocol.GetStatus =>
      SyncProtocol.Status.Syncing(999, Progress(200, 10000), Some(Progress(100, 144)))
    })

    val response = ethService.syncing(SyncingRequest()).runSyncUnsafe().toOption.get

    response shouldEqual SyncingResponse(
      Some(
        EthInfoService.SyncingStatus(
          startingBlock = 999,
          currentBlock = 200,
          highestBlock = 10000,
          knownStates = 144,
          pulledStates = 100
        )
      )
    )
  }

  // scalastyle:off magic.number
  it should "return no syncing info if the peer is not syncing" in new TestSetup {
    syncingController.setAutoPilot(simpleAutoPilot { case SyncProtocol.GetStatus =>
      SyncProtocol.Status.NotSyncing
    })

    val response = ethService.syncing(SyncingRequest()).runSyncUnsafe()

    response shouldEqual Right(SyncingResponse(None))
  }

  it should "return no syncing info if sync is done" in new TestSetup {
    syncingController.setAutoPilot(simpleAutoPilot { case SyncProtocol.GetStatus =>
      SyncProtocol.Status.SyncDone
    })

    val response = ethService.syncing(SyncingRequest()).runSyncUnsafe()

    response shouldEqual Right(SyncingResponse(None))
  }

  it should "execute call and return a value" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val worldStateProxy = InMemoryWorldStateProxy(
      storagesInstance.storages.evmCodeStorage,
      blockchain.getBackingMptStorage(-1),
      (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      UInt256.Zero,
      ByteString.empty,
      noEmptyAccounts = false,
      ethCompatibleStorage = true
    )

    val txResult = TxResult(worldStateProxy, 123, Nil, ByteString("return_value"), None)
    (stxLedger.simulateTransaction _).expects(*, *, *).returning(txResult)

    val tx = CallTx(
      Some(ByteString(Hex.decode("da714fe079751fa7a1ad80b76571ea6ec52a446c"))),
      Some(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477"))),
      Some(1),
      2,
      3,
      ByteString("")
    )
    val response = ethService.call(CallRequest(tx, BlockParam.Latest))

    response.runSyncUnsafe() shouldEqual Right(CallResponse(ByteString("return_value")))
  }

  it should "execute estimateGas and return a value" in new TestSetup {
    blockchainWriter.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val estimatedGas = BigInt(123)
    (stxLedger.binarySearchGasEstimation _).expects(*, *, *).returning(estimatedGas)

    val tx = CallTx(
      Some(ByteString(Hex.decode("da714fe079751fa7a1ad80b76571ea6ec52a446c"))),
      Some(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477"))),
      Some(1),
      2,
      3,
      ByteString("")
    )
    val response = ethService.estimateGas(CallRequest(tx, BlockParam.Latest))

    response.runSyncUnsafe() shouldEqual Right(EstimateGasResponse(123))
  }

  // NOTE TestSetup uses Ethash consensus; check `consensusConfig`.
  class TestSetup(implicit system: ActorSystem) extends MockFactory with EphemBlockchainTestSetup {
    val blockGenerator: PoWBlockGenerator = mock[PoWBlockGenerator]
    val appStateStorage: AppStateStorage = mock[AppStateStorage]
    val keyStore: KeyStore = mock[KeyStore]
    override lazy val stxLedger: StxLedger = mock[StxLedger]

    override lazy val mining: TestMining = buildTestMining().withBlockGenerator(blockGenerator)
    override lazy val miningConfig = MiningConfigs.miningConfig

    val syncingController: TestProbe = TestProbe()

    val currentProtocolVersion = Capability.ETH63.version

    lazy val ethService = new EthInfoService(
      blockchain,
      blockchainReader,
      blockchainConfig,
      mining,
      stxLedger,
      keyStore,
      syncingController.ref,
      Capability.ETH63,
      Timeouts.shortTimeout
    )

    val blockToRequest: Block = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txToRequest = Fixtures.Blocks.Block3125369.body.transactionList.head
    val txSender: Address = SignedTransaction.getSender(txToRequest).get
  }
}
