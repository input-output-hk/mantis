package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.consensus.ethash.blocks.EthashBlockGenerator
import io.iohk.ethereum.consensus.ethash.validators.ValidatorsExecutor
import io.iohk.ethereum.consensus.{ConsensusConfigs, TestConsensus}
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.HefPostEcip1098
import io.iohk.ethereum.domain.{Block, BlockBody, BlockHeader, Checkpoint, SignedTransaction, UInt256}
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.{BloomFilter, InMemoryWorldStateProxy, Ledger, StxLedger}
import io.iohk.ethereum.nodebuilder.ApisBuilder
import io.iohk.ethereum.utils.{Config, FilterConfig}
import io.iohk.ethereum.{Fixtures, ObjectGenerators, Timeouts}
import org.bouncycastle.util.encoders.Hex
import org.json4s.JsonAST.{JArray, JInt, JString, JValue}
import org.scalamock.scalatest.MockFactory

import scala.concurrent.duration._

class JsonRpcControllerFixture(implicit system: ActorSystem)
    extends MockFactory
    with EphemBlockchainTestSetup
    with JsonMethodsImplicits
    with ApisBuilder {

  def config: JsonRpcConfig = JsonRpcConfig(Config.config, available)

  def rawTrnHex(xs: Seq[SignedTransaction], idx: Int): Option[JString] =
    xs.lift(idx)
      .map(encodeSignedTrx)

  def encodeSignedTrx(x: SignedTransaction): JString =
    encodeAsHex(RawTransactionCodec.asRawTransaction(x))

  val version = Config.clientVersion
  val blockGenerator: EthashBlockGenerator = mock[EthashBlockGenerator]

  val syncingController: TestProbe = TestProbe()
  override lazy val ledger: Ledger = mock[Ledger]
  override lazy val stxLedger: StxLedger = mock[StxLedger]
  override lazy val validators: ValidatorsExecutor = mock[ValidatorsExecutor]
  override lazy val consensus: TestConsensus = buildTestConsensus()
    .withValidators(validators)
    .withBlockGenerator(blockGenerator)

  val keyStore: KeyStore = mock[KeyStore]

  val pendingTransactionsManager: TestProbe = TestProbe()
  val ommersPool: TestProbe = TestProbe()
  val filterManager: TestProbe = TestProbe()

  val ethashConfig = ConsensusConfigs.ethashConfig
  override lazy val consensusConfig = ConsensusConfigs.consensusConfig
  val fullConsensusConfig = ConsensusConfigs.fullConsensusConfig
  val getTransactionFromPoolTimeout: FiniteDuration = 5.seconds

  val filterConfig: FilterConfig = new FilterConfig {
    override val filterTimeout: FiniteDuration = Timeouts.normalTimeout
    override val filterManagerQueryTimeout: FiniteDuration = Timeouts.normalTimeout
  }

  val currentProtocolVersion = 63

  val appStateStorage: AppStateStorage = mock[AppStateStorage]
  val web3Service = new Web3Service
  val netService: NetService = mock[NetService]

  val ethInfoService = new EthInfoService(
    blockchain,
    blockchainConfig,
    ledger,
    stxLedger,
    keyStore,
    syncingController.ref,
    currentProtocolVersion,
    Timeouts.shortTimeout
  )

  val ethMiningService = new EthMiningService(
    blockchain,
    ledger,
    config,
    ommersPool.ref,
    syncingController.ref,
    pendingTransactionsManager.ref,
    getTransactionFromPoolTimeout
  )

  val ethBlocksService = new EthBlocksService(blockchain, ledger)

  val ethTxService = new EthTxService(
    blockchain,
    ledger,
    pendingTransactionsManager.ref,
    getTransactionFromPoolTimeout
  )

  val ethUserService = new EthUserService(
    blockchain,
    ledger,
    blockchainConfig
  )

  val ethFilterService = new EthFilterService(
    filterManager.ref,
    filterConfig
  )
  val personalService: PersonalService = mock[PersonalService]
  val debugService: DebugService = mock[DebugService]
  val qaService: QAService = mock[QAService]
  val checkpointingService: CheckpointingService = mock[CheckpointingService]
  val mantisService: MantisService = mock[MantisService]

  def jsonRpcController: JsonRpcController =
    JsonRpcController(
      web3Service,
      netService,
      ethInfoService,
      ethMiningService,
      ethBlocksService,
      ethTxService,
      ethUserService,
      ethFilterService,
      personalService,
      None,
      debugService,
      qaService,
      checkpointingService,
      mantisService,
      ProofServiceDummy,
      config
    )

  val blockHeader: BlockHeader = Fixtures.Blocks.ValidBlock.header.copy(
    logsBloom = BloomFilter.EmptyBloomFilter,
    difficulty = 10,
    number = 2,
    gasLimit = 0,
    gasUsed = 0,
    unixTimestamp = 0
  )

  val checkpoint: Checkpoint = ObjectGenerators.fakeCheckpointGen(2, 5).sample.get
  val checkpointBlockGenerator = new CheckpointBlockGenerator()
  val blockWithCheckpoint: Block = checkpointBlockGenerator.generate(Fixtures.Blocks.Block3125369.block, checkpoint)
  val blockWithTreasuryOptOut: Block =
    Block(
      Fixtures.Blocks.Block3125369.header.copy(extraFields = HefPostEcip1098(true)),
      Fixtures.Blocks.Block3125369.body
    )

  val parentBlock: Block = Block(blockHeader.copy(number = 1), BlockBody.empty)

  val r: ByteString = ByteString(Hex.decode("a3f20717a250c2b0b729b7e5becbff67fdaef7e0699da4de7ca5895b02a170a1"))
  val s: ByteString = ByteString(Hex.decode("2d887fd3b17bfdce3481f10bea41f45ba9f709d39ce8325427b57afcfc994cee"))
  val v: Byte = ByteString(Hex.decode("1b")).last
  val sig: ECDSASignature = ECDSASignature(r, s, v)

  def newJsonRpcRequest(method: String, params: List[JValue]): JsonRpcRequest =
    JsonRpcRequest("2.0", method, Some(JArray(params)), Some(JInt(1)))

  def newJsonRpcRequest(method: String): JsonRpcRequest =
    JsonRpcRequest("2.0", method, None, Some(JInt(1)))

  val fakeWorld: InMemoryWorldStateProxy = blockchain.getReadOnlyWorldStateProxy(
    None,
    UInt256.Zero,
    ByteString.empty,
    noEmptyAccounts = false,
    ethCompatibleStorage = true
  )
}
