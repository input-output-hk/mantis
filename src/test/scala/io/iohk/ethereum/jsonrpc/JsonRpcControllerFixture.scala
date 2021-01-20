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
import io.iohk.ethereum.domain.{Block, BlockBody, SignedTransaction, UInt256}
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.{BloomFilter, Ledger, StxLedger}
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

  def encodeSignedTrx(x: SignedTransaction) =
    encodeAsHex(RawTransactionCodec.asRawTransaction(x))

  val version = Config.clientVersion
  val blockGenerator = mock[EthashBlockGenerator]

  val syncingController = TestProbe()
  override lazy val ledger = mock[Ledger]
  override lazy val stxLedger = mock[StxLedger]
  override lazy val validators = mock[ValidatorsExecutor]
  override lazy val consensus: TestConsensus = buildTestConsensus()
    .withValidators(validators)
    .withBlockGenerator(blockGenerator)

  val keyStore = mock[KeyStore]

  val pendingTransactionsManager = TestProbe()
  val ommersPool = TestProbe()
  val filterManager = TestProbe()

  val ethashConfig = ConsensusConfigs.ethashConfig
  override lazy val consensusConfig = ConsensusConfigs.consensusConfig
  val fullConsensusConfig = ConsensusConfigs.fullConsensusConfig
  val getTransactionFromPoolTimeout: FiniteDuration = 5.seconds

  val filterConfig = new FilterConfig {
    override val filterTimeout: FiniteDuration = Timeouts.normalTimeout
    override val filterManagerQueryTimeout: FiniteDuration = Timeouts.normalTimeout
  }

  val currentProtocolVersion = 63

  val appStateStorage = mock[AppStateStorage]
  val web3Service = new Web3Service
  val netService = mock[NetService]
  val personalService = mock[PersonalService]
  val debugService = mock[DebugService]
  val qaService = mock[QAService]
  val checkpointingService = mock[CheckpointingService]
  val mantisService = mock[MantisService]

  val ethService = new EthService(
    blockchain,
    ledger,
    stxLedger,
    keyStore,
    pendingTransactionsManager.ref,
    syncingController.ref,
    ommersPool.ref,
    filterManager.ref,
    filterConfig,
    blockchainConfig,
    currentProtocolVersion,
    config,
    getTransactionFromPoolTimeout,
    Timeouts.shortTimeout
  )

  protected def newJsonRpcController(ethService: EthService, proofService: ProofService = ProofServiceDummy) =
    new JsonRpcController(
      web3Service,
      netService,
      ethService,
      personalService,
      None,
      debugService,
      qaService,
      checkpointingService,
      mantisService,
      proofService,
      config
    )

  val jsonRpcController = newJsonRpcController(ethService)

  val blockHeader = Fixtures.Blocks.ValidBlock.header.copy(
    logsBloom = BloomFilter.EmptyBloomFilter,
    difficulty = 10,
    number = 2,
    gasLimit = 0,
    gasUsed = 0,
    unixTimestamp = 0
  )

  val checkpoint = ObjectGenerators.fakeCheckpointGen(2, 5).sample.get
  val checkpointBlockGenerator = new CheckpointBlockGenerator()
  val blockWithCheckpoint = checkpointBlockGenerator.generate(Fixtures.Blocks.Block3125369.block, checkpoint)
  val blockWithTreasuryOptOut =
    Block(
      Fixtures.Blocks.Block3125369.header.copy(extraFields = HefPostEcip1098(true)),
      Fixtures.Blocks.Block3125369.body
    )

  val parentBlock = Block(blockHeader.copy(number = 1), BlockBody.empty)

  val r: ByteString = ByteString(Hex.decode("a3f20717a250c2b0b729b7e5becbff67fdaef7e0699da4de7ca5895b02a170a1"))
  val s: ByteString = ByteString(Hex.decode("2d887fd3b17bfdce3481f10bea41f45ba9f709d39ce8325427b57afcfc994cee"))
  val v: Byte = ByteString(Hex.decode("1b")).last
  val sig = ECDSASignature(r, s, v)

  def newJsonRpcRequest(method: String, params: List[JValue]) =
    JsonRpcRequest("2.0", method, Some(JArray(params)), Some(JInt(1)))

  def newJsonRpcRequest(method: String) =
    JsonRpcRequest("2.0", method, None, Some(JInt(1)))

  val fakeWorld = blockchain.getReadOnlyWorldStateProxy(
    None,
    UInt256.Zero,
    ByteString.empty,
    noEmptyAccounts = false,
    ethCompatibleStorage = true
  )
}
