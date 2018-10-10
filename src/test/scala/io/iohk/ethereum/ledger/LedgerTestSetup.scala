package io.iohk.ethereum.ledger

import java.util.concurrent.Executors

import akka.util.ByteString
import akka.util.ByteString.{ empty => bEmpty }
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.ethash.validators.{ OmmersValidator, StdOmmersValidator }
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderParentNotFoundError
import io.iohk.ethereum.consensus.validators.{ BlockHeaderValidator, Validators }
import io.iohk.ethereum.consensus.{ GetBlockHeaderByHash, GetNBlocksBack, TestConsensus }
import io.iohk.ethereum.crypto.{ generateKeyPair, kec256 }
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationAfterExecError
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.{ BlockchainConfig, Config, DaoForkConfig, MonetaryPolicyConfig }
import io.iohk.ethereum.{ Fixtures, Mocks, ObjectGenerators }
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.bouncycastle.util.encoders.Hex
import org.scalamock.handlers.{ CallHandler0, CallHandler1, CallHandler4 }
import org.scalamock.scalatest.MockFactory

import scala.concurrent.ExecutionContext

// scalastyle:off magic.number
trait TestSetup extends SecureRandomBuilder with EphemBlockchainTestSetup {
  //+ cake overrides
  // Give a more specific type to Ledger, it is needed by the tests
  override lazy val ledger: LedgerImpl = newLedger()

  val prep: BlockPreparator = consensus.blockPreparator
  //- cake overrides

  val originKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val receiverKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of bouncycastle encoding
  val originAddress = Address(kec256(originKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))
  val receiverAddress = Address(kec256(receiverKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))
  val minerAddress = Address(666)

  val defaultBlockHeader = BlockHeader(
    parentHash = bEmpty,
    ommersHash = bEmpty,
    beneficiary = bEmpty,
    stateRoot = bEmpty,
    transactionsRoot = bEmpty,
    receiptsRoot = bEmpty,
    logsBloom = bEmpty,
    difficulty = 1000000,
    number = blockchainConfig.homesteadBlockNumber + 1,
    gasLimit = 1000000,
    gasUsed = 0,
    unixTimestamp = 1486752441,
    extraData = bEmpty,
    mixHash = bEmpty,
    nonce = bEmpty
  )

  val defaultTx = Transaction(
    nonce = 42,
    gasPrice = 1,
    gasLimit = 90000,
    receivingAddress = receiverAddress,
    value = 0,
    payload = ByteString.empty)

  val defaultLog = TxLogEntry(
    loggerAddress = originAddress,
    logTopics = Seq(ByteString(Hex.decode("962cd36cf694aa154c5d3a551f19c98f356d906e96828eeb616e16fae6415738"))),
    data = ByteString(Hex.decode("1" * 128))
  )

  val initialOriginBalance: UInt256 = 100000000
  val initialMinerBalance: UInt256 = 2000000

  val initialOriginNonce: BigInt = defaultTx.nonce

  val defaultAddressesToDelete = Set(Address(Hex.decode("01")), Address(Hex.decode("02")), Address(Hex.decode("03")))
  val defaultLogs = Seq(defaultLog.copy(loggerAddress = defaultAddressesToDelete.head))
  val defaultGasPrice: UInt256 = 10
  val defaultGasLimit: UInt256 = 1000000
  val defaultValue: BigInt = 1000

  val emptyWorld: InMemoryWorldStateProxy = BlockchainImpl(storagesInstance.storages)
    .getWorldStateProxy(-1, UInt256.Zero, None, noEmptyAccounts = false, ethCompatibleStorage = true)

  val worldWithMinerAndOriginAccounts: InMemoryWorldStateProxy = InMemoryWorldStateProxy.persistState(emptyWorld
    .saveAccount(originAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))
    .saveAccount(receiverAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))
    .saveAccount(minerAddress, Account(balance = initialMinerBalance)))

  val initialWorld: InMemoryWorldStateProxy = InMemoryWorldStateProxy.persistState(
    defaultAddressesToDelete.foldLeft(worldWithMinerAndOriginAccounts){
      (recWorld, address) => recWorld.saveAccount(address, Account.empty())
    }
  )
}

trait BlockchainSetup extends TestSetup {
  val blockchainStorages: storagesInstance.Storages = storagesInstance.storages

  val validBlockParentHeader: BlockHeader = defaultBlockHeader.copy(stateRoot = initialWorld.stateRootHash)
  val validBlockHeader: BlockHeader = defaultBlockHeader.copy(
    stateRoot = initialWorld.stateRootHash,
    parentHash = validBlockParentHeader.hash,
    beneficiary = minerAddress.bytes,
    receiptsRoot = Account.EmptyStorageRootHash,
    logsBloom = BloomFilter.EmptyBloomFilter,
    gasLimit = defaultGasLimit,
    gasUsed = 0
  )
  val validBlockBodyWithNoTxs: BlockBody = BlockBody(Nil, Nil)

  blockchain.save(validBlockParentHeader)
  blockchain.save(validBlockParentHeader.hash, validBlockBodyWithNoTxs)
  storagesInstance.storages.appStateStorage.putBestBlockNumber(validBlockParentHeader.number)
  storagesInstance.storages.totalDifficultyStorage.put(validBlockParentHeader.hash, 0)

  val validTx: Transaction = defaultTx.copy(
    nonce = initialOriginNonce,
    gasLimit = defaultGasLimit,
    value = defaultValue
  )
  val validStxSignedByOrigin: SignedTransactionWithSender = SignedTransaction.sign(validTx, originKeyPair, Some(blockchainConfig.chainId))
}

trait DaoForkTestSetup extends TestSetup with MockFactory {

  lazy val testBlockchain: BlockchainImpl = mock[BlockchainImpl]
  val worldState: InMemoryWorldStateProxy = mock[InMemoryWorldStateProxy]
  val proDaoBlock: Block = Fixtures.Blocks.ProDaoForkBlock.block

  val supportDaoForkConfig: DaoForkConfig = new DaoForkConfig {
    override val blockExtraData: Option[ByteString] = Some(ByteString("refund extra data"))
    override val range: Int = 10
    override val drainList: Seq[Address] = Seq(Address(1), Address(2), Address(3))
    override val forkBlockHash: ByteString = proDaoBlock.header.hash
    override val forkBlockNumber: BigInt = proDaoBlock.header.number
    override val refundContract: Option[Address] = Some(Address(4))
  }

  val proDaoBlockchainConfig: BlockchainConfig = new BlockchainConfig {
    override val frontierBlockNumber: BigInt = blockchainConfig.frontierBlockNumber
    override val accountStartNonce: UInt256 = blockchainConfig.accountStartNonce
    override val homesteadBlockNumber: BigInt = blockchainConfig.homesteadBlockNumber
    override val difficultyBombPauseBlockNumber: BigInt = blockchainConfig.difficultyBombPauseBlockNumber
    override val difficultyBombRemovalBlockNumber: BigInt = blockchainConfig.difficultyBombRemovalBlockNumber
    override val eip155BlockNumber: BigInt = blockchainConfig.eip155BlockNumber
    override val monetaryPolicyConfig: MonetaryPolicyConfig = blockchainConfig.monetaryPolicyConfig
    override val eip161BlockNumber: BigInt = blockchainConfig.eip161BlockNumber
    override val eip160BlockNumber: BigInt = blockchainConfig.eip160BlockNumber
    override val eip150BlockNumber: BigInt = blockchainConfig.eip150BlockNumber
    override val byzantiumBlockNumber: BigInt = blockchainConfig.byzantiumBlockNumber
    override val chainId: Byte = 0x01.toByte
    override val difficultyBombContinueBlockNumber: BigInt = blockchainConfig.difficultyBombContinueBlockNumber
    override val daoForkConfig: Option[DaoForkConfig] = Some(supportDaoForkConfig)
    override val customGenesisFileOpt: Option[String] = None
    override val eip106BlockNumber: BigInt= Long.MaxValue
    override val maxCodeSize: Option[BigInt] = None
    val gasTieBreaker: Boolean = false
    val ethCompatibleStorage: Boolean = true
  }

  (testBlockchain.getBlockHeaderByHash _).expects(proDaoBlock.header.parentHash).returning(Some(Fixtures.Blocks.DaoParentBlock.header))
  (testBlockchain.getWorldStateProxy _)
    .expects(proDaoBlock.header.number, proDaoBlockchainConfig.accountStartNonce, Some(Fixtures.Blocks.DaoParentBlock.header.stateRoot), false, true)
    .returning(worldState)
}

trait BinarySimulationChopSetup {
  sealed trait TxError
  case object TxError extends TxError

  val minimalGas:BigInt = 20000
  val maximalGas:BigInt = 100000
  val stepGas: BigInt = 625

  val testGasValues: List[BigInt] = minimalGas.to(maximalGas, stepGas).toList

  val mockTransaction: BigInt => BigInt => Option[TxError] =
    minimalWorkingGas => gasLimit => if (gasLimit >= minimalWorkingGas) None else Some(TxError)
}

trait TestSetupWithVmAndValidators extends EphemBlockchainTestSetup {
  //+ cake overrides
  override lazy val vm: VMImpl = new VMImpl

  // Make type more specific
  override lazy val consensus: TestConsensus = buildTestConsensus()
  //- cake overrides

  val blockQueue: BlockQueue

  implicit val testContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))

  class TestLedgerImpl(validators: Validators)(implicit testContext: ExecutionContext) extends LedgerImpl(
    blockchain,
    blockQueue,
    blockchainConfig,
    consensus.withValidators(validators).withVM(new Mocks.MockVM()),
    testContext
  )

  override lazy val ledger = new TestLedgerImpl(successValidators)

  def randomHash(): ByteString =
    ObjectGenerators.byteStringOfLengthNGen(32).sample.get

  val defaultHeader = BlockHeader(
    parentHash = bEmpty,
    ommersHash = bEmpty,
    beneficiary = bEmpty,
    stateRoot = bEmpty,
    transactionsRoot = bEmpty,
    receiptsRoot = bEmpty,
    logsBloom = bEmpty,
    difficulty = 100,
    number = 1,
    gasLimit = 1000000,
    gasUsed = 0,
    unixTimestamp = 0,
    extraData = bEmpty,
    mixHash = bEmpty,
    nonce = bEmpty
  )

  val genesisHeader: BlockHeader = defaultHeader.copy(number = 0, extraData = ByteString("genesis"))

  def getBlock(
    number: BigInt = 1,
    difficulty: BigInt = 100,
    parent: ByteString = randomHash(),
    salt: ByteString = randomHash(),
    ommers: Seq[BlockHeader] = Nil): Block =
    Block(
      defaultHeader.copy(
        parentHash = parent,
        difficulty = difficulty,
        number = number,
        extraData = salt),
      BlockBody(Nil, ommers))

  def getChain(from: BigInt, to: BigInt, parent: ByteString = randomHash()): List[Block] =
    if (from > to) {
      Nil
    } else {
      val block = getBlock(number = from, parent = parent)
      block :: getChain(from + 1, to, block.header.hash)
    }

  def getChainHeaders(from: BigInt, to: BigInt, parent: ByteString = randomHash()): List[BlockHeader] =
    getChain(from, to, parent).map(_.header)

  val receipts = Seq(Receipt.withHashOutcome(randomHash(), 50000, randomHash(), Nil))

  val currentTd = 99999

  val bestNum = BigInt(5)

  val bestBlock: Block = getBlock(bestNum, currentTd / 2)

  val execError = ValidationAfterExecError("error")

  object FailHeaderValidation extends Mocks.MockValidatorsAlwaysSucceed {
    override val blockHeaderValidator: BlockHeaderValidator =
      (_: BlockHeader, _: GetBlockHeaderByHash) => Left(HeaderParentNotFoundError)
  }

  object NotFailAfterExecValidation extends Mocks.MockValidatorsAlwaysSucceed {
    override def validateBlockAfterExecution(block: Block, stateRootHash: ByteString,receipts: Seq[Receipt], gasUsed: BigInt)
    : Either[BlockExecutionError, BlockExecutionSuccess] = Right(BlockExecutionSuccess)
  }

  lazy val failLedger = new TestLedgerImpl(FailHeaderValidation)

  lazy val ledgerNotFailingAfterExecValidation = new TestLedgerImpl(NotFailAfterExecValidation)
}

trait MockBlockchain extends MockFactory { self: TestSetupWithVmAndValidators =>
  //+ cake overrides
  override lazy val blockchain: BlockchainImpl = mock[BlockchainImpl]
  //- cake overrides

  class MockBlockQueue extends BlockQueue(null, 10, 10)
  val blockQueue: BlockQueue = mock[MockBlockQueue]

  def setBlockExists(block: Block, inChain: Boolean, inQueue: Boolean): CallHandler1[ByteString, Boolean] = {
    (blockchain.getBlockByHash _).expects(block.header.hash).anyNumberOfTimes().returning(Some(block).filter(_ => inChain))
    (blockQueue.isQueued _).expects(block.header.hash).anyNumberOfTimes().returning(inQueue)
  }

  def setBestBlock(block: Block): CallHandler0[BigInt] = {
    (blockchain.getBestBlock _).expects().returning(block)
    (blockchain.getBestBlockNumber _).expects().anyNumberOfTimes().returning(block.header.number)
  }

  def setBestBlockNumber(num: BigInt): CallHandler0[BigInt] =
    (blockchain.getBestBlockNumber _).expects().returning(num)

  def setTotalDifficultyForBlock(block: Block, td: BigInt): CallHandler1[ByteString, Option[BigInt]] =
    (blockchain.getTotalDifficultyByHash _).expects(block.header.hash).returning(Some(td))

  def expectBlockSaved(
    block: Block,
    receipts: Seq[Receipt],
    td: BigInt,
    saveAsBestBlock: Boolean
  ): CallHandler4[Block, Seq[Receipt], BigInt, Boolean, Unit] = {
    (blockchain.save(_: Block, _: Seq[Receipt], _: BigInt, _: Boolean))
      .expects(block, receipts, td, saveAsBestBlock).once()
  }

  def setHeaderByHash(hash: ByteString, header: Option[BlockHeader]): CallHandler1[ByteString, Option[BlockHeader]] =
    (blockchain.getBlockHeaderByHash _).expects(hash).returning(header)

  def setBlockByNumber(number: BigInt, block: Option[Block]): CallHandler1[BigInt, Option[Block]] =
    (blockchain.getBlockByNumber _).expects(number).returning(block)

  def setGenesisHeader(header: BlockHeader): CallHandler1[ByteString, Option[BlockHeader]] = {
    (blockchain.getBlockHeaderByNumber _).expects(BigInt(0)).returning(Some(header))
    setHeaderByHash(header.parentHash, None)
  }
}

trait EphemBlockchain extends TestSetupWithVmAndValidators with MockFactory {
  val blockQueue = BlockQueue(blockchain, SyncConfig(Config.config))

  lazy val ledgerWithMockedBlockExecution: LedgerImpl = new TestLedgerImpl(validators){
    override private[ledger] lazy val blockExecution = mock[BlockExecution]
  }
}

trait OmmersTestSetup extends EphemBlockchain {
  object OmmerValidation extends Mocks.MockValidatorsAlwaysSucceed {
    override val ommersValidator: OmmersValidator = (
      parentHash: ByteString,
      blockNumber: BigInt,
      ommers: Seq[BlockHeader],
      getBlockHeaderByHash: GetBlockHeaderByHash,
      getNBlocksBack: GetNBlocksBack
    ) => new StdOmmersValidator(blockchainConfig, blockHeaderValidator)
      .validate(parentHash, blockNumber, ommers, getBlockHeaderByHash, getNBlocksBack)
  }

  override lazy val ledgerWithMockedBlockExecution: LedgerImpl = new TestLedgerImpl(OmmerValidation){
    override private[ledger] lazy val blockExecution = mock[BlockExecution]
  }
}
