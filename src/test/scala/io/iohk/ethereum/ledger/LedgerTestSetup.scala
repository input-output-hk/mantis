package io.iohk.ethereum.ledger

import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}

import cats.data.NonEmptyList

import monix.execution.Scheduler

import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.bouncycastle.util.encoders.Hex
import org.scalamock.handlers.CallHandler0
import org.scalamock.handlers.CallHandler1
import org.scalamock.handlers.CallHandler4
import org.scalamock.scalatest.MockFactory

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.GetBlockHeaderByHash
import io.iohk.ethereum.consensus.GetNBlocksBack
import io.iohk.ethereum.consensus.TestConsensus
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.consensus.pow.validators.OmmersValidator
import io.iohk.ethereum.consensus.pow.validators.StdOmmersValidator
import io.iohk.ethereum.consensus.validators.BlockHeaderError
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderParentNotFoundError
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.consensus.validators.BlockHeaderValidator
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationAfterExecError
import io.iohk.ethereum.ledger.PC
import io.iohk.ethereum.ledger.PR
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.DaoForkConfig
import io.iohk.ethereum.vm.ProgramError
import io.iohk.ethereum.vm.ProgramResult

// scalastyle:off magic.number
trait TestSetup extends SecureRandomBuilder with EphemBlockchainTestSetup {
  //+ cake overrides

  val prep: BlockPreparator = mining.blockPreparator
  //- cake overrides

  val originKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val receiverKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of bouncycastle encoding
  val originAddress: Address = Address(
    kec256(originKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail)
  )
  val receiverAddress: Address = Address(
    kec256(receiverKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail)
  )
  val minerAddress: Address = Address(666)

  val defaultBlockHeader: BlockHeader = Fixtures.Blocks.ValidBlock.header.copy(
    difficulty = 1000000,
    number = blockchainConfig.forkBlockNumbers.homesteadBlockNumber + 1,
    gasLimit = 1000000,
    gasUsed = 0,
    unixTimestamp = 1486752441
  )

  val defaultTx: LegacyTransaction = LegacyTransaction(
    nonce = 42,
    gasPrice = 1,
    gasLimit = 90000,
    receivingAddress = receiverAddress,
    value = 0,
    payload = ByteString.empty
  )

  val defaultLog: TxLogEntry = TxLogEntry(
    loggerAddress = originAddress,
    logTopics = Seq(ByteString(Hex.decode("962cd36cf694aa154c5d3a551f19c98f356d906e96828eeb616e16fae6415738"))),
    data = ByteString(Hex.decode("1" * 128))
  )

  val defaultChainWeight: ChainWeight = ChainWeight.zero.increase(defaultBlockHeader)

  val initialOriginBalance: UInt256 = 100000000
  val initialMinerBalance: UInt256 = 2000000

  val initialOriginNonce: BigInt = defaultTx.nonce

  val defaultAddressesToDelete: Set[Address] =
    Set(Address(Hex.decode("01")), Address(Hex.decode("02")), Address(Hex.decode("03")))
  val defaultLogs: Seq[TxLogEntry] = Seq(defaultLog.copy(loggerAddress = defaultAddressesToDelete.head))
  val defaultGasPrice: UInt256 = 10
  val defaultGasLimit: UInt256 = 1000000
  val defaultValue: BigInt = 1000

  val emptyWorld: InMemoryWorldStateProxy = InMemoryWorldStateProxy(
    storagesInstance.storages.evmCodeStorage,
    blockchain.getBackingMptStorage(-1),
    (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
    UInt256.Zero,
    ByteString(MerklePatriciaTrie.EmptyRootHash),
    noEmptyAccounts = false,
    ethCompatibleStorage = true
  )

  val worldWithMinerAndOriginAccounts: InMemoryWorldStateProxy = InMemoryWorldStateProxy.persistState(
    emptyWorld
      .saveAccount(originAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))
      .saveAccount(receiverAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))
      .saveAccount(minerAddress, Account(balance = initialMinerBalance))
  )

  val initialWorld: InMemoryWorldStateProxy = InMemoryWorldStateProxy.persistState(
    defaultAddressesToDelete.foldLeft(worldWithMinerAndOriginAccounts) { (recWorld, address) =>
      recWorld.saveAccount(address, Account.empty())
    }
  )

  def createResult(
      context: PC,
      gasUsed: BigInt,
      gasLimit: BigInt,
      gasRefund: BigInt,
      error: Option[ProgramError] = None,
      returnData: ByteString = bEmpty,
      logs: Seq[TxLogEntry] = Nil,
      addressesToDelete: Set[Address] = Set.empty
  ): PR = ProgramResult(
    returnData = returnData,
    gasRemaining = gasLimit - gasUsed,
    world = context.world,
    addressesToDelete = addressesToDelete,
    logs = logs,
    internalTxs = Nil,
    gasRefund = gasRefund,
    error = error
  )

  sealed trait Changes
  case class UpdateBalance(amount: UInt256) extends Changes
  case object IncreaseNonce extends Changes
  case object DeleteAccount extends Changes

  def applyChanges(
      stateRootHash: ByteString,
      blockchainStorages: BlockchainStorages,
      changes: Seq[(Address, Changes)]
  ): ByteString = {
    val initialWorld = InMemoryWorldStateProxy(
      storagesInstance.storages.evmCodeStorage,
      blockchain.getBackingMptStorage(-1),
      (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      UInt256.Zero,
      stateRootHash,
      noEmptyAccounts = false,
      ethCompatibleStorage = true
    )

    val newWorld = changes.foldLeft[InMemoryWorldStateProxy](initialWorld) { case (recWorld, (address, change)) =>
      change match {
        case UpdateBalance(balanceIncrease) =>
          val accountWithBalanceIncrease =
            recWorld.getAccount(address).getOrElse(Account.empty()).increaseBalance(balanceIncrease)
          recWorld.saveAccount(address, accountWithBalanceIncrease)
        case IncreaseNonce =>
          val accountWithNonceIncrease = recWorld.getAccount(address).getOrElse(Account.empty()).increaseNonce()
          recWorld.saveAccount(address, accountWithNonceIncrease)
        case DeleteAccount =>
          recWorld.deleteAccount(address)
      }
    }
    InMemoryWorldStateProxy.persistState(newWorld).stateRootHash
  }

}

trait BlockchainSetup extends TestSetup {
  val blockchainStorages: storagesInstance.Storages = storagesInstance.storages

  val validBlockParentHeader: BlockHeader = defaultBlockHeader.copy(stateRoot = initialWorld.stateRootHash)
  val validBlockParentBlock: Block = Block(validBlockParentHeader, BlockBody.empty)
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

  blockchainWriter
    .storeBlockHeader(validBlockParentHeader)
    .and(blockchainWriter.storeBlockBody(validBlockParentHeader.hash, validBlockBodyWithNoTxs))
    .and(storagesInstance.storages.appStateStorage.putBestBlockNumber(validBlockParentHeader.number))
    .and(storagesInstance.storages.chainWeightStorage.put(validBlockParentHeader.hash, ChainWeight.zero))
    .commit()

  val validTx: LegacyTransaction = defaultTx.copy(
    nonce = initialOriginNonce,
    gasLimit = defaultGasLimit,
    value = defaultValue
  )
  val validStxSignedByOrigin: SignedTransaction =
    SignedTransaction.sign(validTx, originKeyPair, Some(blockchainConfig.chainId))
}

trait DaoForkTestSetup extends TestSetup with MockFactory {

  lazy val testBlockchainReader: BlockchainReader = mock[BlockchainReader]
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
    override val includeOnForkIdList: Boolean = false
  }

  val proDaoBlockchainConfig: BlockchainConfig = blockchainConfig
    .withUpdatedForkBlocks(
      _.copy(
        eip106BlockNumber = Long.MaxValue,
        atlantisBlockNumber = Long.MaxValue,
        aghartaBlockNumber = Long.MaxValue,
        phoenixBlockNumber = Long.MaxValue,
        petersburgBlockNumber = Long.MaxValue
      )
    )
    .copy(
      chainId = 0x01.toByte,
      networkId = 1,
      daoForkConfig = Some(supportDaoForkConfig),
      customGenesisFileOpt = None,
      maxCodeSize = None,
      bootstrapNodes = Set(),
      gasTieBreaker = false,
      ethCompatibleStorage = true
    )

  val parentBlockHeader = Fixtures.Blocks.DaoParentBlock.header

  (testBlockchainReader.getBlockHeaderByHash _)
    .expects(proDaoBlock.header.parentHash)
    .returning(Some(parentBlockHeader))
  (testBlockchain.getBackingMptStorage _)
    .expects(*)
    .returning(storagesInstance.storages.stateStorage.getBackingStorage(1920000))
}

trait BinarySimulationChopSetup {
  sealed trait TxError
  case object TxError extends TxError

  val minimalGas: BigInt = 20000
  val maximalGas: BigInt = 100000
  val stepGas: BigInt = 625

  val testGasValues: List[BigInt] = minimalGas.to(maximalGas, stepGas).toList

  val mockTransaction: BigInt => BigInt => Option[TxError] =
    minimalWorkingGas => gasLimit => if (gasLimit >= minimalWorkingGas) None else Some(TxError)
}

trait TestSetupWithVmAndValidators extends EphemBlockchainTestSetup {
  //+ cake overrides
  override lazy val vm: VMImpl = new VMImpl

  // Make type more specific
  override lazy val mining: TestConsensus = buildTestConsensus()
  //- cake overrides

  val blockQueue: BlockQueue

  implicit val schedulerContext: Scheduler = Scheduler.fixedPool("ledger-test-pool", 4)

  override lazy val blockImport: BlockImport = mkBlockImport()

  def randomHash(): ByteString =
    ObjectGenerators.byteStringOfLengthNGen(32).sample.get

  val defaultHeader: BlockHeader = Fixtures.Blocks.ValidBlock.header.copy(
    difficulty = 100,
    number = 1,
    gasLimit = 1000000,
    gasUsed = 0,
    unixTimestamp = 0
  )

  val genesisHeader: BlockHeader = defaultHeader.copy(number = 0, extraData = ByteString("genesis"))

  def getBlock(
      number: BigInt = 1,
      difficulty: BigInt = 100,
      parent: ByteString = randomHash(),
      salt: ByteString = randomHash(),
      ommers: Seq[BlockHeader] = Nil
  ): Block =
    Block(
      defaultHeader.copy(parentHash = parent, difficulty = difficulty, number = number, extraData = salt),
      BlockBody(Nil, ommers)
    )

  def getChain(from: BigInt, to: BigInt, parent: ByteString = randomHash(), difficulty: BigInt = 100): List[Block] =
    if (from > to) {
      Nil
    } else {
      val block = getBlock(number = from, difficulty = difficulty, parent = parent)
      block :: getChain(from + 1, to, block.header.hash, difficulty)
    }

  def getChainNel(
      from: BigInt,
      to: BigInt,
      parent: ByteString = randomHash(),
      difficulty: BigInt = 100
  ): NonEmptyList[Block] =
    NonEmptyList.fromListUnsafe(getChain(from, to, parent, difficulty))

  def getChainHeaders(from: BigInt, to: BigInt, parent: ByteString = randomHash()): List[BlockHeader] =
    getChain(from, to, parent).map(_.header)

  def getChainHeadersNel(from: BigInt, to: BigInt, parent: ByteString = randomHash()): NonEmptyList[BlockHeader] =
    NonEmptyList.fromListUnsafe(getChainHeaders(from, to, parent))

  val receipts: Seq[Receipt] = Seq(Receipt.withHashOutcome(randomHash(), 50000, randomHash(), Nil))

  val currentWeight: ChainWeight = ChainWeight.totalDifficultyOnly(99999)

  val bestNum: BigInt = BigInt(5)

  val bestBlock: Block = getBlock(bestNum, currentWeight.totalDifficulty / 2)

  val execError: ValidationAfterExecError = ValidationAfterExecError("error")

  object FailHeaderValidation extends Mocks.MockValidatorsAlwaysSucceed {
    override val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidator {
      override def validate(
          blockHeader: BlockHeader,
          getBlockHeaderByHash: GetBlockHeaderByHash
      ): Either[BlockHeaderError, BlockHeaderValid] = Left(HeaderParentNotFoundError)

      override def validateHeaderOnly(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
        Left(HeaderParentNotFoundError)
    }
  }

  object NotFailAfterExecValidation extends Mocks.MockValidatorsAlwaysSucceed {
    override def validateBlockAfterExecution(
        block: Block,
        stateRootHash: ByteString,
        receipts: Seq[Receipt],
        gasUsed: BigInt
    ): Either[BlockExecutionError, BlockExecutionSuccess] = Right(BlockExecutionSuccess)
  }

  lazy val failBlockImport: BlockImport = mkBlockImport(validators = FailHeaderValidation)

  lazy val blockImportNotFailingAfterExecValidation: BlockImport = {
    val consensuz = mining.withValidators(NotFailAfterExecValidation).withVM(new Mocks.MockVM())
    val blockValidation = new BlockValidation(consensuz, blockchainReader, blockQueue)
    new BlockImport(
      blockchain,
      blockchainReader,
      blockchainWriter,
      blockQueue,
      blockValidation,
      new BlockExecution(
        blockchain,
        blockchainReader,
        blockchainWriter,
        storagesInstance.storages.evmCodeStorage,
        blockchainConfig,
        consensuz.blockPreparator,
        blockValidation
      ) {
        override def executeAndValidateBlock(
            block: Block,
            alreadyValidated: Boolean = false
        ): Either[BlockExecutionError, Seq[Receipt]] = {
          val emptyWorld = InMemoryWorldStateProxy(
            storagesInstance.storages.evmCodeStorage,
            blockchain.getBackingMptStorage(-1),
            (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
            blockchainConfig.accountStartNonce,
            ByteString(MerklePatriciaTrie.EmptyRootHash),
            noEmptyAccounts = false,
            ethCompatibleStorage = true
          )
          Right(BlockResult(emptyWorld).receipts)
        }
      },
      Scheduler(system.dispatchers.lookup("validation-context"))
    )
  }
}

trait MockBlockchain extends MockFactory { self: TestSetupWithVmAndValidators =>
  //+ cake overrides
  override lazy val blockchainReader: BlockchainReader = mock[BlockchainReader]
  override lazy val blockchainWriter: BlockchainWriter = mock[BlockchainWriter]
  override lazy val blockchain: BlockchainImpl = mock[BlockchainImpl]
  //- cake overrides

  class MockBlockQueue extends BlockQueue(null, null, 10, 10)
  override lazy val blockQueue: BlockQueue = mock[MockBlockQueue]

  def setBlockExists(block: Block, inChain: Boolean, inQueue: Boolean): CallHandler1[ByteString, Boolean] = {
    (blockchainReader.getBlockByHash _)
      .expects(block.header.hash)
      .anyNumberOfTimes()
      .returning(Some(block).filter(_ => inChain))
    (blockQueue.isQueued _).expects(block.header.hash).anyNumberOfTimes().returning(inQueue)
  }

  def setBestBlock(block: Block): CallHandler0[BigInt] = {
    (blockchainReader.getBestBlock _).expects().returning(Some(block))
    (blockchainReader.getBestBlockNumber _).expects().anyNumberOfTimes().returning(block.header.number)
  }

  def setBestBlockNumber(num: BigInt): CallHandler0[BigInt] =
    (blockchainReader.getBestBlockNumber _).expects().returning(num)

  def setChainWeightForBlock(block: Block, weight: ChainWeight): CallHandler1[ByteString, Option[ChainWeight]] =
    setChainWeightByHash(block.hash, weight)

  def setChainWeightByHash(hash: ByteString, weight: ChainWeight): CallHandler1[ByteString, Option[ChainWeight]] =
    (blockchain.getChainWeightByHash _).expects(hash).returning(Some(weight))

  def expectBlockSaved(
      block: Block,
      receipts: Seq[Receipt],
      weight: ChainWeight,
      saveAsBestBlock: Boolean
  ): CallHandler4[Block, Seq[Receipt], ChainWeight, Boolean, Unit] =
    (blockchainWriter
      .save(_: Block, _: Seq[Receipt], _: ChainWeight, _: Boolean))
      .expects(block, receipts, weight, saveAsBestBlock)
      .once()

  def setHeaderInChain(hash: ByteString, result: Boolean = true): CallHandler1[ByteString, Boolean] =
    (blockchain.isInChain _).expects(hash).returning(result)

  def setBlockByNumber(number: BigInt, block: Option[Block]): CallHandler1[BigInt, Option[Block]] =
    (blockchainReader.getBlockByNumber _).expects(number).returning(block)

  def setGenesisHeader(header: BlockHeader): Unit =
    (() => blockchainReader.genesisHeader).expects().returning(header)
}

trait EphemBlockchain extends TestSetupWithVmAndValidators with MockFactory {
  override lazy val blockQueue: BlockQueue = BlockQueue(blockchain, blockchainReader, SyncConfig(Config.config))

  lazy val blockImportWithMockedBlockExecution: BlockImport =
    mkBlockImport(blockExecutionOpt = Some(mock[BlockExecution]))
}

trait CheckpointHelpers {
  private val sampleCheckpoint = ObjectGenerators.fakeCheckpointGen(3, 3).sample.get

  def getCheckpointBlock(parent: Block, difficulty: BigInt, checkpoint: Checkpoint = sampleCheckpoint): Block =
    new CheckpointBlockGenerator().generate(parent, checkpoint)
}

trait OmmersTestSetup extends EphemBlockchain {
  object OmmerValidation extends Mocks.MockValidatorsAlwaysSucceed {
    override val ommersValidator: OmmersValidator = (
        parentHash: ByteString,
        blockNumber: BigInt,
        ommers: Seq[BlockHeader],
        getBlockHeaderByHash: GetBlockHeaderByHash,
        getNBlocksBack: GetNBlocksBack
    ) =>
      new StdOmmersValidator(blockHeaderValidator)
        .validate(parentHash, blockNumber, ommers, getBlockHeaderByHash, getNBlocksBack)
  }

  override lazy val blockImportWithMockedBlockExecution: BlockImport =
    mkBlockImport(validators = OmmerValidation, blockExecutionOpt = Some(mock[BlockExecution]))
}
