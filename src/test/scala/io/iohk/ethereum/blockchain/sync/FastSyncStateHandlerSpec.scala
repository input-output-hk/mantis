package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.util.ByteString
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.blockchain.sync.FastSync.SyncState
import io.iohk.ethereum.blockchain.sync.FastSyncStateHandler._
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderParentNotFoundError
import io.iohk.ethereum.consensus.validators.{BlockHeaderValid, BlockHeaderValidator, Validators}
import io.iohk.ethereum.domain.{Account, BlockHeader, Receipt}
import io.iohk.ethereum.ledger.BloomFilter
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.Config.SyncConfig
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.util.Random

class FastSyncStateHandlerSpec  extends FlatSpec with Matchers with MockFactory{
  "FastSyncStateHandler" should "not allow empty headers response" in new TestSetup() {
    val (comm, state) = stateHandler.handleHeaders(peer1, Nil, syncConfig.blockHeadersPerRequest)(defaultState)
    state shouldEqual defaultState
    comm match {
      case ContinueSyncing(Some(BlackListCommand(peer1, _))) => succeed
      case _ => fail
    }
  }

  it should "not process headers response which not form chain" in new TestSetup() {
    val (comm, state) = stateHandler.handleHeaders(
      peer1,
      Random.shuffle(defaultHeadersPacket),
      syncConfig.blockHeadersPerRequest)(defaultState)
    state shouldEqual defaultState
    comm match {
      case ContinueSyncing(Some(BlackListCommand(peer1, _))) => succeed
      case _ => fail
    }
  }

  it should "not process headers response which start from wrong block" in new TestSetup() {
    val (comm, state) = stateHandler.handleHeaders(
      peer1,
      defaultHeadersPacket.tail,
      syncConfig.blockHeadersPerRequest)(defaultState)
    state shouldEqual defaultState
    comm match {
      case ContinueSyncing(Some(BlackListCommand(peer1, _))) => succeed
      case _ => fail
    }
  }

  it should "properly handle correct block headers" in new TestSetup() {

    val (comm, state) = stateHandler.handleHeaders(peer1, defaultHeadersPacket, syncConfig.blockHeadersPerRequest)(defaultState)
    val expectedState = defaultState.copy(
      blockBodiesQueue = defaultHeadersPacket.map(_.hash),
      receiptsQueue = defaultHeadersPacket.map(_.hash),
      nextBlockToFullyValidate = firstNewBlockToValidate + syncConfig.fastSyncBlockValidationK / 2 + 10,
      bestBlockHeaderNumber = defaultBestBlockNumber + syncConfig.blockHeadersPerRequest
    )

    state shouldEqual expectedState

    comm match {
      case ContinueSyncing(None) => succeed
      case _ => fail
    }
  }

  it should "properly handle block header validation failure" in new TestSetup(_validators = new Mocks.MockValidatorsAlwaysSucceed {
    override val blockHeaderValidator: BlockHeaderValidator = (blockHeader: BlockHeader, getBlockHeaderByHash: ByteString => Option[BlockHeader]) =>
      if (blockHeader.number == 5001)
        Left(HeaderParentNotFoundError)
      else
        Right(BlockHeaderValid)
  }) {
    val (comm, state) = stateHandler.handleHeaders(
      peer1,
      defaultHeadersPacket,
      syncConfig.blockHeadersPerRequest)(defaultState)

    val expectedState = defaultState.copy(
      bestBlockHeaderNumber = firstNewBlockToValidate - syncConfig.fastSyncBlockValidationN - 1,
      nextBlockToFullyValidate =  firstNewBlockToValidate - syncConfig.fastSyncBlockValidationN
    )

    state shouldEqual expectedState

    comm match {
      case ContinueSyncing(Some(BlackListCommand(peer1, _))) => succeed
      case _ => fail
    }
  }

  it should "properly initiate update of target block afer importing last header" in new TestSetup {
    val targetBlockNumber = 5000
    val targetBlock = defaultTargetBlockHeader.copy(number = targetBlockNumber)
    val safe = targetBlockNumber + syncConfig.fastSyncBlockValidationX

    val startState = SyncState(
      targetBlock,
      safe,
      bestBlockHeaderNumber = defaultBestBlockNumber
    )

    val expectedState = startState.copy(
      blockBodiesQueue = defaultHeadersPacket.map(_.hash),
      receiptsQueue = defaultHeadersPacket.map(_.hash),
      bestBlockHeaderNumber = defaultBestBlockNumber + syncConfig.blockHeadersPerRequest,
      nextBlockToFullyValidate = defaultBestBlockNumber + syncConfig.blockHeadersPerRequest + 1
    )

    val (comm, state1) = stateHandler.handleHeaders(peer1, defaultHeadersPacket, syncConfig.blockHeadersPerRequest)(startState)
    state1 shouldEqual expectedState

    comm match {
      case InitTargetBlockUpdate(ImportedLastBlock) => succeed
      case _ => fail
    }
  }

  it should "properly initiate update of target block after failing to validate one of last blocks" in new TestSetup(
    _validators = new Mocks.MockValidatorsAlwaysSucceed {
    override val blockHeaderValidator: BlockHeaderValidator = (blockHeader: BlockHeader, getBlockHeaderByHash: ByteString => Option[BlockHeader]) =>
      if (blockHeader.number == 5006)
        Left(HeaderParentNotFoundError)
      else
        Right(BlockHeaderValid)
  }) {
    val targetBlockNumber = 5000
    val targetBlock = defaultTargetBlockHeader.copy(number = targetBlockNumber)
    val safe = targetBlockNumber + syncConfig.fastSyncBlockValidationX

    val startState = SyncState(
      targetBlock,
      safe,
      bestBlockHeaderNumber = defaultBestBlockNumber
    )

    val expectedState = startState.copy(
      bestBlockHeaderNumber = 5006 - syncConfig.fastSyncBlockValidationN - 1,
      nextBlockToFullyValidate = 5006 - syncConfig.fastSyncBlockValidationN
    )

    val (comm, state1) = stateHandler.handleHeaders(peer1, defaultHeadersPacket, syncConfig.blockHeadersPerRequest)(startState)
    state1 shouldEqual expectedState

    comm match {
      case InitTargetBlockUpdate(LastBlockValidationFailed(_)) => succeed
      case _ => fail
    }
  }

  it should "properly handle new target block, and switch to state download" in new TestSetup {
    val targetBlockNumber = 5000
    val targetBlock = defaultTargetBlockHeader.copy(number = targetBlockNumber)
    val safe = targetBlockNumber + syncConfig.fastSyncBlockValidationX


    val startState = SyncState(
      targetBlock,
      safe,
      bestBlockHeaderNumber = safe,
      updatingTargetBlock = true
    )

    val correctNewTargetBlock = targetBlock.copy(number = targetBlockNumber + syncConfig.maxTargetDifference)


    val expectedState = startState.copy(
        pendingMptNodes = Seq(StateMptNodeHash(correctNewTargetBlock.stateRoot)),
        updatingTargetBlock = false
    )

    val (comm, state1) = stateHandler.handleNewTargetBlock(ImportedLastBlock, correctNewTargetBlock)(startState)
    state1 shouldEqual expectedState
    comm match {
      case BackToSyncing => succeed
      case _ => fail
    }
  }

  it should "properly handle new target block, and switch to higher block" in new TestSetup {
    val targetBlockNumber = 5000
    val targetBlock = defaultTargetBlockHeader.copy(number = targetBlockNumber)
    val safe = targetBlockNumber + syncConfig.fastSyncBlockValidationX


    val startState = SyncState(
      targetBlock,
      safe,
      bestBlockHeaderNumber = safe,
      updatingTargetBlock = true
    )

    val correctNewTargetBlock = targetBlock.copy(number = targetBlockNumber + syncConfig.maxTargetDifference + 1)


    val expectedState = startState.copy(
      targetBlock = correctNewTargetBlock,
      safeDownloadTarget = correctNewTargetBlock.number + syncConfig.fastSyncBlockValidationX,
      updatingTargetBlock = false
    )

    val (comm, state1) = stateHandler.handleNewTargetBlock(ImportedLastBlock, correctNewTargetBlock)(startState)
    state1 shouldEqual expectedState
    comm match {
      case BackToSyncing => succeed
      case _ => fail
    }
  }

  it should "re-ask for new target block, after receiving target block which has lower number" in new TestSetup {
    val targetBlockNumber = 5000
    val targetBlock = defaultTargetBlockHeader.copy(number = targetBlockNumber)
    val safe = targetBlockNumber + syncConfig.fastSyncBlockValidationX


    val startState = SyncState(
      targetBlock,
      safe,
      bestBlockHeaderNumber = safe,
      updatingTargetBlock = true
    )

    val correctNewTargetBlock = targetBlock.copy(number = targetBlockNumber - 1)


    val expectedState = startState.copy(
      targetBlockUpdateFailures = 1,
      updatingTargetBlock = true
    )

    val (comm, state1) = stateHandler.handleNewTargetBlock(ImportedLastBlock, correctNewTargetBlock)(startState)
    state1 shouldEqual expectedState
    comm match {
      case AskForNewTarget(_) => succeed
      case _ => fail
    }
  }

  it should "properly handle new target block update, after validation failure" in new TestSetup {
    val targetBlockNumber = 5000
    val targetBlock = defaultTargetBlockHeader.copy(number = targetBlockNumber)
    val safe = targetBlockNumber + syncConfig.fastSyncBlockValidationX


    val startState = SyncState(
      targetBlock,
      safe,
      bestBlockHeaderNumber = safe,
      updatingTargetBlock = true
    )

    val correctNewTargetBlock = targetBlock.copy(number = targetBlockNumber + syncConfig.maxTargetDifference + 1)


    val expectedState = startState.copy(
      targetBlock = correctNewTargetBlock,
      safeDownloadTarget = correctNewTargetBlock.number + syncConfig.fastSyncBlockValidationX,
      updatingTargetBlock = false,
      targetBlockUpdateFailures = 1,
    )

    val (comm, state1) = stateHandler.handleNewTargetBlock(LastBlockValidationFailed(BlackListCommand(peer1, "")), correctNewTargetBlock)(startState)
    state1 shouldEqual expectedState
    comm match {
      case BackToSyncing => succeed
      case _ => fail
    }
  }

  it should "properly handle new blockbodies, when db error happens" in new TestSetup {
    val bodies = defaultHeadersPacket.map(_ => BlockBody(Nil, Nil))
    val (comm, state) = stateHandler.handleBlockBodies(peer1, defaultHeadersPacket.map(_.hash), bodies)(defaultState)

    val expectedState = defaultState.copy(
      bestBlockHeaderNumber = defaultBestBlockNumber - 2 * syncConfig.blockHeadersPerRequest
    )
    state shouldEqual expectedState
    comm match {
      case ContinueSyncing(None) => succeed
      case _ => fail
    }
  }

  it should "properly handle new receipts, when db error happens" in new TestSetup {
    val receipts = defaultHeadersPacket.map(_ => Seq.empty[Receipt])
    val (comm, state) = stateHandler.handleReceipts(peer1, defaultHeadersPacket.map(_.hash), receipts)(defaultState)

    val expectedState = defaultState.copy(
      bestBlockHeaderNumber = defaultBestBlockNumber - 2 * syncConfig.blockHeadersPerRequest
    )
    state shouldEqual expectedState
    comm match {
      case ContinueSyncing(None) => succeed
      case _ => fail
    }
  }

  it should "properly handle new receipts, bodies and headers" in new TestSetup {
    val headers = defaultHeadersPacket
    val bodies = headers.map(_ => BlockBody(Nil, Nil))
    val receipts = headers.map(_ => Seq.empty[Receipt])

    val (comm, state) = stateHandler.handleHeaders(peer1, headers, syncConfig.blockHeadersPerRequest)(defaultState)
    val expectedState = defaultState.copy(
      blockBodiesQueue = defaultHeadersPacket.map(_.hash),
      receiptsQueue = defaultHeadersPacket.map(_.hash),
      nextBlockToFullyValidate = firstNewBlockToValidate + syncConfig.fastSyncBlockValidationK / 2 + 10,
      bestBlockHeaderNumber = defaultBestBlockNumber + syncConfig.blockHeadersPerRequest
    )

    state shouldEqual expectedState

    val (comm1, state1) = stateHandler.requestReceipts(peer1)(state)

    comm1 shouldEqual RequestReceipts(peer1, state.receiptsQueue)
    state1 shouldEqual state.copy(
      receiptsQueue = Seq.empty[ByteString]
    )

    val (comm2, state2) = stateHandler.handleReceipts(peer1, state.receiptsQueue, receipts)(state1)
    comm2 shouldEqual ContinueSyncing(None)
    state2 shouldEqual state1

    // We must process bodies to have best block updated
    appStateStorage.getBestBlockNumber() shouldEqual 0

    val (comm3, state3) = stateHandler.requestBodies(peer1)(state2)
    comm3 shouldEqual RequestBodies(peer1, state2.blockBodiesQueue)
    state3 shouldEqual state2.copy(
      blockBodiesQueue = Seq.empty[ByteString]
    )

    val (comm4, state4) = stateHandler.handleBlockBodies(peer1, comm3.bodiesToGet, bodies)(state3)
    state4 shouldEqual state3

    // We have processed headers, receipts, and bodies, best block should be updated
    appStateStorage.getBestBlockNumber() shouldEqual headers.last.number
  }

}

class TestSetup(_validators: Validators = new Mocks.MockValidatorsAlwaysSucceed)
  extends EphemBlockchainTestSetup {
  import TestUtils.generateChain

  override lazy val syncConfig = SyncConfig(
    doFastSync = true,
    printStatusInterval = 1.hour,
    persistStateSnapshotInterval = 20.seconds,
    targetBlockOffset = 500,
    branchResolutionRequestSize = 20,
    blacklistDuration = 5.seconds,
    syncRetryInterval = 1.second,
    checkForNewBlockInterval = 1.second,
    startRetryInterval = 500.milliseconds,
    blockChainOnlyPeersPoolSize = 100,
    maxConcurrentRequests = 10,
    blockHeadersPerRequest = 10,
    blockBodiesPerRequest = 10,
    nodesPerRequest = 10,
    receiptsPerRequest = 10,
    minPeersToChooseTargetBlock = 1,
    peerResponseTimeout = 1.second,
    peersScanInterval = 500.milliseconds,
    fastSyncThrottle = 100.milliseconds,
    maxQueuedBlockNumberAhead = 10,
    maxQueuedBlockNumberBehind = 10,
    maxNewBlockHashAge = 20,
    maxNewHashes = 64,
    broadcastNewBlockHashes = true,
    redownloadMissingStateNodes = false,
    fastSyncBlockValidationK = 100,
    fastSyncBlockValidationN = 2048,
    fastSyncBlockValidationX = 10,
    maxTargetDifference =  5,
    maximumTargetUpdateFailures = 1
  )
  val EmptyTrieRootHash: ByteString = Account.EmptyStorageRootHash
  val baseBlockHeader = BlockHeader(
    parentHash = ByteString("unused"),
    ommersHash = ByteString("unused"),
    beneficiary = ByteString("unused"),
    stateRoot = EmptyTrieRootHash,
    transactionsRoot = EmptyTrieRootHash,
    receiptsRoot = EmptyTrieRootHash,
    logsBloom = BloomFilter.EmptyBloomFilter,
    difficulty = 0,
    number = 0,
    gasLimit = 0,
    gasUsed = 0,
    unixTimestamp = 0,
    extraData = ByteString("unused"),
    mixHash = ByteString("unused"),
    nonce = ByteString("unused"))
  val appStateStorage =  storagesInstance.storages.appStateStorage
  blockchain.save(baseBlockHeader.parentHash, BigInt(0))
  val fakeRand: Int => Int = n => 10
  lazy val stateHandler = new FastSyncStateHandler(blockchain, _validators, syncConfig, appStateStorage, fakeRand)

  val defaultExpectedTargetBlock = 10000
  val defaultSafeDownloadTarget = defaultExpectedTargetBlock + syncConfig.fastSyncBlockValidationX
  val defaultBestBlockNumber = 5000
  val defaultStateRoot = "deae1dfad5ec8dcef15915811e1f044d2543674fd648f94345231da9fc2646cc"
  val defaultTargetBlockHeader = baseBlockHeader.copy(
    number = defaultExpectedTargetBlock,
    stateRoot = ByteString(Hex.decode(defaultStateRoot)))

  val defaultState = SyncState(
    defaultTargetBlockHeader,
    defaultSafeDownloadTarget,
    bestBlockHeaderNumber = defaultBestBlockNumber
  )
  val firstNewBlockToValidate = defaultBestBlockNumber + 1
  val defaultHeadersPacket =  generateChain(baseBlockHeader)(firstNewBlockToValidate, syncConfig.blockHeadersPerRequest)

  val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), null, false)
}