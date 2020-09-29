package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString
import io.iohk.ethereum.{Fixtures, ObjectGenerators}
import io.iohk.ethereum.blockchain.sync.SyncSchedulerSpec.{MptNodeData, checkAllDataExists, genMultipleNodeData}
import io.iohk.ethereum.blockchain.sync.SyncStateScheduler.{AlreadyProcessedItem, CannotDecodeMptNode, NotRequestedItem, SchedulerState, SyncResponse}
import io.iohk.ethereum.db.components.{EphemDataSourceComponent, Storages}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.domain.{Account, Address, Blockchain, BlockchainImpl}
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.nodebuilder.PruningConfigBuilder
import io.iohk.ethereum.utils.ByteUtils
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SyncSchedulerSpec extends AnyFlatSpec with Matchers with EitherValues with ScalaCheckPropertyChecks {
  "SyncSchedulerState" should "sync with mptTrie with one account (1 leaf node)" in new TestSetup {
    val prov = getTrieProvider
    val worldHash = prov.buildWorld(Seq(MptNodeData(Address(1), None, Seq(), 20)))
    val (scheduler, schedulerBlockchain, schedulerDb) = buildScheduler()
    val initialState = scheduler.initState(worldHash).get
    val (missingNodes, newState) = scheduler.getMissingNodes(initialState, 1)
    assert(missingNodes.size == 1)
    val responses = prov.getNodes(missingNodes)
    assert(responses.size == 1)
    val result = scheduler.processResponses(newState, responses)
    assert(result.isRight)
    val (newRequests, state) = scheduler.getMissingNodes(result.right.value, 1)
    assert(newRequests.isEmpty)
    assert(state.numberOfPendingRequests == 0)
    scheduler.persistBatch(state, 1)
    assert(schedulerDb.storages.nodeStorage.get(missingNodes.head).isDefined)
  }

  it should "sync with mptTrie with one account with code and storage" in new TestSetup {
    val prov = getTrieProvider
    val worldHash = prov.buildWorld(
      Seq(MptNodeData(Address(1), Some(ByteString(1, 2, 3)), Seq((1, 1)), 20))
    )
    val (scheduler, schedulerBlockchain, schedulerDb) = buildScheduler()
    val initState = scheduler.initState(worldHash).get
    val state1 = exchangeSingleNode(initState, scheduler, prov).right.value
    assert(state1.numberOfPendingRequests > 0)

    val state2 = exchangeSingleNode(state1, scheduler, prov).right.value
    assert(state2.numberOfPendingRequests > 0)

    val state3 = exchangeSingleNode(state2, scheduler, prov).right.value
    // only after processing third result request is finalized as code and storage of account has been retrieved
    assert(state3.numberOfPendingRequests == 0)
    scheduler.persistBatch(state3, 1)
    // 1 leaf node + 1 code + 1 storage
    assert(schedulerDb.dataSource.storage.size == 3)
  }

  it should "sync with mptTrie with 2 accounts with different code and storage" in new TestSetup {
    val prov = getTrieProvider
    // root is branch with 2 leaf nodes
    val worldHash = prov.buildWorld(
      Seq(
        MptNodeData(Address(1), Some(ByteString(1, 2, 3)), Seq((1, 1)), 20),
        MptNodeData(Address(2), Some(ByteString(1, 2, 3, 4)), Seq((2, 2)), 20)
      )
    )
    val (scheduler, schedulerBlockchain, schedulerDb) = buildScheduler()
    val initState = scheduler.initState(worldHash).get
    val state1 = exchangeSingleNode(initState, scheduler, prov).right.value
    // 1 non finalized request for branch node + 2 non finalized request for leaf nodes
    assert(state1.numberOfPendingRequests == 3)
    assert(schedulerDb.dataSource.storage.isEmpty)

    val state2 = exchangeSingleNode(state1, scheduler, prov).right.value
    // 1 non finalized request for branch node + 2 non finalized requests for leaf nodes + 2 non finalized requests for code and
    // storage
    assert(state2.numberOfPendingRequests == 5)

    val state3 = exchangeSingleNode(state2, scheduler, prov).right.value
    val state4 = exchangeSingleNode(state3, scheduler, prov).right.value
    val state5 = scheduler.persistBatch(state4, 1)
    // finalized leaf node i.e state node + storage node + code
    assert(schedulerDb.dataSource.storage.size == 3)

    // 1 non finalized request for branch node + 1 non finalized request for leaf node
    assert(state5.numberOfPendingRequests == 2)

    val state6 = exchangeSingleNode(state5, scheduler, prov).right.value
    // 1 non finalized request for branch node + 1 non finalized request for leaf node + 2 non finalized request for code and storage
    assert(state6.numberOfPendingRequests == 4)

    val state7 = exchangeSingleNode(state6, scheduler, prov).right.value
    val state8 = exchangeSingleNode(state7, scheduler, prov).right.value
    // received code and storage finalized remaining leaf node, and branch node
    assert(state8.numberOfPendingRequests == 0)
    val state9 = scheduler.persistBatch(state8, 1)
    // 1 branch node + 2 leaf nodes + 4 code and storage data
    assert(state9.numberOfPendingRequests == 0)
    assert(schedulerDb.dataSource.storage.size == 7)
  }

  it should "should not request already known code or storage" in new TestSetup {
    val prov = getTrieProvider
    // root is branch with 2 leaf nodes, two different account with same code and same storage
    val worldHash = prov.buildWorld(
      Seq(
        MptNodeData(Address(1), Some(ByteString(1, 2, 3)), Seq((1, 1)), 20),
        MptNodeData(Address(2), Some(ByteString(1, 2, 3)), Seq((1, 1)), 20)
      )
    )
    val (scheduler, schedulerBlockchain, schedulerDb) = buildScheduler()
    val initState = scheduler.initState(worldHash).get
    val state1 = exchangeSingleNode(initState, scheduler, prov).right.value
    // 1 non finalized request for branch node + 2 non finalized request for leaf nodes
    assert(state1.numberOfPendingRequests == 3)
    val (allMissingNodes1, state2) = scheduler.getAllMissingNodes(state1)
    assert(allMissingNodes1.size == 2)
    val allMissingNodes1Response = prov.getNodes(allMissingNodes1)
    val state3 = scheduler.processResponses(state2, allMissingNodes1Response).right.value
    val (allMissingNodes2, state4) = scheduler.getAllMissingNodes(state3)
    assert(allMissingNodes2.size == 2)
    val allMissingNodes2Response = prov.getNodes(allMissingNodes2)
    val state5 = scheduler.processResponses(state4, allMissingNodes2Response).right.value
    val remaingNodes = state5.numberOfPendingRequests
    assert(remaingNodes == 0)
    val state6 = scheduler.persistBatch(state5, 1)
    // 1 branch node + 2 leaf node + 1 code + 1 storage (code and storage are shared by 2 leaf nodes)
    assert(schedulerDb.dataSource.storage.size == 5)
  }

  it should "should return error when processing unrequested response" in new TestSetup {
    val prov = getTrieProvider
    // root is branch with 2 leaf nodes, two different account with same code and same storage
    val worldHash = prov.buildWorld(
      Seq(
        MptNodeData(Address(1), Some(ByteString(1, 2, 3)), Seq((1, 1)), 20),
        MptNodeData(Address(2), Some(ByteString(1, 2, 3)), Seq((1, 1)), 20)
      )
    )
    val (scheduler, schedulerBlockchain, schedulerDb) = buildScheduler()
    val initState = scheduler.initState(worldHash).get
    val (firstMissing, state1) = scheduler.getMissingNodes(initState, 1)
    val result1 = scheduler.processResponse(state1, SyncResponse(ByteString(1), ByteString(2)))
    assert(result1.isLeft)
    assert(result1.left.value == NotRequestedItem)
  }

  it should "should return error when processing already processed response" in new TestSetup {
    val prov = getTrieProvider
    // root is branch with 2 leaf nodes, two different account with same code and same storage
    val worldHash = prov.buildWorld(
      Seq(
        MptNodeData(Address(1), Some(ByteString(1, 2, 3)), Seq((1, 1)), 20),
        MptNodeData(Address(2), Some(ByteString(1, 2, 3)), Seq((1, 1)), 20)
      )
    )
    val (scheduler, schedulerBlockchain, schedulerDb) = buildScheduler()
    val initState = scheduler.initState(worldHash).get
    val (firstMissing, state1) = scheduler.getMissingNodes(initState, 1)
    val firstMissingResponse = prov.getNodes(firstMissing)
    val result1 = scheduler.processResponse(state1, firstMissingResponse.head)
    assert(result1.isRight)
    val stateAfterReceived = result1.right.value
    val result2 = scheduler.processResponse(stateAfterReceived, firstMissingResponse.head)
    assert(result2.isLeft)
    assert(result2.left.value == AlreadyProcessedItem)
  }

  it should "should return critical error when node is malformed" in new TestSetup {
    val prov = getTrieProvider
    // root is branch with 2 leaf nodes, two different account with same code and same storage
    val worldHash = prov.buildWorld(
      Seq(
        MptNodeData(Address(1), Some(ByteString(1, 2, 3)), Seq((1, 1)), 20),
        MptNodeData(Address(2), Some(ByteString(1, 2, 3)), Seq((1, 1)), 20)
      )
    )
    val (scheduler, schedulerBlockchain, schedulerDb) = buildScheduler()
    val initState = scheduler.initState(worldHash).get
    val (firstMissing, state1) = scheduler.getMissingNodes(initState, 1)
    val firstMissingResponse = prov.getNodes(firstMissing)
    val result1 = scheduler.processResponse(state1, firstMissingResponse.head.copy(data = ByteString(1, 2, 3)))
    assert(result1.isLeft)
    assert(result1.left.value == CannotDecodeMptNode)
  }

  // Long running test generating random mpt tries and checking that scheduler is able to correctly
  // traverse them
  it should "sync whole trie when receiving all nodes from remote side" in new TestSetup {
    forAll(genMultipleNodeData(5000)) { nodeData =>
      val prov = getTrieProvider
      val worldHash = prov.buildWorld(nodeData)
      val (scheduler, schedulerBlockchain, schedulerDb) = buildScheduler()
      val header = Fixtures.Blocks.ValidBlock.header.copy(stateRoot = worldHash, number = 1)
      schedulerBlockchain.storeBlockHeader(header).commit()
      var state = scheduler.initState(worldHash).get
      while (state.activeRequest.nonEmpty) {
        val (allMissingNodes1, state2) = scheduler.getAllMissingNodes(state)
        val allMissingNodes1Response = prov.getNodes(allMissingNodes1)
        val state3 = scheduler.processResponses(state2, allMissingNodes1Response).right.value
        state = state3
      }
      assert(state.memBatch.nonEmpty)
      val finalState = scheduler.persistBatch(state, 1)
      assert(finalState.memBatch.isEmpty)
      assert(finalState.activeRequest.isEmpty)
      assert(finalState.queue.isEmpty)
      assert(checkAllDataExists(nodeData, schedulerBlockchain, 1))
    }
  }

  trait TestSetup extends EphemBlockchainTestSetup {

    sealed trait LocalPruningConfigBuilder extends PruningConfigBuilder {
      override lazy val pruningMode: PruningMode = ArchivePruning
    }

    class TrieProvider(bl: Blockchain) {
      def getNodes(hashes: List[ByteString]) = {
        hashes.map { hash =>
          val maybeResult = bl.getMptNodeByHash(hash) match {
            case Some(value) => Some(ByteString(value.encode))
            case None => bl.getEvmCodeByHash(hash)
          }
          maybeResult match {
            case Some(result) => SyncResponse(hash, result)
            case None => fail()
          }
        }
      }

      def buildWorld(accountData: Seq[MptNodeData]): ByteString = {
        val init: InMemoryWorldStateProxy = bl.getWorldStateProxy(
          blockNumber = 1,
          accountStartNonce = blockchainConfig.accountStartNonce,
          stateRootHash = None,
          noEmptyAccounts = true,
          ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
        ).asInstanceOf[InMemoryWorldStateProxy]


        val modifiedWorld = accountData.foldLeft(init) { case (world, data) =>
          val storage = world.getStorage(data.accountAddress)
          val modifiedStorage = data.accountStorage.foldLeft(storage) { case (s, v) =>
            s.store(v._1, v._2)
          }
          val code = world.getCode(data.accountAddress)
          val worldWithAccAndStorage = world
            .saveAccount(data.accountAddress, Account.empty().copy(balance = data.accountBalance))
            .saveStorage(data.accountAddress, modifiedStorage)

          val finalWorld =
            if (data.accountCode.isDefined)
              worldWithAccAndStorage.saveCode(data.accountAddress, data.accountCode.get)
            else
              worldWithAccAndStorage
          finalWorld
        }

        val persisted = InMemoryWorldStateProxy.persistState(modifiedWorld)
        persisted.stateRootHash
      }
    }

    def getTrieProvider: TrieProvider = {
      val freshStorage = new EphemDataSourceComponent with LocalPruningConfigBuilder with Storages.DefaultStorages
      val freshBlockchain = BlockchainImpl(freshStorage.storages)
      new TrieProvider(freshBlockchain)
    }

    def buildScheduler() = {
      val freshStorage = new EphemDataSourceComponent with LocalPruningConfigBuilder with Storages.DefaultStorages
      val freshBlockchain = BlockchainImpl(freshStorage.storages)
      (SyncStateScheduler(freshBlockchain), freshBlockchain, freshStorage)
    }

    def exchangeSingleNode(initState: SchedulerState,
                           scheduler: SyncStateScheduler,
                           provider: TrieProvider): Either[SyncStateScheduler.ResponseProcessingError, SchedulerState] = {
      val (missingNodes, newState) = scheduler.getMissingNodes(initState, 1)
      val providedResponse = provider.getNodes(missingNodes)
      scheduler.processResponses(newState, providedResponse)
    }

  }

}

object SyncSchedulerSpec extends ObjectGenerators {

  final case class MptNodeData(accountAddress: Address,
                               accountCode: Option[ByteString],
                               accountStorage: Seq[(BigInt, BigInt)],
                               accountBalance: Int)

  def createNodeDataStartingFrom(initialNumber: Int, lastNumber: Int, storageOffset: Int) = {
    (initialNumber until lastNumber).map { i =>
      val address = Address(i)
      val codeBytes = ByteString(BigInt(i).toByteArray)
      val storage = (initialNumber until initialNumber + storageOffset).map(s => (BigInt(s), BigInt(s)))
      val balance = i
      MptNodeData(address, Some(codeBytes), storage, balance)
    }
  }

  def genMptNodeData: Gen[MptNodeData] = for {
    receivingAddress <- byteArrayOfNItemsGen(20).map(Address(_))
    code <- byteStringOfLengthNGen(10)
    storageSize <- intGen(1, 100)
    storage <- Gen.listOfN(storageSize, intGen(1, 5000))
    storageAsBigInts = storage.distinct.map(s => (BigInt(s), BigInt(s)))
    value <- intGen(0, 2000)
  } yield MptNodeData(receivingAddress, Some(code), storageAsBigInts, value)

  def genMultipleNodeData(max: Int): Gen[List[MptNodeData]] = for {
    n <- intGen(1, max)
    list <- Gen.listOfN(n, genMptNodeData)
  } yield list

  def checkAllDataExists(nodeData: List[MptNodeData], bl: Blockchain, blNumber: BigInt): Boolean = {
    def go(remaining: List[MptNodeData]): Boolean = {
      if (remaining.isEmpty) {
        true
      } else {
        val dataToCheck = remaining.head
        val address = bl.getAccount(dataToCheck.accountAddress, blNumber)
        val code = address.flatMap(a => bl.getEvmCodeByHash(a.codeHash))

        val storageCorrect = dataToCheck.accountStorage.forall { case (key, value) =>
          val stored = bl.getAccountStorageAt(address.get.storageRoot, key, ethCompatibleStorage = true)
          ByteUtils.toBigInt(stored) == value
        }

        if (address.isDefined && code.isDefined && storageCorrect) {
          go(remaining.tail)
        } else {
          false
        }
      }
    }

    go(nodeData)
  }

}