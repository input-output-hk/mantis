package io.iohk.ethereum.domain

import akka.util.ByteString

import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.BlockHelpers
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.ObjectGenerators._
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.HefPostEcip1097
import io.iohk.ethereum.mpt.HashNode
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.proof.MptProofVerifier
import io.iohk.ethereum.proof.ProofVerifyResult.ValidProof

class BlockchainSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  val checkpoint: Checkpoint = ObjectGenerators.fakeCheckpointGen(2, 5).sample.get
  val checkpointBlockGenerator = new CheckpointBlockGenerator

  "Blockchain" should "be able to store a block and return it if queried by hash" in new EphemBlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchainWriter.storeBlock(validBlock).commit()
    val block = blockchainReader.getBlockByHash(validBlock.header.hash)
    block.isDefined should ===(true)
    validBlock should ===(block.get)
    val blockHeader = blockchainReader.getBlockHeaderByHash(validBlock.header.hash)
    blockHeader.isDefined should ===(true)
    validBlock.header should ===(blockHeader.get)
    val blockBody = blockchainReader.getBlockBodyByHash(validBlock.header.hash)
    blockBody.isDefined should ===(true)
    validBlock.body should ===(blockBody.get)
  }

  it should "be able to store a block and retrieve it by number" in new EphemBlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchainWriter.storeBlock(validBlock).commit()
    blockchainWriter.saveBestKnownBlocks(validBlock.hash, validBlock.number)
    val block = blockchainReader.getBlockByNumber(blockchainReader.getBestBranch(), validBlock.header.number)
    block.isDefined should ===(true)
    validBlock should ===(block.get)
  }

  it should "be able to do strict check of block existence in the chain" in new EphemBlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchainWriter.save(
      validBlock.copy(header = validBlock.header.copy(number = validBlock.number - 1)),
      Seq.empty,
      ChainWeight(100, 100),
      saveAsBestBlock = true
    )
    blockchainWriter.save(validBlock, Seq.empty, ChainWeight(100, 100), saveAsBestBlock = true)
    blockchainReader.isInChain(blockchainReader.getBestBranch(), validBlock.hash) should ===(true)
    // simulation of node restart
    blockchainWriter.saveBestKnownBlocks(validBlock.header.parentHash, validBlock.header.number - 1)
    blockchainReader.isInChain(blockchainReader.getBestBranch(), validBlock.hash) should ===(false)
  }

  it should "be able to query a stored blockHeader by it's number" in new EphemBlockchainTestSetup {
    val validHeader = Fixtures.Blocks.ValidBlock.header
    blockchainWriter.storeBlockHeader(validHeader).commit()
    val header = blockchainReader.getBlockHeaderByNumber(validHeader.number)
    header.isDefined should ===(true)
    validHeader should ===(header.get)
  }

  it should "not return a value if not stored" in new EphemBlockchainTestSetup {
    blockchainReader
      .getBlockByNumber(blockchainReader.getBestBranch(), Fixtures.Blocks.ValidBlock.header.number) shouldBe None
    blockchainReader.getBlockByHash(Fixtures.Blocks.ValidBlock.header.hash) shouldBe None
  }

  it should "be able to store a block with checkpoint and retrieve it and checkpoint" in new EphemBlockchainTestSetup {
    val parent = Fixtures.Blocks.Genesis.block
    blockchainWriter.storeBlock(parent)

    val validBlock = new CheckpointBlockGenerator().generate(parent, checkpoint)

    blockchainWriter.save(validBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)

    val retrievedBlock = blockchainReader.getBlockByHash(validBlock.header.hash)
    retrievedBlock.isDefined should ===(true)
    validBlock should ===(retrievedBlock.get)

    blockchainReader.getLatestCheckpointBlockNumber() should ===(validBlock.number)
    blockchainReader.getBestBlockNumber() should ===(validBlock.number)
  }

  it should "be able to rollback block with checkpoint and store the previous existed checkpoint" in new EphemBlockchainTestSetup {
    val genesis = Fixtures.Blocks.Genesis.block
    blockchainWriter.storeBlock(genesis)

    def nextBlock(parent: Block, body: BlockBody = BlockBody.empty): Block =
      Block(
        header = parent.header.copy(
          number = parent.number + 1,
          parentHash = parent.hash,
          extraFields = HefPostEcip1097(None)
        ),
        body = body
      )

    val firstBlock = checkpointBlockGenerator.generate(genesis, checkpoint) // Older checkpoint
    val secondBlock = nextBlock(firstBlock)
    val thirdBlock = checkpointBlockGenerator.generate(secondBlock, checkpoint)

    blockchainWriter.save(firstBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)
    blockchainWriter.save(secondBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)
    blockchainWriter.save(thirdBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)

    blockchain.removeBlock(thirdBlock.hash)

    blockchainReader.getLatestCheckpointBlockNumber() should ===(firstBlock.number)
    blockchainReader.getBestBlockNumber() should ===(secondBlock.number)
  }

  it should "be able to rollback block with last checkpoint in the chain" in new EphemBlockchainTestSetup {
    val genesis = Fixtures.Blocks.Genesis.block
    blockchainWriter.storeBlock(genesis)

    val validBlock = checkpointBlockGenerator.generate(genesis, checkpoint)

    blockchainWriter.save(validBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)

    blockchain.removeBlock(validBlock.hash)

    blockchainReader.getLatestCheckpointBlockNumber() should ===(genesis.number)
    blockchainReader.getBestBlockNumber() should ===(genesis.number)
  }

  it should "return an account given an address and a block number" in new EphemBlockchainTestSetup {
    val address = Address(42)
    val account = Account.empty(UInt256(7))

    val validHeader = Fixtures.Blocks.ValidBlock.header

    StateStorage.createTestStateStorage(EphemDataSource())._1
    val emptyMpt = MerklePatriciaTrie[Address, Account](
      storagesInstance.storages.stateStorage.getBackingStorage(0)
    )
    val mptWithAcc = emptyMpt.put(address, account)
    val headerWithAcc = validHeader.copy(stateRoot = ByteString(mptWithAcc.getRootHash))

    blockchainWriter.storeBlockHeader(headerWithAcc).commit()
    blockchainWriter.saveBestKnownBlocks(headerWithAcc.hash, headerWithAcc.number)

    val retrievedAccount = blockchainReader.getAccount(blockchainReader.getBestBranch(), address, headerWithAcc.number)
    retrievedAccount shouldEqual Some(account)
  }

  it should "return correct account proof" in new EphemBlockchainTestSetup {
    val address = Address(42)
    val account = Account.empty(UInt256(7))

    val validHeader = Fixtures.Blocks.ValidBlock.header

    val emptyMpt = MerklePatriciaTrie[Address, Account](
      storagesInstance.storages.stateStorage.getBackingStorage(0)
    )
    val mptWithAcc = emptyMpt.put(address, account)

    val headerWithAcc = validHeader.copy(stateRoot = ByteString(mptWithAcc.getRootHash))

    blockchainWriter.storeBlockHeader(headerWithAcc).commit()
    blockchainWriter.saveBestKnownBlocks(headerWithAcc.hash, headerWithAcc.number)

    //unhappy path
    val wrongAddress = Address(666)
    val retrievedAccountProofWrong =
      blockchainReader.getAccountProof(blockchainReader.getBestBranch(), wrongAddress, headerWithAcc.number)
    //the account doesn't exist, so we can't retrieve it, but we do receive a proof of non-existence with a full path of nodes that we iterated
    retrievedAccountProofWrong.isDefined shouldBe true
    retrievedAccountProofWrong.size shouldBe 1
    mptWithAcc.get(wrongAddress) shouldBe None

    //happy path
    val retrievedAccountProof =
      blockchainReader.getAccountProof(blockchainReader.getBestBranch(), address, headerWithAcc.number)
    retrievedAccountProof.isDefined shouldBe true
    retrievedAccountProof.map { proof =>
      MptProofVerifier.verifyProof(mptWithAcc.getRootHash, address, proof) shouldBe ValidProof
    }
  }

  it should "return proof for non-existent account" in new EphemBlockchainTestSetup {
    val emptyMpt = MerklePatriciaTrie[Address, Account](
      storagesInstance.storages.stateStorage.getBackingStorage(0)
    )
    val mptWithAcc = emptyMpt.put(Address(42), Account.empty(UInt256(7)))

    val headerWithAcc = Fixtures.Blocks.ValidBlock.header.copy(stateRoot = ByteString(mptWithAcc.getRootHash))

    blockchainWriter.storeBlockHeader(headerWithAcc).commit()
    blockchainWriter.saveBestKnownBlocks(headerWithAcc.hash, headerWithAcc.number)

    val wrongAddress = Address(666)
    val retrievedAccountProofWrong =
      blockchainReader.getAccountProof(blockchainReader.getBestBranch(), wrongAddress, headerWithAcc.number)
    //the account doesn't exist, so we can't retrieve it, but we do receive a proof of non-existence with a full path of nodes(root node) that we iterated
    (retrievedAccountProofWrong.getOrElse(Vector.empty).toList match {
      case _ @HashNode(_) :: Nil => true
      case _                     => false
    }) shouldBe true
    mptWithAcc.get(wrongAddress) shouldBe None
  }

  it should "return correct best block number after saving and rolling back blocks" in new TestSetup {
    forAll(intGen(min = 1, max = maxNumberBlocksToImport)) { numberBlocksToImport =>
      val testSetup = newSetup()
      import testSetup._

      // Import blocks
      val blocksToImport = BlockHelpers.generateChain(numberBlocksToImport, Fixtures.Blocks.Genesis.block)

      // Randomly select the block import to persist (empty means no persistence)
      val blockImportToPersist = Gen.option(Gen.oneOf(blocksToImport)).sample.get
      (stubStateStorage
        .onBlockSave(_: BigInt, _: BigInt)(_: () => Unit))
        .when(*, *, *)
        .onCall { (bn, _, persistFn) =>
          if (blockImportToPersist.exists(_.number == bn)) persistFn()
        }

      blocksToImport.foreach { block =>
        blockchainWriterWithStubPersisting.save(block, Nil, ChainWeight.zero, saveAsBestBlock = true)
      }

      blockchainReaderWithStubPersisting.getBestBlockNumber() shouldBe blocksToImport.last.number

      // Rollback blocks
      val numberBlocksToKeep = intGen(0, numberBlocksToImport).sample.get

      val (_, blocksToRollback) = blocksToImport.splitAt(numberBlocksToKeep)

      // Randomly select the block rollback to persist (empty means no persistence)
      val blockRollbackToPersist =
        if (blocksToRollback.isEmpty) None else Gen.option(Gen.oneOf(blocksToRollback)).sample.get
      (stubStateStorage
        .onBlockRollback(_: BigInt, _: BigInt)(_: () => Unit))
        .when(*, *, *)
        .onCall { (bn, _, persistFn) =>
          if (blockRollbackToPersist.exists(_.number == bn)) persistFn()
        }

      blocksToRollback.reverse.foreach { block =>
        blockchainWithStubPersisting.removeBlock(block.hash)
      }

      blockchainReaderWithStubPersisting.getBestBlockNumber() shouldBe numberBlocksToKeep
    }
  }

  trait TestSetup extends MockFactory {
    val maxNumberBlocksToImport: Int = 30

    trait StubPersistingBlockchainSetup {
      def stubStateStorage: StateStorage
      def blockchainStoragesWithStubPersisting: BlockchainStorages
      def blockchainReaderWithStubPersisting: BlockchainReader
      def blockchainWriterWithStubPersisting: BlockchainWriter
      def blockchainWithStubPersisting: BlockchainImpl
    }

    def newSetup(): StubPersistingBlockchainSetup =
      new StubPersistingBlockchainSetup with EphemBlockchainTestSetup {
        override val stubStateStorage = stub[StateStorage]
        override val blockchainStoragesWithStubPersisting = new BlockchainStorages {
          val blockHeadersStorage = storagesInstance.storages.blockHeadersStorage
          val blockBodiesStorage = storagesInstance.storages.blockBodiesStorage
          val blockNumberMappingStorage = storagesInstance.storages.blockNumberMappingStorage
          val receiptStorage = storagesInstance.storages.receiptStorage
          val evmCodeStorage = storagesInstance.storages.evmCodeStorage
          val chainWeightStorage = storagesInstance.storages.chainWeightStorage
          val transactionMappingStorage = storagesInstance.storages.transactionMappingStorage
          val appStateStorage = storagesInstance.storages.appStateStorage
          val stateStorage = stubStateStorage
        }
        override val blockchainReaderWithStubPersisting =
          BlockchainReader(blockchainStoragesWithStubPersisting)
        override val blockchainWriterWithStubPersisting =
          BlockchainWriter(blockchainStoragesWithStubPersisting)
        override val blockchainWithStubPersisting =
          BlockchainImpl(
            blockchainStoragesWithStubPersisting,
            blockchainReaderWithStubPersisting
          )

        blockchainWriterWithStubPersisting.storeBlock(Fixtures.Blocks.Genesis.block)
      }

  }
}
