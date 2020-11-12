package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.HefPostEcip1097
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.{Fixtures, ObjectGenerators}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BlockchainSpec extends AnyFlatSpec with Matchers {

  val checkpoint = ObjectGenerators.fakeCheckpointGen(2, 5).sample.get
  val checkpointBlockGenerator = new CheckpointBlockGenerator

  "Blockchain" should "be able to store a block and return it if queried by hash" in new EphemBlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchain.storeBlock(validBlock).commit()
    val block = blockchain.getBlockByHash(validBlock.header.hash)
    assert(block.isDefined)
    assert(validBlock == block.get)
    val blockHeader = blockchain.getBlockHeaderByHash(validBlock.header.hash)
    assert(blockHeader.isDefined)
    assert(validBlock.header == blockHeader.get)
    val blockBody = blockchain.getBlockBodyByHash(validBlock.header.hash)
    assert(blockBody.isDefined)
    assert(validBlock.body == blockBody.get)
  }

  it should "be able to store a block and retrieve it by number" in new EphemBlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchain.storeBlock(validBlock).commit()
    val block = blockchain.getBlockByNumber(validBlock.header.number)
    assert(block.isDefined)
    assert(validBlock == block.get)
  }

  it should "be able to query a stored blockHeader by it's number" in new EphemBlockchainTestSetup {
    val validHeader = Fixtures.Blocks.ValidBlock.header
    blockchain.storeBlockHeader(validHeader).commit()
    val header = blockchain.getBlockHeaderByNumber(validHeader.number)
    assert(header.isDefined)
    assert(validHeader == header.get)
  }

  it should "not return a value if not stored" in new EphemBlockchainTestSetup {
    assert(blockchain.getBlockByNumber(Fixtures.Blocks.ValidBlock.header.number).isEmpty)
    assert(blockchain.getBlockByHash(Fixtures.Blocks.ValidBlock.header.hash).isEmpty)
  }

  it should "be able to store a block with checkpoint and retrieve it and checkpoint" in new EphemBlockchainTestSetup {
    val parent = Fixtures.Blocks.Genesis.block
    blockchain.storeBlock(parent)

    val validBlock = new CheckpointBlockGenerator().generate(parent, checkpoint)

    blockchain.save(validBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)

    val retrievedBlock = blockchain.getBlockByHash(validBlock.header.hash)
    assert(retrievedBlock.isDefined)
    assert(validBlock == retrievedBlock.get)

    blockchain.getLatestCheckpointBlockNumber should ===(validBlock.number)
    blockchain.getBestBlockNumber should ===(validBlock.number)
  }

  it should "be able to rollback block with checkpoint and store the previous existed checkpoint" in new EphemBlockchainTestSetup {
    val genesis = Fixtures.Blocks.Genesis.block
    blockchain.storeBlock(genesis)

    def nextBlock(parent: Block, body: BlockBody = BlockBody.empty): Block =
      Block(
        header = parent.header.copy(
          number = parent.number + 1,
          parentHash = parent.hash,
          extraFields = HefPostEcip1097(false, None)
        ),
        body = body
      )

    val firstBlock = checkpointBlockGenerator.generate(genesis, checkpoint) // Older checkpoint
    val secondBlock = nextBlock(firstBlock)
    val thirdBlock = checkpointBlockGenerator.generate(secondBlock, checkpoint)

    blockchain.save(firstBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)
    blockchain.save(secondBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)
    blockchain.save(thirdBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)

    blockchain.removeBlock(thirdBlock.hash, withState = true)

    blockchain.getLatestCheckpointBlockNumber should ===(firstBlock.number)
    blockchain.getBestBlockNumber should ===(secondBlock.number)
  }

  it should "be able to rollback block with last checkpoint in the chain" in new EphemBlockchainTestSetup {
    val genesis = Fixtures.Blocks.Genesis.block
    blockchain.storeBlock(genesis)

    val validBlock = checkpointBlockGenerator.generate(genesis, checkpoint)

    blockchain.save(validBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)

    blockchain.removeBlock(validBlock.hash, withState = true)

    blockchain.getLatestCheckpointBlockNumber should ===(genesis.number)
    blockchain.getBestBlockNumber should ===(genesis.number)
  }

  it should "return an account given an address and a block number" in new EphemBlockchainTestSetup {
    val address = Address(42)
    val account = Account.empty(UInt256(7))

    val validHeader = Fixtures.Blocks.ValidBlock.header

    val stateStorage = StateStorage.createTestStateStorage(EphemDataSource())._1
    val emptyMpt = MerklePatriciaTrie[Address, Account](
      storagesInstance.storages.stateStorage.getBackingStorage(0)
    )
    val mptWithAcc = emptyMpt.put(address, account)
    val headerWithAcc = validHeader.copy(stateRoot = ByteString(mptWithAcc.getRootHash))

    blockchain.storeBlockHeader(headerWithAcc).commit()

    val retrievedAccount = blockchain.getAccount(address, headerWithAcc.number)
    retrievedAccount shouldEqual Some(account)
  }

  // TODO PP add test for getAccountStorageAt

  // TODO PP add test for getAccountStorageProofAt
}
