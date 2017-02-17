package io.iohk.ethereum.blockchain

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages, StoragesComp}
import org.scalatest.{FlatSpec, Matchers}

class BlockchainSpec extends FlatSpec with Matchers {

  "Blockchain" should "be able to store a block and return if if queried by hash" in new BlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchain.save(validBlock)
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

  "Blockchain" should "be able to store a block and retrieve it by number" in new BlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchain.save(validBlock)
    val block = blockchain.getBlockByNumber(validBlock.header.number)
    assert(block.isDefined)
    assert(validBlock == block.get)
  }

  "Blockchain" should "not return a value if not stored" in new BlockchainTestSetup {
    assert(blockchain.getBlockByNumber(Fixtures.Blocks.ValidBlock.header.number).isEmpty)
    assert(blockchain.getBlockByHash(Fixtures.Blocks.ValidBlock.header.hash).isEmpty)
  }

  trait BlockchainTestSetup {
    private val storagesImpl = new Storages.DefaultStorages with SharedEphemDataSources
    private val blockchainComp: BlockchainComp = new BlockchainCompImpl {
      override val storagesComp: StoragesComp = storagesImpl
    }
    val blockchain: Blockchain = blockchainComp.blockchain
  }

}
