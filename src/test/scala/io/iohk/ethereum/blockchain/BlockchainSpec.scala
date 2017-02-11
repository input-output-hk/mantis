package io.iohk.ethereum.blockchain

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages, StoragesComp}
import org.scalatest.{FlatSpec, Matchers}

class BlockchainSpec extends FlatSpec with Matchers {

  def withBlockchain(testCode: Blockchain => Any): Unit = {
    val storagesImpl = new Storages.DefaultStorages with SharedEphemDataSources
    val blockchainComp: BlockchainComp = new BlockchainCompImpl {
      override val storagesComp: StoragesComp = storagesImpl
    }
    testCode(blockchainComp.blockchain)
  }

  "Blockchain" should "be able to store a block and return if if queried by hash" in {
    withBlockchain { blockchain =>
      val validBlock = Fixtures.Blocks.ValidBlock.block
      blockchain.save(validBlock)
      val block = blockchain.getBlockByHash(validBlock.header.hash)
      assert(block.isDefined)
      assert(validBlock == block.get)
    }
  }

  "Blockchain" should "be able to store a block and retrieve it by number" in {
    withBlockchain { blockchain =>
      val validBlock = Fixtures.Blocks.ValidBlock.block
      blockchain.save(validBlock)
      val block = blockchain.getBlockByNumber(validBlock.header.number)
      assert(block.isDefined)
      assert(validBlock == block.get)
    }
  }

  "Blockchain" should "not return a value if not stored" in {
    withBlockchain { blockchain =>
      assert(blockchain.getBlockByNumber(Fixtures.Blocks.ValidBlock.header.number).isEmpty)
      assert(blockchain.getBlockByHash(Fixtures.Blocks.ValidBlock.header.hash).isEmpty)
    }
  }

}
