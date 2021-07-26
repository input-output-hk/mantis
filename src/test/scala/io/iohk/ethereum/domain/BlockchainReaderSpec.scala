package io.iohk.ethereum.domain

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup

class BlockchainReaderSpec extends AnyFlatSpec with Matchers {

  "BlockchainReader" should "be able to get the best block after it was stored by BlockchainWriter" in new EphemBlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block

    blockchainWriter.save(validBlock, Nil, ChainWeight.zero, true)

    blockchainReader.getBestBlock() shouldBe Some(validBlock)
  }

}
