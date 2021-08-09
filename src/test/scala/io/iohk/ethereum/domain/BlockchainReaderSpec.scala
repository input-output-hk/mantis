package io.iohk.ethereum.domain

import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.NewBlock
import io.iohk.ethereum.security.SecureRandomBuilder

class BlockchainReaderSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks with SecureRandomBuilder {

  val chainId: Option[Byte] = Hex.decode("3d").headOption

  "BlockchainReader" should "be able to get the best block after it was stored by BlockchainWriter" in new EphemBlockchainTestSetup {
    forAll(ObjectGenerators.newBlockGen(secureRandom, chainId)) { case NewBlock(block, weight) =>
      blockchainWriter.save(block, Nil, ChainWeight(0, weight), true)

      blockchainReader.getBestBlock() shouldBe Some(block)
    }
  }

}
