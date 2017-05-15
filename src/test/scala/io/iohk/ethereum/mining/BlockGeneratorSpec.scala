package io.iohk.ethereum.mining

import akka.util.ByteString
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain.{Address, Block, BlockchainImpl}
import io.iohk.ethereum.ledger.{BlockPreparationError, LedgerImpl}
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm.UInt256
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class BlockGeneratorSpec extends FlatSpec with Matchers with PropertyChecks with Logger {

  "BlockGenerator" should "generate correct block with empty transactions" in new Envirnoment {
    val result: Either[BlockPreparationError, Block] = blockGenerator.generateBlockForMining(1, Nil, Nil, Address(testAddress))
    result shouldBe a[Right[_, Block]]

    val minedNonce = ByteString(Hex.decode("4930c4d7949e3678"))
    val minedMixHash = ByteString(Hex.decode("97779cb0c9dc1c23629c60387055ac738824199bd3c02e6e313042482eebaac5"))
    val miningTimestamp = 1494604913

    //state root problem
    val fulBlock: Either[BlockPreparationError, Block] = result.right
      .map(b => b.copy(header = b.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)))
    fulBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.header, blockchain) shouldBe Right(b.header))
  }

  trait Envirnoment {

    val testAddress = 42

    val blockchainStorages = new SharedEphemDataSources with Storages.DefaultStorages
    val blockchainConfig = new BlockchainConfig {
      override val frontierBlockNumber: BigInt = 0
      override val homesteadBlockNumber: BigInt = 1150000
      override val difficultyBombPauseBlockNumber: BigInt = 3000000
      override val difficultyBombContinueBlockNumber: BigInt = 5000000
      override val chainId: Byte = 0x3d.toByte
      override val blockReward: UInt256 = UInt256(BigInt("5000000000000000000"))
      override val customGenesisFileOpt: Option[String] = Some("test-genesis.json")

      // unused
      override val daoForkBlockNumber: BigInt = Long.MaxValue
      override val eip160BlockNumber: BigInt = Long.MaxValue
      override val eip150BlockNumber: BigInt = Long.MaxValue
      override val daoForkBlockHash: ByteString = ByteString("unused")
      override val daoForkBlockTotalDifficulty: BigInt = 0
    }
    val ledger = new LedgerImpl(new Mocks.MockVM(), blockchainConfig)

    val validators = new Validators {
      val blockValidator: BlockValidator = BlockValidator
      val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidatorImpl(blockchainConfig)
      val ommersValidator: OmmersValidator = new OmmersValidatorImpl(blockchainConfig)
      val signedTransactionValidator: SignedTransactionValidator = new SignedTransactionValidatorImpl(blockchainConfig)
    }

    val blockchain = BlockchainImpl(blockchainStorages.storages)

    val genesisDataLoader = new GenesisDataLoader(blockchainStorages.ephemDataSource, blockchain, blockchainConfig)
    genesisDataLoader.loadGenesisData()

    val blockGenerator = new BlockGenerator(blockchainStorages.storages, blockchainConfig, ledger, validators)
  }

}
