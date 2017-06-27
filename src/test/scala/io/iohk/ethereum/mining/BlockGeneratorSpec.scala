package io.iohk.ethereum.mining

import java.time.Instant

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.{BlockPreparationError, LedgerImpl}
import io.iohk.ethereum.utils.{BlockchainConfig, Logger, MiningConfig}
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm.{UInt256, VM}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.SignedTransaction.FirstByteOfAddress
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

class BlockGeneratorSpec extends FlatSpec with Matchers with PropertyChecks with Logger {

  "BlockGenerator" should "generate correct block with empty transactions" in new TestSetup {
    val result: Either[BlockPreparationError, PendingBlock] = blockGenerator.generateBlockForMining(1, Nil, Nil, Address(testAddress))
    result shouldBe a[Right[_, Block]]

    //mined with etc-client + ethminer
    val minedNonce = ByteString(Hex.decode("ce1b500070aeec4f"))
    val minedMixHash = ByteString(Hex.decode("40d9bd2064406d7f22390766d6fe5eccd2a67aa89bf218e99df35b2dbb425fb1"))
    val miningTimestamp = 1494604913

    val fullBlock: Either[BlockPreparationError, Block] = result.right
      .map(pb => pb.block.copy(header = pb.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)))
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.header, blockchain) shouldBe Right(b.header))
    fullBlock.right.foreach(b => ledger.executeBlock(b, blockchainStorages.storages, validators) shouldBe a[Right[_, Seq[Receipt]]])
  }

  it should "generate correct block with transactions" in new TestSetup {
    val result: Either[BlockPreparationError, PendingBlock] = blockGenerator.generateBlockForMining(1, Seq(signedTransaction), Nil, Address(testAddress))
    result shouldBe a[Right[_, Block]]

    //mined with etc-client + ethminer
    val minedNonce = ByteString(Hex.decode("5e8d5c12cea7e0c7"))
    val minedMixHash = ByteString(Hex.decode("9247b81258f97159f987a5f4f9e94df1d95e10eeabff2836020eafb27a8228b0"))
    val miningTimestamp = 1494604913

    val fullBlock: Either[BlockPreparationError, Block] = result.right
      .map(pb => pb.block.copy(header = pb.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)))
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.header, blockchain) shouldBe Right(b.header))
    fullBlock.right.foreach(b => ledger.executeBlock(b, blockchainStorages.storages, validators) shouldBe a[Right[_, Seq[Receipt]]])
  }

  it should "filter out failing transactions" in new TestSetup {
    val result: Either[BlockPreparationError, PendingBlock] =
      blockGenerator.generateBlockForMining(1, Seq(signedTransaction, duplicatedSignedTransaction), Nil, Address(testAddress))
    result shouldBe a[Right[_, Block]]

    //mined with etc-client + ethminer
    val minedNonce = ByteString(Hex.decode("5e8d5c12cea7e0c7"))
    val minedMixHash = ByteString(Hex.decode("9247b81258f97159f987a5f4f9e94df1d95e10eeabff2836020eafb27a8228b0"))
    val miningTimestamp = 1494604913

    val fullBlock: Either[BlockPreparationError, Block] = result.right
      .map(pb => pb.block.copy(header = pb.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)))
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.header, blockchain) shouldBe Right(b.header))
    fullBlock.right.foreach(b => ledger.executeBlock(b, blockchainStorages.storages, validators) shouldBe a[Right[_, Seq[Receipt]]])
  }

  it should "filter out transactions exceeding block gas limit and include correct transactions" in new TestSetup {
    val txWitGasTooBigGasLimit = SignedTransaction.sign(
      transaction.copy(
        gasLimit = BigInt(2).pow(100000),
        nonce = signedTransaction.tx.nonce - 1),
      keyPair, Some(0x3d.toByte))

    val result: Either[BlockPreparationError, PendingBlock] =
      blockGenerator.generateBlockForMining(1, Seq(txWitGasTooBigGasLimit, signedTransaction, duplicatedSignedTransaction), Nil, Address(testAddress))
    result shouldBe a[Right[_, Block]]

    //mined with etc-client + ethminer
    val minedNonce = ByteString(Hex.decode("5e8d5c12cea7e0c7"))
    val minedMixHash = ByteString(Hex.decode("9247b81258f97159f987a5f4f9e94df1d95e10eeabff2836020eafb27a8228b0"))
    val miningTimestamp = 1494604913

    val fullBlock: Either[BlockPreparationError, Block] = result.right
      .map(pb => pb.block.copy(header = pb.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)))
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.header, blockchain) shouldBe Right(b.header))
    fullBlock.right.foreach(b => ledger.executeBlock(b, blockchainStorages.storages, validators) shouldBe a[Right[_, Seq[Receipt]]])
  }

  trait TestSetup {

    val testAddress = 42
    val privateKey = BigInt(1, Hex.decode("f3202185c84325302d43887e90a2e23e7bc058d0450bb58ef2f7585765d7d48b"))
    val keyPair: AsymmetricCipherKeyPair = keyPairFromPrvKey(privateKey)
    val pubKey: Array[Byte] = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail
    val address = Address(crypto.kec256(pubKey).drop(FirstByteOfAddress))

    val txGasLimit = 21000
    val txTransfer = 9000
    val transaction = Transaction(
      nonce = 0,
      gasPrice = 1,
      gasLimit = txGasLimit,
      receivingAddress = Address(testAddress),
      value = txTransfer,
      payload = ByteString.empty)
    val signedTransaction: SignedTransaction = SignedTransaction.sign(transaction, keyPair, Some(0x3d.toByte))
    val duplicatedSignedTransaction: SignedTransaction = SignedTransaction.sign(transaction.copy(gasLimit = 2), keyPair, Some(0x3d.toByte))

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
    val ledger = new LedgerImpl(VM, blockchainConfig)

    val validators = new Validators {
      val blockValidator: BlockValidator = BlockValidator
      val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidatorImpl(blockchainConfig)
      val ommersValidator: OmmersValidator = new OmmersValidatorImpl(blockchainConfig)
      val signedTransactionValidator: SignedTransactionValidator = new SignedTransactionValidatorImpl(blockchainConfig)
    }

    val blockchain = BlockchainImpl(blockchainStorages.storages)

    val genesisDataLoader = new GenesisDataLoader(blockchainStorages.ephemDataSource, blockchain, blockchainConfig)
    genesisDataLoader.loadGenesisData()

    val miningConfig = new MiningConfig {
      override val coinbase: Address = Address(42)
      override val blockCacheSize: Int = 30
      override val ommersPoolSize: Int = 30
      override val txPoolSize: Int = 30
      override val poolingServicesTimeout: FiniteDuration = 3.seconds
    }

    val blockTimestampProvider = new FakeBlockTimestampProvider

    val blockGenerator = new BlockGenerator(blockchainStorages.storages, blockchainConfig, miningConfig, ledger, validators, blockTimestampProvider)
  }
}

class FakeBlockTimestampProvider extends BlockTimestampProvider {
  private var timestamp = Instant.now.getEpochSecond

  def advance(seconds: Long): Unit = timestamp += seconds

  override def getEpochSecond: Long = timestamp
}
