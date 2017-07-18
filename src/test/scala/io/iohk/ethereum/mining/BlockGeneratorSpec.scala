package io.iohk.ethereum.mining

import java.time.Instant

import akka.util.ByteString
import io.iohk.ethereum.{Timeouts, crypto}
import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.{BlockPreparationError, LedgerImpl}
import io.iohk.ethereum.utils.{BlockchainConfig, Logger, MiningConfig, MonetaryPolicyConfig}
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
    val txWitGasTooBigGasLimit: SignedTransaction = SignedTransaction.sign(
      transaction.copy(
        gasLimit = BigInt(2).pow(100000),
        nonce = signedTransaction.tx.nonce + 1),
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

  it should "generate block before eip155 and filter out chain specific tx" in new TestSetup {
    override lazy val blockchainConfig = new BlockchainConfig {
      override val frontierBlockNumber: BigInt = 0
      override val homesteadBlockNumber: BigInt = 1150000
      override val difficultyBombPauseBlockNumber: BigInt = 3000000
      override val difficultyBombContinueBlockNumber: BigInt = 5000000
      override val eip155BlockNumber: BigInt = Long.MaxValue
      override val chainId: Byte = 0x3d.toByte
      override val customGenesisFileOpt: Option[String] = Some("test-genesis.json")
      override val monetaryPolicyConfig: MonetaryPolicyConfig = MonetaryPolicyConfig(5000000, 0.2, BigInt("5000000000000000000"))

      // unused
      override val daoForkBlockNumber: BigInt = Long.MaxValue
      override val eip160BlockNumber: BigInt = Long.MaxValue
      override val eip150BlockNumber: BigInt = Long.MaxValue
      override val daoForkBlockHash: ByteString = ByteString("unused")
      override val accountStartNonce: UInt256 = UInt256.Zero
    }

    val generalTx = SignedTransaction.sign(transaction, keyPair, None)
    val specificTx = SignedTransaction.sign(transaction.copy(nonce = transaction.nonce + 1), keyPair, Some(0x3d.toByte))

    val result: Either[BlockPreparationError, PendingBlock] =
      blockGenerator.generateBlockForMining(1, Seq(generalTx, specificTx), Nil, Address(testAddress))
    result shouldBe a[Right[_, Block]]

    //mined with etc-client + ethminer
    val minedNonce = ByteString(Hex.decode("48381cb0cd40936a"))
    val minedMixHash = ByteString(Hex.decode("dacd96cf5dbc662fa113c73319fcdc7d6e7053571432345b936fd221c1e18d42"))
    val miningTimestamp = 1499952002

    val fullBlock: Either[BlockPreparationError, Block] = result.right
      .map(pb => pb.block.copy(header = pb.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)))
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.header, blockchain) shouldBe Right(b.header))
    fullBlock.right.foreach(b => ledger.executeBlock(b, blockchainStorages.storages, validators) shouldBe a[Right[_, Seq[Receipt]]])
    fullBlock.right.foreach(b => b.body.transactionList shouldBe Seq(generalTx))
  }

  it should "generate block after eip155 and allow both chain specific and general transactions" in new TestSetup {
    val generalTx: SignedTransaction = SignedTransaction.sign(transaction.copy(nonce = transaction.nonce + 1), keyPair, None)

    val result: Either[BlockPreparationError, PendingBlock] =
      blockGenerator.generateBlockForMining(1, Seq(generalTx, signedTransaction), Nil, Address(testAddress))
    result shouldBe a[Right[_, Block]]

    //mined with etc-client + ethminer
    val minedNonce = ByteString(Hex.decode("39bd50fcbde30b18"))
    val minedMixHash = ByteString(Hex.decode("c77dae7cef6c685896ed6b8026466a2e6338b8bc5f182e2dd7a64cf7da9c7d1b"))
    val miningTimestamp = 1499951223

    val fullBlock: Either[BlockPreparationError, Block] = result.right
      .map(pb => pb.block.copy(header = pb.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)))
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.header, blockchain) shouldBe Right(b.header))
    fullBlock.right.foreach(b => ledger.executeBlock(b, blockchainStorages.storages, validators) shouldBe a[Right[_, Seq[Receipt]]])
    fullBlock.right.foreach(b => b.body.transactionList shouldBe Seq(signedTransaction, generalTx))
  }

  it should "include consecutive transactions from single sender" in new TestSetup {
    val nextTransaction: SignedTransaction = SignedTransaction.sign(transaction.copy(nonce = signedTransaction.tx.nonce + 1), keyPair, Some(0x3d.toByte))

    val result: Either[BlockPreparationError, PendingBlock] =
      blockGenerator.generateBlockForMining(1, Seq(nextTransaction, signedTransaction), Nil, Address(testAddress))
    result shouldBe a[Right[_, Block]]

    //mined with etc-client + ethminer
    val minedNonce = ByteString(Hex.decode("8f88ec20f1be482f"))
    val minedMixHash = ByteString(Hex.decode("247a206abc088487edc1697fcaceb33ad87b55666e438129b7048bb08c8ed88f"))
    val miningTimestamp = 1499721182

    val fullBlock: Either[BlockPreparationError, Block] = result.right
      .map(pb => pb.block.copy(header = pb.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)))
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.header, blockchain) shouldBe Right(b.header))
    fullBlock.right.foreach(b => ledger.executeBlock(b, blockchainStorages.storages, validators) shouldBe a[Right[_, Seq[Receipt]]])
    fullBlock.right.foreach(b => b.body.transactionList shouldBe Seq(signedTransaction, nextTransaction))
  }

  it should "include transaction with higher gas price if nonce is the same" in new TestSetup {
    val txWitSameNonceButLowerGasPrice: SignedTransaction = SignedTransaction.sign(
      transaction.copy(gasPrice = signedTransaction.tx.gasPrice - 1),
      keyPair,
      Some(0x3d.toByte))

    val result: Either[BlockPreparationError, PendingBlock] =
      blockGenerator.generateBlockForMining(1, Seq(txWitSameNonceButLowerGasPrice, signedTransaction), Nil, Address(testAddress))
    result shouldBe a[Right[_, Block]]

    //mined with etc-client + ethminer
    val minedNonce = ByteString(Hex.decode("5e8d5c12cea7e0c7"))
    val minedMixHash = ByteString(Hex.decode("9247b81258f97159f987a5f4f9e94df1d95e10eeabff2836020eafb27a8228b0"))
    val miningTimestamp = 1494604913

    val fullBlock: Either[BlockPreparationError, Block] = result.right
      .map(pb => pb.block.copy(header = pb.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)))
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.header, blockchain) shouldBe Right(b.header))
    fullBlock.right.foreach(b => ledger.executeBlock(b, blockchainStorages.storages, validators) shouldBe a[Right[_, Seq[Receipt]]])
    fullBlock.right.foreach(b => b.body.transactionList shouldBe Seq(signedTransaction))
  }

  trait TestSetup {

    val testAddress = 42
    val privateKey = BigInt(1, Hex.decode("f3202185c84325302d43887e90a2e23e7bc058d0450bb58ef2f7585765d7d48b"))
    lazy val keyPair: AsymmetricCipherKeyPair = keyPairFromPrvKey(privateKey)
    lazy val pubKey: Array[Byte] = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail
    lazy val address = Address(crypto.kec256(pubKey).drop(FirstByteOfAddress))

    val txGasLimit = 21000
    val txTransfer = 9000
    val transaction = Transaction(
      nonce = 0,
      gasPrice = 1,
      gasLimit = txGasLimit,
      receivingAddress = Address(testAddress),
      value = txTransfer,
      payload = ByteString.empty)
    lazy val signedTransaction: SignedTransaction = SignedTransaction.sign(transaction, keyPair, Some(0x3d.toByte))
    lazy val duplicatedSignedTransaction: SignedTransaction = SignedTransaction.sign(transaction.copy(gasLimit = 2), keyPair, Some(0x3d.toByte))

    val blockchainStorages = new SharedEphemDataSources with Storages.DefaultStorages
    lazy val blockchainConfig = new BlockchainConfig {
      override val frontierBlockNumber: BigInt = 0
      override val homesteadBlockNumber: BigInt = 1150000
      override val difficultyBombPauseBlockNumber: BigInt = 3000000
      override val difficultyBombContinueBlockNumber: BigInt = 5000000
      override val eip155BlockNumber: BigInt = 0
      override val chainId: Byte = 0x3d.toByte
      override val customGenesisFileOpt: Option[String] = Some("test-genesis.json")
      override val monetaryPolicyConfig: MonetaryPolicyConfig = MonetaryPolicyConfig(5000000, 0.2, BigInt("5000000000000000000"))

      // unused
      override val daoForkBlockNumber: BigInt = Long.MaxValue
      override val eip160BlockNumber: BigInt = Long.MaxValue
      override val eip150BlockNumber: BigInt = Long.MaxValue
      override val daoForkBlockHash: ByteString = ByteString("unused")
      override val accountStartNonce: UInt256 = UInt256.Zero
    }
    lazy val ledger = new LedgerImpl(VM, blockchainConfig)

    lazy val validators = new Validators {
      val blockValidator: BlockValidator = BlockValidator
      val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidatorImpl(blockchainConfig)
      val ommersValidator: OmmersValidator = new OmmersValidatorImpl(blockchainConfig)
      val signedTransactionValidator: SignedTransactionValidator = new SignedTransactionValidatorImpl(blockchainConfig)
    }

    lazy val blockchain = BlockchainImpl(blockchainStorages.storages)

    lazy val genesisDataLoader = new GenesisDataLoader(blockchainStorages.ephemDataSource, blockchain, blockchainConfig)
    genesisDataLoader.loadGenesisData()

    val miningConfig = new MiningConfig {
      override val coinbase: Address = Address(42)
      override val blockCacheSize: Int = 30
      override val ommersPoolSize: Int = 30
      override val ommerPoolQueryTimeout: FiniteDuration = Timeouts.normalTimeout
    }

    lazy val blockTimestampProvider = new FakeBlockTimestampProvider

    lazy val blockGenerator = new BlockGenerator(blockchainStorages.storages, blockchainConfig, miningConfig, ledger, validators, blockTimestampProvider)
  }
}

class FakeBlockTimestampProvider extends BlockTimestampProvider {
  private var timestamp = Instant.now.getEpochSecond

  def advance(seconds: Long): Unit = timestamp += seconds

  override def getEpochSecond: Long = timestamp
}
