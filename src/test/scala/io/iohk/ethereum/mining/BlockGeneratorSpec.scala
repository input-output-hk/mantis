package io.iohk.ethereum.mining

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.{BlockPreparationError, LedgerImpl}
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm.{UInt256, VM}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.SignedTransaction.FirstByteOfAddress
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.{ECPrivateKeyParameters, ECPublicKeyParameters}

class BlockGeneratorSpec extends FlatSpec with Matchers with PropertyChecks with Logger {

  "BlockGenerator" should "generate correct block with empty transactions" in new Envirnoment {
    val result: Either[BlockPreparationError, Block] = blockGenerator.generateBlockForMining(1, Nil, Nil, Address(testAddress))
    result shouldBe a[Right[_, Block]]

    //mined with etc-client + ethminer
    val minedNonce = ByteString(Hex.decode("ce1b500070aeec4f"))
    val minedMixHash = ByteString(Hex.decode("40d9bd2064406d7f22390766d6fe5eccd2a67aa89bf218e99df35b2dbb425fb1"))
    val miningTimestamp = 1494604913

    val fulBlock: Either[BlockPreparationError, Block] = result.right
      .map(b => b.copy(header = b.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)))
    fulBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.header, blockchain) shouldBe Right(b.header))
    fulBlock.right.foreach(b => ledger.executeBlock(b, blockchainStorages.storages, validators) shouldBe a[Right[_, Seq[Receipt]]])
  }

  "BlockGenerator" should "generate correct block with transactions" in new Envirnoment {
    val result: Either[BlockPreparationError, Block] = blockGenerator.generateBlockForMining(1, Seq(signedTransaction), Nil, Address(testAddress))
    result shouldBe a[Right[_, Block]]

    //mined with etc-client + ethminer
    val minedNonce = ByteString(Hex.decode("5e8d5c12cea7e0c7"))
    val minedMixHash = ByteString(Hex.decode("9247b81258f97159f987a5f4f9e94df1d95e10eeabff2836020eafb27a8228b0"))
    val miningTimestamp = 1494604913

    val fulBlock: Either[BlockPreparationError, Block] = result.right
      .map(b => b.copy(header = b.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)))
    fulBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.header, blockchain) shouldBe Right(b.header))
    fulBlock.right.foreach(b => ledger.executeBlock(b, blockchainStorages.storages, validators) shouldBe a[Right[_, Seq[Receipt]]])
  }

  trait Envirnoment {

    val testAddress = 42
    val privateKey = BigInt(1, Hex.decode("f3202185c84325302d43887e90a2e23e7bc058d0450bb58ef2f7585765d7d48b"))
    val keyPair: AsymmetricCipherKeyPair = getKeyPair(privateKey)
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
    val signedTransaction: SignedTransaction = SignedTransaction.sign(transaction, keyPair, 0x3d.toByte)

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

    val blockGenerator = new BlockGenerator(blockchainStorages.storages, blockchainConfig, ledger, validators)
  }
}
