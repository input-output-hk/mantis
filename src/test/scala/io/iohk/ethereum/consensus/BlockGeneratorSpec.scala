package io.iohk.ethereum.consensus

import java.time.Instant
import java.util.concurrent.Executors

import akka.util.ByteString
import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.blocks.{BlockTimestampProvider, PendingBlock}
import io.iohk.ethereum.consensus.ethash.validators.ValidatorsExecutor
import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.{HefEmpty, HefPostEcip1097, HefPostEcip1098}
import io.iohk.ethereum.domain.SignedTransaction.FirstByteOfAddress
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.{BlockExecution, BlockQueue, BlockValidation}
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException
import io.iohk.ethereum.utils._
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.bouncycastle.util.encoders.Hex
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BlockGeneratorSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks with Logger {
  implicit val testContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))

  "BlockGenerator" should "generate correct block with empty transactions" in new TestSetup {
    val pendingBlock =
      blockGenerator.generateBlock(bestBlock, Nil, Address(testAddress), blockGenerator.emptyX)

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("eb49a2da108d63de"))
    val minedMixHash = ByteString(Hex.decode("a91c44e62d17005c4b22f6ed116f485ea30d8b63f2429745816093b304eb4f73"))
    val miningTimestamp = 1508751768

    val fullBlock = pendingBlock.block.copy(header =
      pendingBlock.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
    )
    validators.blockHeaderValidator.validate(
      fullBlock.header,
      blockchain.getBlockHeaderByHash
    ) shouldBe Right(BlockHeaderValid)
    blockExecution.executeBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "generate correct block with transactions" in new TestSetup {
    val pendingBlock =
      blockGenerator.generateBlock(bestBlock, Seq(signedTransaction.tx), Address(testAddress), blockGenerator.emptyX)

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("4139b957dae0488d"))
    val minedMixHash = ByteString(Hex.decode("dc25764fb562d778e5d1320f4c3ba4b09021a2603a0816235e16071e11f342ea"))
    val miningTimestamp = 1508752265

    val fullBlock = pendingBlock.block.copy(header =
      pendingBlock.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
    )
    validators.blockHeaderValidator.validate(
      fullBlock.header,
      blockchain.getBlockHeaderByHash
    ) shouldBe Right(BlockHeaderValid)
    blockExecution.executeBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "be possible to simulate transaction, on world returned with pending block" in new TestSetup {
    val pendingBlock =
      blockGenerator.generateBlock(bestBlock, Seq(signedTransaction.tx), Address(testAddress), blockGenerator.emptyX)

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("4139b957dae0488d"))
    val minedMixHash = ByteString(Hex.decode("dc25764fb562d778e5d1320f4c3ba4b09021a2603a0816235e16071e11f342ea"))
    val miningTimestamp = 1508752265

    val fullBlock = pendingBlock.block.copy(header =
      pendingBlock.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
    )

    // Import Block, to create some existing state
    val res = Await.result(ledger.importBlock(fullBlock), Duration.Inf)

    // Create new pending block, with updated stateRootHash
    val newPendingBlock: PendingBlock = blockGenerator.generateBlock(
      blockchain.getBestBlock(),
      Seq(signedTransaction.tx),
      Address(testAddress),
      blockGenerator.emptyX
    )

    val pendBlockAndState = blockGenerator.getPendingBlockAndState.get

    // Try to simulate transaction, on world with updated stateRootHash, but not updated storages
    assertThrows[MPTException] {
      stxLedger.simulateTransaction(signedTransaction, pendBlockAndState.pendingBlock.block.header, None)
    }

    // Try to simulate transaction, on world with all changes stored in caches
    val simulationResult = stxLedger.simulateTransaction(
      signedTransaction,
      pendBlockAndState.pendingBlock.block.header,
      Some(pendBlockAndState.worldState)
    )

    // Check if transaction was valid
    simulationResult.vmError shouldBe None
  }

  it should "filter out failing transactions" in new TestSetup {
    val pendingBlock =
      blockGenerator.generateBlock(
        bestBlock,
        Seq(signedTransaction.tx, duplicatedSignedTransaction.tx),
        Address(testAddress),
        blockGenerator.emptyX
      )

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("12cb47f9208d1e81"))
    val minedMixHash = ByteString(Hex.decode("908471b57f2d3e70649f9ce0c9c318d61146d3ce19f70d2f94309f135b87b64a"))
    val miningTimestamp = 1508752389

    val fullBlock = pendingBlock.block.copy(header =
      pendingBlock.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
    )
    validators.blockHeaderValidator.validate(
      fullBlock.header,
      blockchain.getBlockHeaderByHash
    ) shouldBe Right(BlockHeaderValid)

    blockExecution.executeBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(signedTransaction.tx)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "filter out transactions exceeding block gas limit and include correct transactions" in new TestSetup {
    val txWitGasTooBigGasLimit = SignedTransaction
      .sign(
        transaction.copy(gasLimit = BigInt(2).pow(100000), nonce = signedTransaction.tx.tx.nonce + 1),
        keyPair,
        Some(0x3d.toByte)
      )
      .tx

    val transactions = Seq(txWitGasTooBigGasLimit, signedTransaction.tx, duplicatedSignedTransaction.tx)
    val pendingBlock =
      blockGenerator.generateBlock(bestBlock, transactions, Address(testAddress), blockGenerator.emptyX)

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("38026e10fb18b458"))
    val minedMixHash = ByteString(Hex.decode("806f26f0efb12a0c0c16e587984227186c46f25fc4e76698a68996183edf2cf1"))
    val miningTimestamp = 1508752492

    val fullBlock = pendingBlock.block.copy(header =
      pendingBlock.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
    )

    validators.blockHeaderValidator.validate(
      fullBlock.header,
      blockchain.getBlockHeaderByHash
    ) shouldBe Right(BlockHeaderValid)
    blockExecution.executeBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(signedTransaction.tx)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "generate block before eip155 and filter out chain specific tx" in new TestSetup {
    override lazy val blockchainConfig = BlockchainConfig(
      frontierBlockNumber = 0,
      homesteadBlockNumber = 1150000,
      difficultyBombPauseBlockNumber = 3000000,
      difficultyBombContinueBlockNumber = 5000000,
      difficultyBombRemovalBlockNumber = 5900000,
      eip155BlockNumber = Long.MaxValue,
      eip106BlockNumber = Long.MaxValue,
      byzantiumBlockNumber = Long.MaxValue,
      constantinopleBlockNumber = Long.MaxValue,
      istanbulBlockNumber = Long.MaxValue,
      chainId = 0x3d.toByte,
      networkId = 1,
      customGenesisFileOpt = Some("test-genesis.json"),
      monetaryPolicyConfig =
        MonetaryPolicyConfig(5000000, 0.2, 5000000000000000000L, 3000000000000000000L, 2000000000000000000L),
      // unused
      maxCodeSize = None,
      eip160BlockNumber = Long.MaxValue,
      eip150BlockNumber = Long.MaxValue,
      eip161BlockNumber = Long.MaxValue,
      accountStartNonce = UInt256.Zero,
      daoForkConfig = None,
      bootstrapNodes = Set(),
      gasTieBreaker = false,
      ethCompatibleStorage = true,
      atlantisBlockNumber = Long.MaxValue,
      aghartaBlockNumber = Long.MaxValue,
      phoenixBlockNumber = Long.MaxValue,
      petersburgBlockNumber = Long.MaxValue,
      ecip1098BlockNumber = Long.MaxValue,
      treasuryAddress = Address(0),
      ecip1097BlockNumber = Long.MaxValue
    )

    override lazy val blockExecution =
      new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation)

    val generalTx = SignedTransaction.sign(transaction, keyPair, None).tx
    val specificTx =
      SignedTransaction.sign(transaction.copy(nonce = transaction.nonce + 1), keyPair, Some(0x3d.toByte)).tx

    val pendingBlock =
      blockGenerator.generateBlock(bestBlock, Seq(generalTx, specificTx), Address(testAddress), blockGenerator.emptyX)

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("48381cb0cd40936a"))
    val minedMixHash = ByteString(Hex.decode("dacd96cf5dbc662fa113c73319fcdc7d6e7053571432345b936fd221c1e18d42"))
    val miningTimestamp = 1499952002

    val fullBlock =
      pendingBlock.block.copy(header =
        pendingBlock.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
      )
    validators.blockHeaderValidator.validate(
      fullBlock.header,
      blockchain.getBlockHeaderByHash
    ) shouldBe Right(BlockHeaderValid)
    blockExecution.executeBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(generalTx)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "generate correct block with (without empty accounts) after EIP-161" in new TestSetup {
    override lazy val blockchainConfig = BlockchainConfig(
      frontierBlockNumber = 0,
      homesteadBlockNumber = 1150000,
      difficultyBombPauseBlockNumber = 3000000,
      difficultyBombContinueBlockNumber = 5000000,
      difficultyBombRemovalBlockNumber = 5900000,
      eip155BlockNumber = Long.MaxValue,
      eip106BlockNumber = Long.MaxValue,
      byzantiumBlockNumber = Long.MaxValue,
      constantinopleBlockNumber = Long.MaxValue,
      istanbulBlockNumber = Long.MaxValue,
      chainId = 0x3d.toByte,
      networkId = 1,
      customGenesisFileOpt = Some("test-genesis.json"),
      monetaryPolicyConfig =
        MonetaryPolicyConfig(5000000, 0.2, 5000000000000000000L, 3000000000000000000L, 2000000000000000000L),
      // unused
      maxCodeSize = None,
      eip160BlockNumber = Long.MaxValue,
      eip150BlockNumber = Long.MaxValue,
      eip161BlockNumber = 0,
      accountStartNonce = UInt256.Zero,
      daoForkConfig = None,
      bootstrapNodes = Set(),
      gasTieBreaker = false,
      ethCompatibleStorage = true,
      atlantisBlockNumber = Long.MaxValue,
      aghartaBlockNumber = Long.MaxValue,
      phoenixBlockNumber = Long.MaxValue,
      petersburgBlockNumber = Long.MaxValue,
      ecip1098BlockNumber = Long.MaxValue,
      treasuryAddress = Address(0),
      ecip1097BlockNumber = Long.MaxValue
    )

    override lazy val blockExecution =
      new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation)

    val transaction1 = Transaction(
      nonce = 0,
      gasPrice = 1,
      gasLimit = 1000000,
      receivingAddress = None,
      value = 0,
      payload = ByteString.empty
    )
    val generalTx = SignedTransaction.sign(transaction1, keyPair, None).tx

    val generatedBlock =
      blockGenerator.generateBlock(bestBlock, Seq(generalTx), Address(testAddress), blockGenerator.emptyX)

    blockExecution.executeBlock(generatedBlock.block, true) shouldBe a[Right[_, Seq[Receipt]]]
  }

  it should "generate block after eip155 and allow both chain specific and general transactions" in new TestSetup {
    val generalTx = SignedTransaction.sign(transaction.copy(nonce = transaction.nonce + 1), keyPair, None).tx

    val pendingBlock =
      blockGenerator.generateBlock(
        bestBlock,
        Seq(generalTx, signedTransaction.tx),
        Address(testAddress),
        blockGenerator.emptyX
      )

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("39bd50fcbde30b18"))
    val minedMixHash = ByteString(Hex.decode("c77dae7cef6c685896ed6b8026466a2e6338b8bc5f182e2dd7a64cf7da9c7d1b"))
    val miningTimestamp = 1499951223

    val fullBlock =
      pendingBlock.block.copy(header =
        pendingBlock.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
      )
    validators.blockHeaderValidator.validate(fullBlock.header, blockchain.getBlockHeaderByHash) shouldBe Right(
      BlockHeaderValid
    )
    blockExecution.executeBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(signedTransaction.tx, generalTx)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "include consecutive transactions from single sender" in new TestSetup {
    val nextTransaction =
      SignedTransaction.sign(transaction.copy(nonce = signedTransaction.tx.tx.nonce + 1), keyPair, Some(0x3d.toByte)).tx

    val pendingBlock =
      blockGenerator.generateBlock(
        bestBlock,
        Seq(nextTransaction, signedTransaction.tx),
        Address(testAddress),
        blockGenerator.emptyX
      )

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("8f88ec20f1be482f"))
    val minedMixHash = ByteString(Hex.decode("247a206abc088487edc1697fcaceb33ad87b55666e438129b7048bb08c8ed88f"))
    val miningTimestamp = 1499721182

    val fullBlock =
      pendingBlock.block.copy(header =
        pendingBlock.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
      )
    validators.blockHeaderValidator.validate(fullBlock.header, blockchain.getBlockHeaderByHash) shouldBe Right(
      BlockHeaderValid
    )
    blockExecution.executeBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(signedTransaction.tx, nextTransaction)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "filter out failing transaction from the middle of tx list" in new TestSetup {
    val nextTransaction =
      SignedTransaction.sign(transaction.copy(nonce = signedTransaction.tx.tx.nonce + 1), keyPair, Some(0x3d.toByte)).tx

    val privateKeyWithNoEthere =
      BigInt(1, Hex.decode("584a31be275195585603ddd05a53d16fae9deafba67213b6060cec9f16e44cae"))

    val failingTransaction = Transaction(
      nonce = 0,
      gasPrice = 1,
      gasLimit = txGasLimit,
      receivingAddress = Address(testAddress),
      value = txTransfer,
      payload = ByteString.empty
    )
    val signedFailingTransaction =
      SignedTransaction.sign(failingTransaction, keyPairFromPrvKey(privateKeyWithNoEthere), Some(0x3d.toByte)).tx

    val pendingBlock =
      blockGenerator.generateBlock(
        bestBlock,
        Seq(nextTransaction, signedFailingTransaction, signedTransaction.tx),
        Address(testAddress),
        blockGenerator.emptyX
      )

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("8f88ec20f1be482f"))
    val minedMixHash = ByteString(Hex.decode("247a206abc088487edc1697fcaceb33ad87b55666e438129b7048bb08c8ed88f"))
    val miningTimestamp = 1499721182

    val fullBlock = pendingBlock.block.copy(header =
      pendingBlock.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
    )
    validators.blockHeaderValidator.validate(fullBlock.header, blockchain.getBlockHeaderByHash) shouldBe Right(
      BlockHeaderValid
    )
    blockExecution.executeBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(signedTransaction.tx, nextTransaction)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "include transaction with higher gas price if nonce is the same" in new TestSetup {
    val txWitSameNonceButLowerGasPrice = SignedTransaction
      .sign(transaction.copy(gasPrice = signedTransaction.tx.tx.gasPrice - 1), keyPair, Some(0x3d.toByte))
      .tx

    val pendingBlock =
      blockGenerator.generateBlock(
        bestBlock,
        Seq(txWitSameNonceButLowerGasPrice, signedTransaction.tx),
        Address(testAddress),
        blockGenerator.emptyX
      )

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("14d7000ac544b38e"))
    val minedMixHash = ByteString(Hex.decode("270f6b2618c5bef6a188397927129c803e5fd41c85492835486832f6825a8d78"))
    val miningTimestamp = 1508752698

    val fullBlock = pendingBlock.block.copy(header =
      pendingBlock.block.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
    )
    validators.blockHeaderValidator.validate(fullBlock.header, blockchain.getBlockHeaderByHash) shouldBe Right(BlockHeaderValid)
    blockExecution.executeBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(signedTransaction.tx)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "generate blocks with the correct extra fields" in {
    val table = Table[Boolean, Boolean, Boolean, HeaderExtraFields](
      ("ecip1098Activated", "ecip1097Activated", "selectedOptOut", "expectedExtraFields"),
      // No ecip activated
      (false, false, true, HefEmpty),
      (false, false, false, HefEmpty),
      // ECIP 1098 activated
      (true, false, true, HefPostEcip1098(true)),
      (true, false, false, HefPostEcip1098(false)),
      // ECIP 1097 and 1098 activated
      (true, true, true, HefPostEcip1097(true, None)),
      (true, true, false, HefPostEcip1097(false, None))
    )

    forAll(table) { case (ecip1098Activated, ecip1097Activated, selectedOptOut, headerExtraFields) =>
      val testSetup = new TestSetup {
        override lazy val blockchainConfig = baseBlockchainConfig.copy(ecip1098BlockNumber = 1000, ecip1097BlockNumber = 2000)

        override lazy val consensusConfig = buildConsensusConfig().copy(treasuryOptOut = selectedOptOut)
      }
      import testSetup._

      val blockNumber =
        if (ecip1098Activated && ecip1097Activated)
          blockchainConfig.ecip1097BlockNumber * 2
        else if (ecip1098Activated)
          (blockchainConfig.ecip1097BlockNumber + blockchainConfig.ecip1098BlockNumber) / 2
        else
          blockchainConfig.ecip1098BlockNumber / 2
      val parentBlock = bestBlock.copy(header = bestBlock.header.copy(number = blockNumber - 1))
      val generatedBlock = blockGenerator.generateBlock(parentBlock, Nil, Address(testAddress), blockGenerator.emptyX)

      generatedBlock.block.header.extraFields shouldBe headerExtraFields
    }

  }

  trait TestSetup extends EphemBlockchainTestSetup {

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
      payload = ByteString.empty
    )

    lazy val signedTransaction = SignedTransaction.sign(transaction, keyPair, Some(0x3d.toByte))
    lazy val duplicatedSignedTransaction =
      SignedTransaction.sign(transaction.copy(gasLimit = 2), keyPair, Some(0x3d.toByte))

    val baseBlockchainConfig = BlockchainConfig(
      frontierBlockNumber = 0,
      homesteadBlockNumber = 1150000,
      difficultyBombPauseBlockNumber = 3000000,
      difficultyBombContinueBlockNumber = 5000000,
      difficultyBombRemovalBlockNumber = 5900000,
      eip155BlockNumber = 0,
      eip106BlockNumber = Long.MaxValue,
      byzantiumBlockNumber = Long.MaxValue,
      constantinopleBlockNumber = Long.MaxValue,
      istanbulBlockNumber = Long.MaxValue,
      chainId = 0x3d.toByte,
      networkId = 1,
      customGenesisFileOpt = Some("test-genesis.json"),
      monetaryPolicyConfig =
        MonetaryPolicyConfig(5000000, 0.2, 5000000000000000000L, 3000000000000000000L, 2000000000000000000L),
      // unused
      maxCodeSize = None,
      eip160BlockNumber = Long.MaxValue,
      eip150BlockNumber = Long.MaxValue,
      eip161BlockNumber = Long.MaxValue,
      accountStartNonce = UInt256.Zero,
      daoForkConfig = None,
      bootstrapNodes = Set(),
      gasTieBreaker = false,
      ethCompatibleStorage = true,
      atlantisBlockNumber = Long.MaxValue,
      aghartaBlockNumber = Long.MaxValue,
      phoenixBlockNumber = Long.MaxValue,
      petersburgBlockNumber = Long.MaxValue,
      ecip1098BlockNumber = Long.MaxValue,
      treasuryAddress = Address(0),
      ecip1097BlockNumber = Long.MaxValue
    )
    override lazy val blockchainConfig = baseBlockchainConfig

    val genesisDataLoader = new GenesisDataLoader(blockchain, blockchainConfig)
    genesisDataLoader.loadGenesisData()

    val bestBlock = blockchain.getBestBlock()

    lazy val blockTimestampProvider = new FakeBlockTimestampProvider

    val blockCacheSize: Int = 30
    val headerExtraData: ByteString = ByteString("mined with etc scala")

    override lazy val validators: ValidatorsExecutor = ethashValidators

    override lazy val consensusConfig =
      buildConsensusConfig().copy(headerExtraData = headerExtraData, blockCacheSize = blockCacheSize)

    lazy val blockGenerator = consensus.blockGenerator.withBlockTimestampProvider(blockTimestampProvider)

    lazy val blockValidation = new BlockValidation(consensus, blockchain, BlockQueue(blockchain, syncConfig))
    lazy val blockExecution =
      new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation)

  }
}

class FakeBlockTimestampProvider extends BlockTimestampProvider {
  private var timestamp = Instant.now.getEpochSecond

  def advance(seconds: Long): Unit = timestamp += seconds

  override def getEpochSecond: Long = timestamp
}
