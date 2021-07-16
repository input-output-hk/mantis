package io.iohk.ethereum.consensus.blocks

import java.time.Instant

import akka.util.ByteString

import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService

import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.mining.MiningConfig
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.HefEmpty
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.HefPostEcip1097
import io.iohk.ethereum.domain.SignedTransaction.FirstByteOfAddress
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecution
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationAfterExecError
import io.iohk.ethereum.ledger.BlockQueue
import io.iohk.ethereum.ledger.BlockValidation
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException
import io.iohk.ethereum.utils._

class BlockGeneratorSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks with Logger {
  implicit val testContext: SchedulerService = Scheduler.fixedPool("block-generator-spec-pool", 4)

  "BlockGenerator" should "generate correct block with empty transactions" in new TestSetup {
    val pendingBlock =
      blockGenerator.generateBlock(bestBlock.get, Nil, Address(testAddress), blockGenerator.emptyX, None).pendingBlock

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("eb49a2da108d63de"))
    val minedMixHash = ByteString(Hex.decode("a91c44e62d17005c4b22f6ed116f485ea30d8b63f2429745816093b304eb4f73"))
    val miningTimestamp = 1508751768

    val fullBlock = pendingBlock.block.copy(
      header = pendingBlock.block.header.copy(
        nonce = minedNonce,
        mixHash = minedMixHash,
        unixTimestamp = miningTimestamp,
        gasLimit = generatedBlockGasLimit
      )
    )
    validators.blockHeaderValidator.validate(
      fullBlock.header,
      blockchainReader.getBlockHeaderByHash
    ) shouldBe Right(BlockHeaderValid)
    blockExecution.executeAndValidateBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "generate correct block with transactions" in new TestSetup {
    val pendingBlock =
      blockGenerator
        .generateBlock(bestBlock.get, Seq(signedTransaction), Address(testAddress), blockGenerator.emptyX, None)
        .pendingBlock

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("4139b957dae0488d"))
    val minedMixHash = ByteString(Hex.decode("dc25764fb562d778e5d1320f4c3ba4b09021a2603a0816235e16071e11f342ea"))
    val miningTimestamp = 1508752265

    val fullBlock = pendingBlock.block.copy(
      header = pendingBlock.block.header.copy(
        nonce = minedNonce,
        mixHash = minedMixHash,
        unixTimestamp = miningTimestamp,
        gasLimit = generatedBlockGasLimit
      )
    )
    validators.blockHeaderValidator.validate(
      fullBlock.header,
      blockchainReader.getBlockHeaderByHash
    ) shouldBe Right(BlockHeaderValid)
    blockExecution.executeAndValidateBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "be possible to simulate transaction, on world returned with pending block" in new TestSetup {
    val pendingBlock =
      blockGenerator
        .generateBlock(bestBlock.get, Seq(signedTransaction), Address(testAddress), blockGenerator.emptyX, None)
        .pendingBlock

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("4139b957dae0488d"))
    val minedMixHash = ByteString(Hex.decode("dc25764fb562d778e5d1320f4c3ba4b09021a2603a0816235e16071e11f342ea"))
    val miningTimestamp = 1508752265

    val fullBlock = pendingBlock.block.copy(
      header = pendingBlock.block.header.copy(
        nonce = minedNonce,
        mixHash = minedMixHash,
        unixTimestamp = miningTimestamp,
        gasLimit = generatedBlockGasLimit
      )
    )

    // Import Block, to create some existing state
    blockImport.importBlock(fullBlock).runSyncUnsafe()

    // Create new pending block, with updated stateRootHash
    val pendBlockAndState = blockGenerator.generateBlock(
      blockchainReader.getBestBlock().get,
      Seq(signedTransaction),
      Address(testAddress),
      blockGenerator.emptyX,
      None
    )

    // Try to simulate transaction, on world with updated stateRootHash, but not updated storages
    assertThrows[MPTException] {
      stxLedger.simulateTransaction(signedTransactionWithAddress, pendBlockAndState.pendingBlock.block.header, None)
    }

    // Try to simulate transaction, on world with all changes stored in caches
    val simulationResult = stxLedger.simulateTransaction(
      signedTransactionWithAddress,
      pendBlockAndState.pendingBlock.block.header,
      Some(pendBlockAndState.worldState)
    )

    // Check if transaction was valid
    simulationResult.vmError shouldBe None
  }

  it should "filter out failing transactions" in new TestSetup {
    val pendingBlock =
      blockGenerator
        .generateBlock(
          bestBlock.get,
          Seq(signedTransaction, duplicatedSignedTransaction),
          Address(testAddress),
          blockGenerator.emptyX,
          None
        )
        .pendingBlock

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("12cb47f9208d1e81"))
    val minedMixHash = ByteString(Hex.decode("908471b57f2d3e70649f9ce0c9c318d61146d3ce19f70d2f94309f135b87b64a"))
    val miningTimestamp = 1508752389

    val fullBlock = pendingBlock.block.copy(
      header = pendingBlock.block.header.copy(
        nonce = minedNonce,
        mixHash = minedMixHash,
        unixTimestamp = miningTimestamp,
        gasLimit = generatedBlockGasLimit
      )
    )
    validators.blockHeaderValidator.validate(
      fullBlock.header,
      blockchainReader.getBlockHeaderByHash
    ) shouldBe Right(BlockHeaderValid)

    blockExecution.executeAndValidateBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(signedTransaction)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "filter out transactions exceeding block gas limit and include correct transactions" in new TestSetup {
    val txWitGasTooBigGasLimit = SignedTransaction
      .sign(
        transaction.copy(gasLimit = BigInt(2).pow(100000), nonce = signedTransaction.tx.nonce + 1),
        keyPair,
        Some(0x3d.toByte)
      )

    val transactions = Seq(txWitGasTooBigGasLimit, signedTransaction, duplicatedSignedTransaction)
    val pendingBlock =
      blockGenerator
        .generateBlock(bestBlock.get, transactions, Address(testAddress), blockGenerator.emptyX, None)
        .pendingBlock

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("38026e10fb18b458"))
    val minedMixHash = ByteString(Hex.decode("806f26f0efb12a0c0c16e587984227186c46f25fc4e76698a68996183edf2cf1"))
    val miningTimestamp = 1508752492

    val fullBlock = pendingBlock.block.copy(
      header = pendingBlock.block.header.copy(
        nonce = minedNonce,
        mixHash = minedMixHash,
        unixTimestamp = miningTimestamp,
        gasLimit = generatedBlockGasLimit
      )
    )

    validators.blockHeaderValidator.validate(
      fullBlock.header,
      blockchainReader.getBlockHeaderByHash
    ) shouldBe Right(BlockHeaderValid)
    blockExecution.executeAndValidateBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(signedTransaction)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "generate block before eip155 and filter out chain specific tx" in new TestSetup {
    implicit override lazy val blockchainConfig = BlockchainConfig(
      chainId = 0x3d.toByte,
      networkId = 1,
      customGenesisFileOpt = Some("test-genesis.json"),
      customGenesisJsonOpt = None,
      monetaryPolicyConfig =
        MonetaryPolicyConfig(5000000, 0.2, 5000000000000000000L, 3000000000000000000L, 2000000000000000000L),
      // unused
      maxCodeSize = None,
      accountStartNonce = UInt256.Zero,
      daoForkConfig = None,
      bootstrapNodes = Set(),
      gasTieBreaker = false,
      ethCompatibleStorage = true,
      treasuryAddress = Address(0),
      forkBlockNumbers = ForkBlockNumbers(
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
        eip160BlockNumber = Long.MaxValue,
        eip150BlockNumber = Long.MaxValue,
        eip161BlockNumber = Long.MaxValue,
        atlantisBlockNumber = Long.MaxValue,
        aghartaBlockNumber = Long.MaxValue,
        phoenixBlockNumber = Long.MaxValue,
        petersburgBlockNumber = Long.MaxValue,
        ecip1098BlockNumber = Long.MaxValue,
        ecip1097BlockNumber = Long.MaxValue,
        ecip1099BlockNumber = Long.MaxValue,
        ecip1049BlockNumber = None
      )
    )

    override lazy val blockExecution =
      new BlockExecution(
        blockchain,
        blockchainReader,
        blockchainWriter,
        storagesInstance.storages.evmCodeStorage,
        mining.blockPreparator,
        blockValidation
      )

    val generalTx = SignedTransaction.sign(transaction, keyPair, None)
    val specificTx = SignedTransaction.sign(transaction.copy(nonce = transaction.nonce + 1), keyPair, Some(0x3d.toByte))

    val pendingBlock =
      blockGenerator
        .generateBlock(bestBlock.get, Seq(generalTx, specificTx), Address(testAddress), blockGenerator.emptyX, None)
        .pendingBlock

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("48381cb0cd40936a"))
    val minedMixHash = ByteString(Hex.decode("dacd96cf5dbc662fa113c73319fcdc7d6e7053571432345b936fd221c1e18d42"))
    val miningTimestamp = 1499952002

    val fullBlock =
      pendingBlock.block.copy(
        header = pendingBlock.block.header.copy(
          nonce = minedNonce,
          mixHash = minedMixHash,
          unixTimestamp = miningTimestamp,
          gasLimit = generatedBlockGasLimit
        )
      )
    validators.blockHeaderValidator.validate(
      fullBlock.header,
      blockchainReader.getBlockHeaderByHash
    ) shouldBe Right(BlockHeaderValid)
    blockExecution.executeAndValidateBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(generalTx)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "generate correct block with (without empty accounts) after EIP-161" in new TestSetup {
    implicit override lazy val blockchainConfig = BlockchainConfig(
      forkBlockNumbers = ForkBlockNumbers(
        frontierBlockNumber = 0,
        homesteadBlockNumber = 1150000,
        eip155BlockNumber = Long.MaxValue,
        eip106BlockNumber = Long.MaxValue,
        difficultyBombPauseBlockNumber = 3000000,
        difficultyBombContinueBlockNumber = 5000000,
        difficultyBombRemovalBlockNumber = 5900000,
        byzantiumBlockNumber = Long.MaxValue,
        constantinopleBlockNumber = Long.MaxValue,
        istanbulBlockNumber = Long.MaxValue,
        eip160BlockNumber = Long.MaxValue,
        eip150BlockNumber = Long.MaxValue,
        eip161BlockNumber = 0,
        atlantisBlockNumber = Long.MaxValue,
        aghartaBlockNumber = Long.MaxValue,
        phoenixBlockNumber = Long.MaxValue,
        petersburgBlockNumber = Long.MaxValue,
        ecip1098BlockNumber = Long.MaxValue,
        ecip1097BlockNumber = Long.MaxValue,
        ecip1099BlockNumber = Long.MaxValue,
        ecip1049BlockNumber = None
      ),
      chainId = 0x3d.toByte,
      networkId = 1,
      customGenesisFileOpt = Some("test-genesis.json"),
      customGenesisJsonOpt = None,
      monetaryPolicyConfig =
        MonetaryPolicyConfig(5000000, 0.2, 5000000000000000000L, 3000000000000000000L, 2000000000000000000L),
      // unused
      maxCodeSize = None,
      accountStartNonce = UInt256.Zero,
      daoForkConfig = None,
      bootstrapNodes = Set(),
      gasTieBreaker = false,
      ethCompatibleStorage = true,
      treasuryAddress = Address(0)
    )

    override lazy val blockExecution =
      new BlockExecution(
        blockchain,
        blockchainReader,
        blockchainWriter,
        storagesInstance.storages.evmCodeStorage,
        mining.blockPreparator,
        blockValidation
      )

    val transaction1 = LegacyTransaction(
      nonce = 0,
      gasPrice = 1,
      gasLimit = 1000000,
      receivingAddress = None,
      value = 0,
      payload = ByteString.empty
    )
    val generalTx = SignedTransaction.sign(transaction1, keyPair, None)

    val generatedBlock =
      blockGenerator
        .generateBlock(bestBlock.get, Seq(generalTx), Address(testAddress), blockGenerator.emptyX, None)
        .pendingBlock

    blockExecution.executeAndValidateBlock(generatedBlock.block, true) shouldBe a[Right[_, Seq[Receipt]]]
  }

  it should "generate block after eip155 and allow both chain specific and general transactions" in new TestSetup {
    val generalTx = SignedTransaction.sign(transaction.copy(nonce = transaction.nonce + 1), keyPair, None)

    val pendingBlock =
      blockGenerator
        .generateBlock(
          bestBlock.get,
          Seq(generalTx, signedTransaction),
          Address(testAddress),
          blockGenerator.emptyX,
          None
        )
        .pendingBlock

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("39bd50fcbde30b18"))
    val minedMixHash = ByteString(Hex.decode("c77dae7cef6c685896ed6b8026466a2e6338b8bc5f182e2dd7a64cf7da9c7d1b"))
    val miningTimestamp = 1499951223

    val fullBlock =
      pendingBlock.block.copy(
        header = pendingBlock.block.header.copy(
          nonce = minedNonce,
          mixHash = minedMixHash,
          unixTimestamp = miningTimestamp,
          gasLimit = generatedBlockGasLimit
        )
      )
    validators.blockHeaderValidator.validate(fullBlock.header, blockchainReader.getBlockHeaderByHash) shouldBe Right(
      BlockHeaderValid
    )
    blockExecution.executeAndValidateBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(signedTransaction, generalTx)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "include consecutive transactions from single sender" in new TestSetup {
    val nextTransaction =
      SignedTransaction.sign(transaction.copy(nonce = signedTransaction.tx.nonce + 1), keyPair, Some(0x3d.toByte))

    val pendingBlock =
      blockGenerator
        .generateBlock(
          bestBlock.get,
          Seq(nextTransaction, signedTransaction),
          Address(testAddress),
          blockGenerator.emptyX,
          None
        )
        .pendingBlock

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("8f88ec20f1be482f"))
    val minedMixHash = ByteString(Hex.decode("247a206abc088487edc1697fcaceb33ad87b55666e438129b7048bb08c8ed88f"))
    val miningTimestamp = 1499721182

    val fullBlock =
      pendingBlock.block.copy(
        header = pendingBlock.block.header.copy(
          nonce = minedNonce,
          mixHash = minedMixHash,
          unixTimestamp = miningTimestamp,
          gasLimit = generatedBlockGasLimit
        )
      )
    validators.blockHeaderValidator.validate(fullBlock.header, blockchainReader.getBlockHeaderByHash) shouldBe Right(
      BlockHeaderValid
    )
    blockExecution.executeAndValidateBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(signedTransaction, nextTransaction)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "filter out failing transaction from the middle of tx list" in new TestSetup {
    val nextTransaction =
      SignedTransaction.sign(transaction.copy(nonce = signedTransaction.tx.nonce + 1), keyPair, Some(0x3d.toByte))

    val privateKeyWithNoEthere =
      BigInt(1, Hex.decode("584a31be275195585603ddd05a53d16fae9deafba67213b6060cec9f16e44cae"))

    val failingTransaction = LegacyTransaction(
      nonce = 0,
      gasPrice = 1,
      gasLimit = txGasLimit,
      receivingAddress = Address(testAddress),
      value = txTransfer,
      payload = ByteString.empty
    )
    val signedFailingTransaction =
      SignedTransaction.sign(failingTransaction, keyPairFromPrvKey(privateKeyWithNoEthere), Some(0x3d.toByte))

    val pendingBlock =
      blockGenerator
        .generateBlock(
          bestBlock.get,
          Seq(nextTransaction, signedFailingTransaction, signedTransaction),
          Address(testAddress),
          blockGenerator.emptyX,
          None
        )
        .pendingBlock

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("8f88ec20f1be482f"))
    val minedMixHash = ByteString(Hex.decode("247a206abc088487edc1697fcaceb33ad87b55666e438129b7048bb08c8ed88f"))
    val miningTimestamp = 1499721182

    val fullBlock = pendingBlock.block.copy(
      header = pendingBlock.block.header.copy(
        nonce = minedNonce,
        mixHash = minedMixHash,
        unixTimestamp = miningTimestamp,
        gasLimit = generatedBlockGasLimit
      )
    )
    validators.blockHeaderValidator.validate(fullBlock.header, blockchainReader.getBlockHeaderByHash) shouldBe Right(
      BlockHeaderValid
    )
    blockExecution.executeAndValidateBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(signedTransaction, nextTransaction)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "include transaction with higher gas price if nonce is the same" in new TestSetup {
    val txWitSameNonceButLowerGasPrice = SignedTransaction
      .sign(transaction.copy(gasPrice = signedTransaction.tx.gasPrice - 1), keyPair, Some(0x3d.toByte))

    val pendingBlock =
      blockGenerator
        .generateBlock(
          bestBlock.get,
          Seq(txWitSameNonceButLowerGasPrice, signedTransaction),
          Address(testAddress),
          blockGenerator.emptyX,
          None
        )
        .pendingBlock

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("14d7000ac544b38e"))
    val minedMixHash = ByteString(Hex.decode("270f6b2618c5bef6a188397927129c803e5fd41c85492835486832f6825a8d78"))
    val miningTimestamp = 1508752698

    val fullBlock = pendingBlock.block.copy(
      header = pendingBlock.block.header.copy(
        nonce = minedNonce,
        mixHash = minedMixHash,
        unixTimestamp = miningTimestamp,
        gasLimit = generatedBlockGasLimit
      )
    )
    validators.blockHeaderValidator.validate(fullBlock.header, blockchainReader.getBlockHeaderByHash) shouldBe Right(
      BlockHeaderValid
    )
    blockExecution.executeAndValidateBlock(fullBlock) shouldBe a[Right[_, Seq[Receipt]]]
    fullBlock.body.transactionList shouldBe Seq(signedTransaction)
    fullBlock.header.extraData shouldBe headerExtraData
  }

  it should "generate blocks with the correct extra fields" in {
    val table = Table[Boolean, Boolean, HeaderExtraFields](
      ("ecip1098Activated", "ecip1097Activated", "expectedExtraFields"),
      // No ecip activated
      (false, false, HefEmpty),
      (false, false, HefEmpty),
      // ECIP 1098 activated
      (true, false, HefEmpty),
      (true, false, HefEmpty),
      // ECIP 1097 and 1098 activated
      (true, true, HefPostEcip1097(None)),
      (true, true, HefPostEcip1097(None))
    )

    forAll(table) { case (ecip1098Activated, ecip1097Activated, headerExtraFields) =>
      val testSetup = new TestSetup {
        override lazy val blockchainConfig =
          baseBlockchainConfig.withUpdatedForkBlocks(
            _.copy(
              ecip1098BlockNumber = 1000,
              ecip1097BlockNumber = 2000
            )
          )

        override lazy val miningConfig = buildMiningConfig()
      }
      import testSetup._

      val blockNumber =
        if (ecip1098Activated && ecip1097Activated)
          blockchainConfig.forkBlockNumbers.ecip1097BlockNumber * 2
        else if (ecip1098Activated)
          (blockchainConfig.forkBlockNumbers.ecip1097BlockNumber + blockchainConfig.forkBlockNumbers.ecip1098BlockNumber) / 2
        else
          blockchainConfig.forkBlockNumbers.ecip1098BlockNumber / 2
      val parentBlock = bestBlock.get.copy(header = bestBlock.get.header.copy(number = blockNumber - 1))
      val generatedBlock =
        blockGenerator.generateBlock(parentBlock, Nil, Address(testAddress), blockGenerator.emptyX, None).pendingBlock

      generatedBlock.block.header.extraFields shouldBe headerExtraFields
    }
  }

  it should "generate a failure if treasury transfer was not made" in {
    val producer = new TestSetup {
      override lazy val blockchainConfig = baseBlockchainConfig
        .withUpdatedForkBlocks(
          _.copy(
            ecip1098BlockNumber = 20000000
          )
        )
        .copy(
          treasuryAddress = treasuryAccount,
          customGenesisFileOpt = Some("test-genesis-treasury.json")
        )
      override lazy val miningConfig = buildMiningConfig()
    }
    val block = {
      import producer._
      blockGenerator
        .generateBlock(bestBlock.get, Seq.empty, Address(testAddress), blockGenerator.emptyX, None)
        .pendingBlock
    }

    val validator = new TestSetup {
      override lazy val blockchainConfig = baseBlockchainConfig
        .withUpdatedForkBlocks(_.copy(ecip1098BlockNumber = 1))
        .copy(
          treasuryAddress = treasuryAccount,
          customGenesisFileOpt = Some("test-genesis-treasury.json")
        )
      override lazy val miningConfig = buildMiningConfig()
    }

    {
      import validator._

      blockExecution.executeAndValidateBlock(block.block, alreadyValidated = true) shouldBe
        Left(
          ValidationAfterExecError(
            "Block has invalid state root hash, expected 47344722e6c52a85685f9c1bb1e0fe66cfaf6be00c1a752f43cc835fb7415e81 but got 41b63e59d34b5b35a040d496582fc587887af79f762210f6cf55c24d2c307d61"
          )
        )
    }
  }

  it should "generate a failure if treasury transfer was made to a different treasury account" in {
    val producer = new TestSetup {
      override lazy val blockchainConfig = baseBlockchainConfig
        .withUpdatedForkBlocks(_.copy(ecip1098BlockNumber = 1))
        .copy(
          treasuryAddress = maliciousAccount,
          customGenesisFileOpt = Some("test-genesis-treasury.json")
        )
      override lazy val miningConfig = buildMiningConfig()
    }
    val block = {
      import producer._
      blockGenerator
        .generateBlock(bestBlock.get, Seq.empty, Address(testAddress), blockGenerator.emptyX, None)
        .pendingBlock
    }

    val validator = new TestSetup {
      override lazy val blockchainConfig = baseBlockchainConfig
        .withUpdatedForkBlocks(_.copy(ecip1098BlockNumber = 1))
        .copy(
          treasuryAddress = treasuryAccount,
          customGenesisFileOpt = Some("test-genesis-treasury.json")
        )
      override lazy val miningConfig = buildMiningConfig()
    }

    {
      import validator._
      blockExecution.executeAndValidateBlock(block.block, alreadyValidated = true) shouldBe
        Left(
          ValidationAfterExecError(
            "Block has invalid state root hash, expected 5bfc811dfee1fecaefbaef2dba502082a8cc72e52260368d83ed6e4ebcecae75 but got 41b63e59d34b5b35a040d496582fc587887af79f762210f6cf55c24d2c307d61"
          )
        )
    }
  }

  trait TestSetup extends EphemBlockchainTestSetup {

    val testAddress = 42
    val privateKey: BigInt = BigInt(1, Hex.decode("f3202185c84325302d43887e90a2e23e7bc058d0450bb58ef2f7585765d7d48b"))
    lazy val keyPair: AsymmetricCipherKeyPair = keyPairFromPrvKey(privateKey)
    lazy val pubKey: Array[Byte] = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail
    lazy val address: Address = Address(crypto.kec256(pubKey).drop(FirstByteOfAddress))

    val txGasLimit = 21000
    val txTransfer = 9000
    val transaction: LegacyTransaction = LegacyTransaction(
      nonce = 0,
      gasPrice = 1,
      gasLimit = txGasLimit,
      receivingAddress = Address(testAddress),
      value = txTransfer,
      payload = ByteString.empty
    )

    //defined in test-genesis-treasury.json
    val treasuryAccount: Address = Address(0xeeeeee)
    val maliciousAccount: Address = Address(0x123)

    lazy val signedTransaction: SignedTransaction =
      SignedTransaction.sign(transaction, keyPair, Some(0x3d.toByte))
    lazy val duplicatedSignedTransaction: SignedTransaction =
      SignedTransaction.sign(transaction.copy(gasLimit = 2), keyPair, Some(0x3d.toByte))

    lazy val signedTransactionWithAddress: SignedTransactionWithSender =
      SignedTransactionWithSender(signedTransaction, Address(keyPair))

    val baseBlockchainConfig: BlockchainConfig = BlockchainConfig(
      forkBlockNumbers = ForkBlockNumbers(
        frontierBlockNumber = 0,
        homesteadBlockNumber = 1150000,
        eip155BlockNumber = 0,
        eip106BlockNumber = Long.MaxValue,
        byzantiumBlockNumber = Long.MaxValue,
        difficultyBombPauseBlockNumber = 3000000,
        difficultyBombContinueBlockNumber = 5000000,
        difficultyBombRemovalBlockNumber = 5900000,
        constantinopleBlockNumber = Long.MaxValue,
        istanbulBlockNumber = Long.MaxValue,
        eip160BlockNumber = Long.MaxValue,
        eip150BlockNumber = Long.MaxValue,
        eip161BlockNumber = Long.MaxValue,
        atlantisBlockNumber = Long.MaxValue,
        aghartaBlockNumber = Long.MaxValue,
        phoenixBlockNumber = Long.MaxValue,
        petersburgBlockNumber = Long.MaxValue,
        ecip1098BlockNumber = Long.MaxValue,
        ecip1097BlockNumber = Long.MaxValue,
        ecip1099BlockNumber = Long.MaxValue,
        ecip1049BlockNumber = None
      ),
      chainId = 0x3d.toByte,
      networkId = 1,
      customGenesisFileOpt = Some("test-genesis.json"),
      customGenesisJsonOpt = None,
      monetaryPolicyConfig =
        MonetaryPolicyConfig(5000000, 0.2, 5000000000000000000L, 3000000000000000000L, 2000000000000000000L),
      // unused
      maxCodeSize = None,
      accountStartNonce = UInt256.Zero,
      daoForkConfig = None,
      bootstrapNodes = Set(),
      gasTieBreaker = false,
      ethCompatibleStorage = true,
      treasuryAddress = Address(0)
    )
    implicit override lazy val blockchainConfig: BlockchainConfig = baseBlockchainConfig

    val genesisDataLoader =
      new GenesisDataLoader(
        blockchainReader,
        blockchainWriter,
        storagesInstance.storages.stateStorage
      )
    genesisDataLoader.loadGenesisData()

    val bestBlock: Option[Block] = blockchainReader.getBestBlock()

    lazy val blockTimestampProvider = new FakeBlockTimestampProvider

    val blockCacheSize: Int = 30
    val headerExtraData: ByteString = ByteString("mined with etc scala")

    override lazy val validators: ValidatorsExecutor = powValidators

    override lazy val miningConfig: MiningConfig =
      buildMiningConfig().copy(headerExtraData = headerExtraData, blockCacheSize = blockCacheSize)

    lazy val blockGenerator: TestBlockGenerator =
      mining.blockGenerator.withBlockTimestampProvider(blockTimestampProvider)

    lazy val blockValidation =
      new BlockValidation(mining, blockchainReader, BlockQueue(blockchain, blockchainReader, syncConfig))
    lazy val blockExecution =
      new BlockExecution(
        blockchain,
        blockchainReader,
        blockchainWriter,
        storagesInstance.storages.evmCodeStorage,
        mining.blockPreparator,
        blockValidation
      )

    // FIXME: the change in gas limit voting strategy caused the hardcoded nonce and mixHash in this file to be invalid
    //        The gas limit of all the generated blocks has to be set to the old strategy of increasing as much as possible
    //        the gas limit, if not PoW validations will fail
    val generatedBlockGasLimit = 16733003
  }
}

class FakeBlockTimestampProvider extends BlockTimestampProvider {
  private var timestamp = Instant.now.getEpochSecond

  def advance(seconds: Long): Unit = timestamp += seconds

  override def getEpochSecond: Long = timestamp
}
