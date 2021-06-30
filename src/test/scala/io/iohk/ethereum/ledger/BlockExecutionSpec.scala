package io.iohk.ethereum.ledger

import akka.util.ByteString

import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3
import org.scalatest.prop.TableFor4
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.BlockHelpers
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.Mocks.MockValidatorsFailOnSpecificBlockNumber
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.consensus.TestConsensus
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.consensus.pow.validators.OmmersValidator
import io.iohk.ethereum.consensus.validators.BlockHeaderValidator
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockRewardCalculatorOps._
import io.iohk.ethereum.utils.ByteStringUtils._
import io.iohk.ethereum.utils.Hex
import io.iohk.ethereum.vm.OutOfGas

// scalastyle:off magic.number
class BlockExecutionSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  "BlockExecution" should {

    "correctly run executeBlocks" when {

      "two blocks with txs (that first one has invalid tx)" in new BlockchainSetup {
        val invalidStx = SignedTransaction(validTx, ECDSASignature(1, 2, 3.toByte))
        val block1BodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(invalidStx))
        val block1 = Block(validBlockHeader, block1BodyWithTxs)
        val block2BodyWithTxs: BlockBody =
          validBlockBodyWithNoTxs.copy(transactionList = Seq(validStxSignedByOrigin.tx))
        val block2 = Block(
          validBlockHeader.copy(parentHash = validBlockHeader.hash, number = validBlockHeader.number + 1),
          block2BodyWithTxs
        )

        override lazy val vm = new MockVM(c =>
          createResult(
            context = c,
            gasUsed = UInt256(defaultGasLimit),
            gasLimit = UInt256(defaultGasLimit),
            gasRefund = UInt256.Zero,
            logs = defaultLogs,
            addressesToDelete = defaultAddressesToDelete
          )
        )

        val mockValidators = new MockValidatorsFailOnSpecificBlockNumber(block1.header.number)
        val newConsensus: TestConsensus = consensus.withVM(vm).withValidators(mockValidators)
        val blockValidation =
          new BlockValidation(newConsensus, blockchainReader, BlockQueue(blockchain, syncConfig))
        val blockExecution =
          new BlockExecution(
            blockchain,
            blockchainReader,
            blockchainWriter,
            blockchainStorages.evmCodeStorage,
            blockchainConfig,
            newConsensus.blockPreparator,
            blockValidation
          )

        val (blocks, error) = blockExecution.executeAndValidateBlocks(List(block1, block2), defaultChainWeight)

        // No block should be executed if first one has invalid transactions
        blocks.isEmpty shouldBe true
        error.isDefined shouldBe true
      }

      "two blocks with txs (that last one has invalid tx)" in new BlockchainSetup {
        val invalidStx = SignedTransaction(validTx, ECDSASignature(1, 2, 3.toByte))
        val block1BodyWithTxs: BlockBody =
          validBlockBodyWithNoTxs.copy(transactionList = Seq(validStxSignedByOrigin.tx))
        val block1 = Block(validBlockHeader, block1BodyWithTxs)
        val block2BodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(invalidStx))
        val block2 = Block(
          validBlockHeader.copy(parentHash = validBlockHeader.hash, number = validBlockHeader.number + 1),
          block2BodyWithTxs
        )

        val mockVm = new MockVM(c =>
          createResult(
            context = c,
            gasUsed = UInt256(0),
            gasLimit = UInt256(defaultGasLimit),
            gasRefund = UInt256.Zero,
            logs = defaultLogs,
            addressesToDelete = defaultAddressesToDelete
          )
        )
        val mockValidators = new MockValidatorsFailOnSpecificBlockNumber(block2.header.number)
        val newConsensus: TestConsensus = consensus.withVM(mockVm).withValidators(mockValidators)
        val blockValidation =
          new BlockValidation(newConsensus, blockchainReader, BlockQueue(blockchain, syncConfig))
        val blockExecution =
          new BlockExecution(
            blockchain,
            blockchainReader,
            blockchainWriter,
            blockchainStorages.evmCodeStorage,
            blockchainConfig,
            newConsensus.blockPreparator,
            blockValidation
          )

        val (blocks, error) = blockExecution.executeAndValidateBlocks(List(block1, block2), defaultChainWeight)

        // Only first block should be executed
        blocks.size shouldBe 1
        blocks.head.block shouldBe block1
        error.isDefined shouldBe true
      }

      "executing a long branch where the last block is invalid" in new BlockchainSetup {
        val chain = BlockHelpers.generateChain(10, validBlockParentBlock)

        val mockVm = new MockVM(c =>
          createResult(
            context = c,
            gasUsed = UInt256(0),
            gasLimit = UInt256(defaultGasLimit),
            gasRefund = UInt256.Zero,
            logs = defaultLogs,
            addressesToDelete = defaultAddressesToDelete
          )
        )
        val mockValidators = new MockValidatorsFailOnSpecificBlockNumber(chain.last.number)
        val newConsensus: TestConsensus = consensus.withVM(mockVm).withValidators(mockValidators)
        val blockValidation =
          new BlockValidation(newConsensus, blockchainReader, BlockQueue(blockchain, syncConfig))
        val blockExecution =
          new BlockExecution(
            blockchain,
            blockchainReader,
            blockchainWriter,
            blockchainStorages.evmCodeStorage,
            blockchainConfig,
            newConsensus.blockPreparator,
            blockValidation
          )

        val (blocks, error) = blockExecution.executeAndValidateBlocks(chain, defaultChainWeight)

        // All blocks but the last should be executed, and they should be returned in incremental order
        blocks.map(_.block) shouldBe chain.init
        error.isDefined shouldBe true
      }

      "block with checkpoint and without txs" in new BlockchainSetup {
        val checkpoint = ObjectGenerators.fakeCheckpointGen(2, 5).sample.get
        val blockWithCheckpoint =
          new CheckpointBlockGenerator().generate(Block(validBlockParentHeader, validBlockBodyWithNoTxs), checkpoint)
        val treasuryAccountBefore = blockchain.getAccount(blockchainConfig.treasuryAddress, blockWithCheckpoint.number)

        val mockValidators = MockValidatorsAlwaysSucceed
        val newConsensus: TestConsensus = consensus.withVM(vm).withValidators(mockValidators)
        val blockValidation =
          new BlockValidation(newConsensus, blockchainReader, BlockQueue(blockchain, syncConfig))
        val blockExecution =
          new BlockExecution(
            blockchain,
            blockchainReader,
            blockchainWriter,
            blockchainStorages.evmCodeStorage,
            blockchainConfig,
            newConsensus.blockPreparator,
            blockValidation
          )

        val (blocks, error) =
          blockExecution.executeAndValidateBlocks(List(blockWithCheckpoint), defaultChainWeight)
        val beneficiaryAccount =
          blockchain.getAccount(Address(blockWithCheckpoint.header.beneficiary), blockWithCheckpoint.number)
        val treasuryAccountAfter = blockchain.getAccount(blockchainConfig.treasuryAddress, blockWithCheckpoint.number)

        beneficiaryAccount.isDefined shouldBe false
        treasuryAccountBefore shouldBe treasuryAccountAfter
        blocks.size shouldBe 1
        blocks.head.block shouldBe blockWithCheckpoint
        error.isDefined shouldBe false
      }
    }

    "correctly run executeBlockTransactions" when {
      "block without txs" in new BlockExecutionTestSetup {
        val block = Block(validBlockHeader, validBlockBodyWithNoTxs)

        val txsExecResult: Either[BlockExecutionError, BlockResult] =
          blockExecution.executeBlockTransactions(block, initialWorld)

        txsExecResult.isRight shouldBe true

        val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.toOption.get
        resultingGasUsed shouldBe 0
        resultingReceipts shouldBe Nil
      }

      "block with one tx (that produces OutOfGas)" in new BlockchainSetup {

        val blockBodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(validStxSignedByOrigin.tx))
        val block = Block(validBlockHeader, blockBodyWithTxs)

        val mockVm = new MockVM(c =>
          createResult(
            context = c,
            gasUsed = UInt256(defaultGasLimit),
            gasLimit = UInt256(defaultGasLimit),
            gasRefund = UInt256.Zero,
            logs = defaultLogs,
            addressesToDelete = defaultAddressesToDelete,
            error = Some(OutOfGas)
          )
        )

        val newConsensus: TestConsensus = consensus.withVM(mockVm)

        val blockValidation =
          new BlockValidation(newConsensus, blockchainReader, BlockQueue(blockchain, syncConfig))
        val blockExecution =
          new BlockExecution(
            blockchain,
            blockchainReader,
            blockchainWriter,
            blockchainStorages.evmCodeStorage,
            blockchainConfig,
            newConsensus.blockPreparator,
            blockValidation
          )

        val txsExecResult: Either[BlockExecutionError, BlockResult] =
          blockExecution.executeBlockTransactions(block, initialWorld)

        txsExecResult.isRight shouldBe true
        val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.toOption.get

        val transaction: Transaction = validStxSignedByOrigin.tx.tx
        // Check valid world
        val minerPaymentForTxs = UInt256(transaction.gasLimit * transaction.gasPrice)
        val changes = Seq(
          originAddress -> IncreaseNonce,
          originAddress -> UpdateBalance(-minerPaymentForTxs), // Origin payment for tx execution and nonce increase
          minerAddress -> UpdateBalance(minerPaymentForTxs) // Miner reward for tx execution
        )
        val expectedStateRoot: ByteString = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)
        expectedStateRoot shouldBe InMemoryWorldStateProxy.persistState(resultingWorldState).stateRootHash

        // Check valid gasUsed
        resultingGasUsed shouldBe transaction.gasLimit

        // Check valid receipts
        resultingReceipts.size shouldBe 1
        val Receipt(rootHashReceipt, gasUsedReceipt, logsBloomFilterReceipt, logsReceipt) = resultingReceipts.head
        rootHashReceipt shouldBe HashOutcome(expectedStateRoot)
        gasUsedReceipt shouldBe resultingGasUsed
        logsBloomFilterReceipt shouldBe BloomFilter.create(Nil)
        logsReceipt shouldBe Nil
      }

      "block with one tx (that produces no errors)" in new BlockchainSetup {

        val table: TableFor4[BigInt, Seq[TxLogEntry], Set[Address], Boolean] =
          Table[BigInt, Seq[TxLogEntry], Set[Address], Boolean](
            ("gasLimit/gasUsed", "logs", "addressesToDelete", "txValidAccordingToValidators"),
            (defaultGasLimit, Nil, Set.empty, true),
            (defaultGasLimit / 2, Nil, defaultAddressesToDelete, true),
            (2 * defaultGasLimit, defaultLogs, Set.empty, true),
            (defaultGasLimit, defaultLogs, defaultAddressesToDelete, true),
            (defaultGasLimit, defaultLogs, defaultAddressesToDelete, false)
          )

        forAll(table) { (gasLimit, logs, addressesToDelete, txValidAccordingToValidators) =>
          val tx = validTx.copy(gasLimit = gasLimit)
          val stx = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))

          val blockHeader: BlockHeader = validBlockHeader.copy(gasLimit = gasLimit)
          val blockBodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(stx.tx))
          val block = Block(blockHeader, blockBodyWithTxs)

          val mockValidators =
            if (txValidAccordingToValidators) Mocks.MockValidatorsAlwaysSucceed else Mocks.MockValidatorsAlwaysFail
          val mockVm = new MockVM(c =>
            createResult(
              context = c,
              gasUsed = UInt256(gasLimit),
              gasLimit = UInt256(gasLimit),
              gasRefund = UInt256.Zero,
              logs = logs,
              addressesToDelete = addressesToDelete
            )
          )

          val newConsensus = consensus.withValidators(mockValidators).withVM(mockVm)
          val blockValidation =
            new BlockValidation(newConsensus, blockchainReader, BlockQueue(blockchain, syncConfig))
          val blockExecution =
            new BlockExecution(
              blockchain,
              blockchainReader,
              blockchainWriter,
              blockchainStorages.evmCodeStorage,
              blockchainConfig,
              newConsensus.blockPreparator,
              blockValidation
            )

          val txsExecResult = blockExecution.executeBlockTransactions(block, initialWorld)

          txsExecResult.isRight shouldBe txValidAccordingToValidators
          if (txsExecResult.isRight) {
            val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.toOption.get

            val transaction = stx.tx.tx
            // Check valid world
            val minerPaymentForTxs = UInt256(transaction.gasLimit * transaction.gasPrice)
            val changes = Seq(
              originAddress -> IncreaseNonce,
              originAddress -> UpdateBalance(-minerPaymentForTxs), // Origin payment for tx execution and nonce increase
              minerAddress -> UpdateBalance(minerPaymentForTxs) // Miner reward for tx execution
            ) ++ addressesToDelete.map(address => address -> DeleteAccount) // Delete all accounts to be deleted
            val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)
            expectedStateRoot shouldBe InMemoryWorldStateProxy.persistState(resultingWorldState).stateRootHash

            // Check valid gasUsed
            resultingGasUsed shouldBe stx.tx.tx.gasLimit

            // Check valid receipts
            resultingReceipts.size shouldBe 1
            val Receipt(rootHashReceipt, gasUsedReceipt, logsBloomFilterReceipt, logsReceipt) = resultingReceipts.head
            rootHashReceipt shouldBe HashOutcome(expectedStateRoot)
            gasUsedReceipt shouldBe resultingGasUsed
            logsBloomFilterReceipt shouldBe BloomFilter.create(logs)
            logsReceipt shouldBe logs
          }
        }
      }

      "last one wasn't executed correctly" in new BlockExecutionTestSetup {
        val invalidStx = SignedTransaction(validTx, ECDSASignature(1, 2, 3.toByte))
        val blockBodyWithTxs: BlockBody =
          validBlockBodyWithNoTxs.copy(transactionList = Seq(validStxSignedByOrigin.tx, invalidStx))
        val block = Block(validBlockHeader, blockBodyWithTxs)

        val txsExecResult: Either[BlockExecutionError, BlockResult] =
          blockExecution.executeBlockTransactions(block, initialWorld)

        txsExecResult.isLeft shouldBe true
      }

      "first one wasn't executed correctly" in new BlockExecutionTestSetup {
        val invalidStx = SignedTransaction(validTx, ECDSASignature(1, 2, 3.toByte))
        val blockBodyWithTxs: BlockBody =
          validBlockBodyWithNoTxs.copy(transactionList = Seq(invalidStx, validStxSignedByOrigin.tx))
        val block = Block(validBlockHeader, blockBodyWithTxs)

        val txsExecResult: Either[BlockExecutionError, BlockResult] =
          blockExecution.executeBlockTransactions(block, initialWorld)

        txsExecResult.isLeft shouldBe true
      }
    }

    // migrated from old LedgerSpec

    "correctly run executeBlock for a valid block without txs" in new BlockchainSetup {

      val table: TableFor2[Int, BigInt] = Table[Int, BigInt](
        ("ommersSize", "ommersBlockDifference"),
        (0, 0),
        (2, 5),
        (1, 3)
      )

      override lazy val vm: VMImpl = new MockVM(c =>
        createResult(
          context = c,
          gasUsed = UInt256(defaultGasLimit),
          gasLimit = UInt256(defaultGasLimit),
          gasRefund = UInt256.Zero,
          logs = defaultLogs,
          addressesToDelete = defaultAddressesToDelete,
          error = Some(OutOfGas)
        )
      )

      forAll(table) { (ommersSize, ommersBlockDifference) =>
        val ommersAddresses = (0 until ommersSize).map(i => Address(i.toByte +: Hex.decode("10")))

        val blockReward =
          consensus.blockPreparator.blockRewardCalculator.calculateMiningReward(validBlockHeader.number, ommersSize)

        val changes = Seq(
          minerAddress -> UpdateBalance(UInt256(blockReward))
        ) ++ ommersAddresses.map { ommerAddress =>
          val ommerReward = consensus.blockPreparator.blockRewardCalculator.calculateOmmerRewardForInclusion(
            validBlockHeader.number,
            validBlockHeader.number - ommersBlockDifference
          )
          ommerAddress -> UpdateBalance(UInt256(ommerReward))
        }

        val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)

        val blockHeader: BlockHeader = validBlockHeader.copy(stateRoot = expectedStateRoot)
        val blockBodyWithOmmers = validBlockBodyWithNoTxs.copy(
          uncleNodesList = ommersAddresses.map(ommerAddress =>
            defaultBlockHeader.copy(
              number = blockHeader.number - ommersBlockDifference,
              beneficiary = ommerAddress.bytes
            )
          )
        )
        val block = Block(blockHeader, blockBodyWithOmmers)

        val blockExecResult = blockImport.blockExecution.executeAndValidateBlock(block)
        assert(blockExecResult.isRight)
      }
    }

    "fail to run executeBlock if a block is invalid before executing it" in new BlockchainSetup {
      object validatorsOnlyFailsBlockValidator extends Mocks.MockValidatorsAlwaysSucceed {
        override val blockValidator: BlockValidator = Mocks.MockValidatorsAlwaysFail.blockValidator
      }

      object validatorsOnlyFailsBlockHeaderValidator extends Mocks.MockValidatorsAlwaysSucceed {
        override val blockHeaderValidator: BlockHeaderValidator = Mocks.MockValidatorsAlwaysFail.blockHeaderValidator
      }

      object validatorsOnlyFailsOmmersValidator extends Mocks.MockValidatorsAlwaysSucceed {
        override val ommersValidator: OmmersValidator = Mocks.MockValidatorsAlwaysFail.ommersValidator
      }

      val seqFailingValidators = Seq(
        validatorsOnlyFailsBlockHeaderValidator,
        validatorsOnlyFailsBlockValidator,
        validatorsOnlyFailsOmmersValidator
      )

      override lazy val vm: VMImpl = new MockVM(c =>
        createResult(
          context = c,
          gasUsed = UInt256(defaultGasLimit),
          gasLimit = UInt256(defaultGasLimit),
          gasRefund = UInt256.Zero,
          logs = defaultLogs,
          addressesToDelete = defaultAddressesToDelete,
          error = Some(OutOfGas)
        )
      )

      val blockReward: BigInt =
        consensus.blockPreparator.blockRewardCalculator.calculateMiningReward(validBlockHeader.number, 0)

      val changes = Seq(
        minerAddress -> UpdateBalance(UInt256(blockReward)) // Paying miner for block processing
      )
      val expectedStateRoot: ByteString = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)
      val blockHeader: BlockHeader = validBlockHeader.copy(stateRoot = expectedStateRoot)
      val block = Block(blockHeader, validBlockBodyWithNoTxs)

      assert(seqFailingValidators.forall { validators =>
        val blockExecResult = blockImport.blockExecution.executeAndValidateBlock(block)

        blockExecResult.left.forall {
          case e: BlockExecutionError.ValidationBeforeExecError => true
          case _                                                => false
        }
      })
    }

    "fail to run executeBlock if a block is invalid after executing it" in new BlockchainSetup {

      object validatorsFailsBlockValidatorWithReceipts extends Mocks.MockValidatorsAlwaysSucceed {
        override val blockValidator: BlockValidator = new BlockValidator {
          override def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody) =
            Right(StdBlockValidator.BlockValid)
          override def validateBlockAndReceipts(blockHeader: BlockHeader, receipts: Seq[Receipt]) =
            Left(StdBlockValidator.BlockTransactionsHashError)
        }
      }

      override lazy val vm: VMImpl = new MockVM(c =>
        createResult(
          context = c,
          gasUsed = UInt256(defaultGasLimit),
          gasLimit = UInt256(defaultGasLimit),
          gasRefund = UInt256.Zero,
          logs = defaultLogs,
          addressesToDelete = defaultAddressesToDelete,
          error = Some(OutOfGas)
        )
      )

      val blockReward: BigInt =
        consensus.blockPreparator.blockRewardCalculator.calculateMiningReward(validBlockHeader.number, 0)

      val changes = Seq(minerAddress -> UpdateBalance(UInt256(blockReward))) //Paying miner for block processing
      val correctStateRoot: ByteString = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)

      val correctGasUsed: BigInt = 0
      val incorrectStateRoot: ByteString =
        concatByteStrings(((correctStateRoot.head + 1) & 0xff).toByte, correctStateRoot.tail)
      val table: TableFor3[ByteString, BigInt, Validators] = Table[ByteString, BigInt, Validators](
        ("stateRootHash", "cumulativeGasUsedBlock", "validators"),
        (correctStateRoot, correctGasUsed + 1, new Mocks.MockValidatorsAlwaysSucceed),
        (incorrectStateRoot, correctGasUsed, new Mocks.MockValidatorsAlwaysSucceed),
        (correctStateRoot, correctGasUsed, validatorsFailsBlockValidatorWithReceipts)
      )

      forAll(table) { (stateRootHash, cumulativeGasUsedBlock, validators) =>
        val blockImport = mkBlockImport(validators = validators)
        val blockHeader: BlockHeader =
          validBlockHeader.copy(gasUsed = cumulativeGasUsedBlock, stateRoot = stateRootHash)
        val block = Block(blockHeader, validBlockBodyWithNoTxs)

        val blockExecResult = blockImport.blockExecution.executeAndValidateBlock(block)

        assert(blockExecResult match {
          case Left(_: BlockExecutionError.ValidationAfterExecError) => true
          case _                                                     => false
        })
      }
    }

    "correctly run a block with more than one tx" in new BlockchainSetup {
      val table: TableFor4[Address, Address, Address, Address] = Table[Address, Address, Address, Address](
        ("origin1Address", "receiver1Address", "origin2Address", "receiver2Address"),
        (originAddress, minerAddress, receiverAddress, minerAddress),
        (originAddress, receiverAddress, receiverAddress, originAddress),
        (originAddress, receiverAddress, originAddress, minerAddress),
        (originAddress, originAddress, originAddress, originAddress)
      )

      override lazy val vm: VMImpl = new MockVM(c =>
        createResult(
          context = c,
          gasUsed = UInt256(defaultGasLimit),
          gasLimit = UInt256(defaultGasLimit),
          gasRefund = UInt256.Zero
        )
      )

      forAll(table) { (origin1Address, receiver1Address, origin2Address, receiver2Address) =>
        def keyPair(address: Address): AsymmetricCipherKeyPair =
          if (address == originAddress) originKeyPair else receiverKeyPair

        val tx1 = validTx.copy(value = 100, receivingAddress = Some(receiver1Address), gasLimit = defaultGasLimit)
        val tx2 = validTx.copy(
          value = 50,
          receivingAddress = Some(receiver2Address),
          gasLimit = defaultGasLimit * 2,
          nonce = validTx.nonce + (if (origin1Address == origin2Address) 1 else 0)
        )
        val stx1 = SignedTransaction.sign(tx1, keyPair(origin1Address), Some(blockchainConfig.chainId))
        val stx2 = SignedTransaction.sign(tx2, keyPair(origin2Address), Some(blockchainConfig.chainId))

        val validBlockBodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(stx1.tx, stx2.tx))
        val block = Block(validBlockHeader, validBlockBodyWithTxs)

        val txsExecResult = blockImport.blockExecution.executeBlockTransactions(block, initialWorld)

        assert(txsExecResult.isRight)
        val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.toOption.get
        val transaction1 = stx1.tx.tx
        val transaction2 = stx2.tx.tx
        // Check valid gasUsed
        resultingGasUsed shouldBe transaction1.gasLimit + transaction2.gasLimit

        // Check valid receipts
        resultingReceipts.size shouldBe 2
        val Seq(receipt1, receipt2) = resultingReceipts

        // Check receipt1
        val minerPaymentForTx1 = UInt256(transaction1.gasLimit * transaction1.gasPrice)
        val changesTx1 = Seq(
          origin1Address -> IncreaseNonce,
          origin1Address -> UpdateBalance(-minerPaymentForTx1), // Origin payment for tx execution and nonce increase
          minerAddress -> UpdateBalance(minerPaymentForTx1) // Miner reward for tx execution
        )
        val expectedStateRootTx1 = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changesTx1)

        val Receipt(rootHashReceipt1, gasUsedReceipt1, logsBloomFilterReceipt1, logsReceipt1) = receipt1
        rootHashReceipt1 shouldBe HashOutcome(expectedStateRootTx1)
        gasUsedReceipt1 shouldBe stx1.tx.tx.gasLimit
        logsBloomFilterReceipt1 shouldBe BloomFilter.create(Nil)
        logsReceipt1 shouldBe Nil

        // Check receipt2
        val minerPaymentForTx2 = UInt256(transaction2.gasLimit * transaction2.gasPrice)
        val changesTx2 = Seq(
          origin2Address -> IncreaseNonce,
          origin2Address -> UpdateBalance(-minerPaymentForTx2), // Origin payment for tx execution and nonce increase
          minerAddress -> UpdateBalance(minerPaymentForTx2) // Miner reward for tx execution
        )
        val expectedStateRootTx2 = applyChanges(expectedStateRootTx1, blockchainStorages, changesTx2)

        val Receipt(rootHashReceipt2, gasUsedReceipt2, logsBloomFilterReceipt2, logsReceipt2) = receipt2
        rootHashReceipt2 shouldBe HashOutcome(expectedStateRootTx2)
        gasUsedReceipt2 shouldBe (transaction1.gasLimit + transaction2.gasLimit)
        logsBloomFilterReceipt2 shouldBe BloomFilter.create(Nil)
        logsReceipt2 shouldBe Nil

        // Check world
        InMemoryWorldStateProxy.persistState(resultingWorldState).stateRootHash shouldBe expectedStateRootTx2

        val blockReward: BigInt =
          consensus.blockPreparator.blockRewardCalculator.calculateMiningReward(block.header.number, 0)
        val changes = Seq(
          minerAddress -> UpdateBalance(UInt256(blockReward))
        )
        val blockExpectedStateRoot = applyChanges(expectedStateRootTx2, blockchainStorages, changes)

        val blockWithCorrectStateAndGasUsed = block.copy(
          header = block.header.copy(stateRoot = blockExpectedStateRoot, gasUsed = gasUsedReceipt2)
        )
        assert(blockImport.blockExecution.executeAndValidateBlock(blockWithCorrectStateAndGasUsed).isRight)
      }
    }

    "drain DAO accounts and send the funds to refund address if Pro DAO Fork was configured" in new DaoForkTestSetup {

      (worldState.getAccount _)
        .expects(supportDaoForkConfig.refundContract.get)
        .anyNumberOfTimes()
        .returning(Some(Account(nonce = 1, balance = UInt256.Zero)))

      // Check we drain all the accounts and send the balance to refund contract
      supportDaoForkConfig.drainList.foreach { addr =>
        val daoAccountsFakeBalance = UInt256(1000)
        (worldState.getAccount _).expects(addr).returning(Some(Account(nonce = 1, balance = daoAccountsFakeBalance)))
        (worldState.transfer _)
          .expects(addr, supportDaoForkConfig.refundContract.get, daoAccountsFakeBalance)
          .returning(worldState)
      }

      // We don't care about block txs in this test
      blockImport.blockExecution.executeBlockTransactions(
        proDaoBlock.copy(body = proDaoBlock.body.copy(transactionList = Seq.empty)),
        initialWorld
      )
    }

    "neither drain DAO accounts nor send the funds to refund address if Pro DAO Fork was not configured" in new DaoForkTestSetup {
      // Check we drain all the accounts and send the balance to refund contract
      supportDaoForkConfig.drainList.foreach { _ =>
        (worldState.transfer _).expects(*, *, *).never()
      }

      // We don't care about block txs in this test
      blockImport.blockExecution.executeBlockTransactions(
        proDaoBlock.copy(body = proDaoBlock.body.copy(transactionList = Seq.empty)),
        initialWorld
      )
    }
  }

  trait BlockExecutionTestSetup extends BlockchainSetup {

    val blockValidation =
      new BlockValidation(consensus, blockchainReader, BlockQueue(blockchain, syncConfig))
    val blockExecution =
      new BlockExecution(
        blockchain,
        blockchainReader,
        blockchainWriter,
        blockchainStorages.evmCodeStorage,
        blockchainConfig,
        consensus.blockPreparator,
        blockValidation
      )

  }
}
