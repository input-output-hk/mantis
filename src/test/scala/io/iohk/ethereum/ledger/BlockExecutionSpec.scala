package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.Mocks.{MockVM, MockValidatorsAlwaysSucceed, MockValidatorsFailOnSpecificBlockNumber}
import io.iohk.ethereum.consensus.TestConsensus
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.Ledger.BlockResult
import io.iohk.ethereum.vm.OutOfGas
import io.iohk.ethereum.{BlockHelpers, Mocks, ObjectGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableFor4
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

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
        val blockValidation = new BlockValidation(newConsensus, blockchain, BlockQueue(blockchain, syncConfig))
        val blockExecution =
          new BlockExecution(blockchain, blockchainConfig, newConsensus.blockPreparator, blockValidation)

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
        val blockValidation = new BlockValidation(newConsensus, blockchain, BlockQueue(blockchain, syncConfig))
        val blockExecution =
          new BlockExecution(blockchain, blockchainConfig, newConsensus.blockPreparator, blockValidation)

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
        val blockValidation = new BlockValidation(newConsensus, blockchain, BlockQueue(blockchain, syncConfig))
        val blockExecution =
          new BlockExecution(blockchain, blockchainConfig, newConsensus.blockPreparator, blockValidation)

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
        val blockValidation = new BlockValidation(newConsensus, blockchain, BlockQueue(blockchain, syncConfig))
        val blockExecution =
          new BlockExecution(blockchain, blockchainConfig, newConsensus.blockPreparator, blockValidation)

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
          blockExecution.executeBlockTransactions(block, validBlockParentHeader)

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

        val blockValidation = new BlockValidation(newConsensus, blockchain, BlockQueue(blockchain, syncConfig))
        val blockExecution =
          new BlockExecution(blockchain, blockchainConfig, newConsensus.blockPreparator, blockValidation)

        val txsExecResult: Either[BlockExecutionError, BlockResult] =
          blockExecution.executeBlockTransactions(block, validBlockParentHeader)

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

          // Beware we are not using `ledger`
          val newConsensus = consensus.withValidators(mockValidators).withVM(mockVm)
          val blockValidation = new BlockValidation(newConsensus, blockchain, BlockQueue(blockchain, syncConfig))
          val blockExecution =
            new BlockExecution(blockchain, blockchainConfig, newConsensus.blockPreparator, blockValidation)

          val txsExecResult = blockExecution.executeBlockTransactions(block, validBlockParentHeader)

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
          blockExecution.executeBlockTransactions(block, validBlockParentHeader)

        txsExecResult.isLeft shouldBe true
      }

      "first one wasn't executed correctly" in new BlockExecutionTestSetup {
        val invalidStx = SignedTransaction(validTx, ECDSASignature(1, 2, 3.toByte))
        val blockBodyWithTxs: BlockBody =
          validBlockBodyWithNoTxs.copy(transactionList = Seq(invalidStx, validStxSignedByOrigin.tx))
        val block = Block(validBlockHeader, blockBodyWithTxs)

        val txsExecResult: Either[BlockExecutionError, BlockResult] =
          blockExecution.executeBlockTransactions(block, validBlockParentHeader)

        txsExecResult.isLeft shouldBe true
      }
    }
  }

  trait BlockExecutionTestSetup extends BlockchainSetup {

    val blockValidation = new BlockValidation(consensus, blockchain, BlockQueue(blockchain, syncConfig))
    val blockExecution = new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation)

  }
}
