package io.iohk.ethereum.ledger

import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.consensus.ethash.validators.OmmersValidator
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.{BlockTransactionsHashError, BlockValid}
import io.iohk.ethereum.consensus.validators.{Validators, _}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.{ValidationAfterExecError, ValidationBeforeExecError}
import io.iohk.ethereum.ledger.Ledger.{BlockResult, VMImpl}
import io.iohk.ethereum.ledger.BlockRewardCalculatorOps._
import io.iohk.ethereum.vm.{OutOfGas, RevertOccurs}
import monix.execution.Scheduler
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.util.encoders.Hex
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.{TableFor2, TableFor3, TableFor4}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.iohk.ethereum.utils.ByteStringUtils._

// scalastyle:off magic.number
class LedgerSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers with ScalaFutures {

  implicit val testContext: Scheduler = Scheduler.fixedPool("ledger-spec", 4)

  "Ledger" should "correctly run executeBlock for a valid block without txs" in new BlockchainSetup {

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

      val blockReward = ledger.blockRewardCalculator.calculateMiningReward(validBlockHeader.number, ommersSize)

      val changes = Seq(
        minerAddress -> UpdateBalance(UInt256(blockReward))
      ) ++ ommersAddresses.map { ommerAddress =>
        val ommerReward = ledger.blockRewardCalculator.calculateOmmerRewardForInclusion(
          validBlockHeader.number,
          validBlockHeader.number - ommersBlockDifference
        )
        ommerAddress -> UpdateBalance(UInt256(ommerReward))
      }

      val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)

      val blockHeader: BlockHeader = validBlockHeader.copy(stateRoot = expectedStateRoot)
      val blockBodyWithOmmers = validBlockBodyWithNoTxs.copy(
        uncleNodesList = ommersAddresses.map(ommerAddress =>
          defaultBlockHeader.copy(number = blockHeader.number - ommersBlockDifference, beneficiary = ommerAddress.bytes)
        )
      )
      val block = Block(blockHeader, blockBodyWithOmmers)

      val blockExecResult = ledger.blockExecution.executeAndValidateBlock(block)
      assert(blockExecResult.isRight)
    }
  }

  it should "fail to run executeBlock if a block is invalid before executing it" in new BlockchainSetup {
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

    val blockReward: BigInt = ledger.blockRewardCalculator.calculateMiningReward(validBlockHeader.number, 0)

    val changes = Seq(
      minerAddress -> UpdateBalance(UInt256(blockReward)) // Paying miner for block processing
    )
    val expectedStateRoot: ByteString = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)
    val blockHeader: BlockHeader = validBlockHeader.copy(stateRoot = expectedStateRoot)
    val block = Block(blockHeader, validBlockBodyWithNoTxs)

    assert(seqFailingValidators.forall { validators =>
      val ledger = newTestLedger(validators = validators)
      val blockExecResult = ledger.blockExecution.executeAndValidateBlock(block)

      blockExecResult.left.forall {
        case e: ValidationBeforeExecError => true
        case _ => false
      }
    })
  }

  it should "fail to run executeBlock if a block is invalid after executing it" in new BlockchainSetup {

    object validatorsFailsBlockValidatorWithReceipts extends Mocks.MockValidatorsAlwaysSucceed {
      override val blockValidator: BlockValidator = new BlockValidator {
        override def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody) = Right(BlockValid)
        override def validateBlockAndReceipts(blockHeader: BlockHeader, receipts: Seq[Receipt]) =
          Left(BlockTransactionsHashError)
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

    val blockReward: BigInt = ledger.blockRewardCalculator.calculateMiningReward(validBlockHeader.number, 0)

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
      val ledger = newTestLedger(validators = validators)

      val blockHeader: BlockHeader = validBlockHeader.copy(gasUsed = cumulativeGasUsedBlock, stateRoot = stateRootHash)
      val block = Block(blockHeader, validBlockBodyWithNoTxs)

      val blockExecResult = ledger.blockExecution.executeAndValidateBlock(block)

      assert(blockExecResult match {
        case Left(_: ValidationAfterExecError) => true
        case _ => false
      })
    }
  }

  it should "correctly run a block with more than one tx" in new BlockchainSetup {
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

      val txsExecResult = ledger.blockExecution.executeBlockTransactions(block, validBlockParentHeader)

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

      val blockReward: BigInt = ledger.blockRewardCalculator.calculateMiningReward(block.header.number, 0)
      val changes = Seq(
        minerAddress -> UpdateBalance(UInt256(blockReward))
      )
      val blockExpectedStateRoot = applyChanges(expectedStateRootTx2, blockchainStorages, changes)

      val blockWithCorrectStateAndGasUsed = block.copy(
        header = block.header.copy(stateRoot = blockExpectedStateRoot, gasUsed = gasUsedReceipt2)
      )
      assert(ledger.blockExecution.executeAndValidateBlock(blockWithCorrectStateAndGasUsed).isRight)
    }
  }

  it should "drain DAO accounts and send the funds to refund address if Pro DAO Fork was configured" in new DaoForkTestSetup {

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

    override lazy val ledger: LedgerImpl =
      newTestLedger(blockchain = testBlockchain, blockchainConfig = proDaoBlockchainConfig)

    // We don't care about block txs in this test
    ledger.blockExecution.executeBlockTransactions(
      proDaoBlock.copy(body = proDaoBlock.body.copy(transactionList = Seq.empty)),
      parentBlockHeader
    )
  }

  it should "neither drain DAO accounts nor send the funds to refund address if Pro DAO Fork was not configured" in new DaoForkTestSetup {
    // Check we drain all the accounts and send the balance to refund contract
    supportDaoForkConfig.drainList.foreach { _ =>
      (worldState.transfer _).expects(*, *, *).never()
    }

    override lazy val ledger: LedgerImpl = newTestLedger(blockchain = testBlockchain)

    // We don't care about block txs in this test
    ledger.blockExecution.executeBlockTransactions(
      proDaoBlock.copy(body = proDaoBlock.body.copy(transactionList = Seq.empty)),
      parentBlockHeader
    )
  }

  it should "correctly determine current block status" in new BlockchainSetup {
    val testHash: ByteString = validBlockParentHeader.copy(number = validBlockParentHeader.number + 5).hash

    val validBlockHeaderNoParent: BlockHeader = validBlockHeader.copy(parentHash = testHash)

    override lazy val vm: VMImpl =
      new MockVM(c => createResult(context = c, gasUsed = defaultGasLimit, gasLimit = defaultGasLimit, gasRefund = 0))

    ledger.checkBlockStatus(validBlockParentHeader.hash) shouldEqual InChain

    whenReady(ledger.importBlock(Block(validBlockHeaderNoParent, validBlockBodyWithNoTxs)).runToFuture) { result =>
      result shouldEqual BlockEnqueued
    }

    ledger.checkBlockStatus(validBlockHeaderNoParent.hash) shouldEqual Queued

    ledger.checkBlockStatus(validBlockHeader.hash) shouldEqual UnknownBlock
  }

  it should "properly find minimal required gas limit to execute transaction" in new BinarySimulationChopSetup {
    testGasValues.foreach { minimumRequiredGas =>
      LedgerUtils.binaryChop[TxError](minimalGas, maximalGas)(
        mockTransaction(minimumRequiredGas)
      ) shouldEqual minimumRequiredGas
    }
  }

  it should "properly assign stateRootHash before byzantium block (exclusive)" in new TestSetup {

    val tx: Transaction = defaultTx.copy(
      gasPrice = defaultGasPrice,
      gasLimit = defaultGasLimit,
      receivingAddress = None,
      payload = ByteString.empty
    )
    val stx: SignedTransactionWithSender = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))
    val header: BlockHeader = defaultBlockHeader.copy(number = blockchainConfig.byzantiumBlockNumber - 1)

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
      consensus.blockPreparator.executeTransactions(Seq(stx.tx), initialWorld, header)

    result shouldBe a[Right[_, BlockResult]]
    result.map { br =>
      br.receipts.last.postTransactionStateHash shouldBe a[HashOutcome]
    }
  }

  it should "properly assign stateRootHash after byzantium block (inclusive) if operation is a success" in new TestSetup {

    val tx: Transaction = defaultTx.copy(
      gasPrice = defaultGasPrice,
      gasLimit = defaultGasLimit,
      receivingAddress = None,
      payload = ByteString.empty
    )
    val stx: SignedTransaction = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId)).tx
    val header: BlockHeader =
      defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = blockchainConfig.byzantiumBlockNumber)

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
      consensus.blockPreparator.executeTransactions(Seq(stx), initialWorld, header)

    result shouldBe a[Right[_, BlockResult]]
    result.map(_.receipts.last.postTransactionStateHash shouldBe SuccessOutcome)
  }

  it should "properly assign stateRootHash after byzantium block (inclusive) if operation is a failure" in new TestSetup {

    val defaultsLogs = Seq(defaultLog)

    lazy val mockVM =
      new MockVM(createResult(_, defaultGasLimit, defaultGasLimit, 0, Some(RevertOccurs), bEmpty, defaultsLogs))

    val testConsensus: Consensus = newTestConsensus(vm = mockVM)

    val tx: Transaction = defaultTx.copy(
      gasPrice = defaultGasLimit,
      gasLimit = defaultGasLimit,
      receivingAddress = None,
      payload = ByteString.empty
    )
    val stx: SignedTransactionWithSender = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))
    val header: BlockHeader =
      defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = blockchainConfig.byzantiumBlockNumber)

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
      testConsensus.blockPreparator.executeTransactions(Seq(stx.tx), initialWorld, header)

    result shouldBe a[Right[_, BlockResult]]
    result.map(_.receipts.last.postTransactionStateHash shouldBe FailureOutcome)
  }
}
