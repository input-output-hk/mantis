package io.iohk.ethereum.ledger

import java.util.concurrent.Executors

import akka.util.ByteString
import akka.util.ByteString.{ empty => bEmpty }
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.Mocks.{ MockVM, MockValidatorsAlwaysSucceed }
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.consensus.ethash.validators.OmmersValidator
import io.iohk.ethereum.consensus.validators.SignedTransactionError.TransactionSignatureError
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.{ BlockTransactionsHashError, BlockValid }
import io.iohk.ethereum.consensus.validators.{ Validators, _ }
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.{ ValidationAfterExecError, ValidationBeforeExecError }
import io.iohk.ethereum.ledger.Ledger.{ BlockResult, VMImpl }
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.MonetaryPolicyConfig
import io.iohk.ethereum.vm._
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.{ PropertyChecks, TableFor2, TableFor3, TableFor4 }
import org.scalatest.{ FlatSpec, Matchers }

import scala.concurrent.{ ExecutionContext, ExecutionContextExecutor }

// scalastyle:off file.size.limit magic.number
class LedgerSpec extends FlatSpec with PropertyChecks with Matchers with MockFactory with ScalaFutures {

  implicit val testContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))

  "Ledger" should "correctly calculate the total gas refund to be returned to the sender and paying for gas to the miner" in new TestSetup {

    val table: TableFor4[BigInt, BigInt, Option[ProgramError], BigInt] = Table[BigInt, BigInt, Option[ProgramError], BigInt](
      ("execGasUsed", "refundsFromVM", "maybeError", "gasUsed"),
      (25000, 20000, None, 25000 - 12500),
      (25000, 10000, None, 25000 - 10000),
      (125000, 10000, Some(OutOfGas), defaultGasLimit),
      (125000, 100000, Some(OutOfGas), defaultGasLimit),
      (125000, 100000, Some(RevertOccurs), 125000)
    )

    forAll(table) { (execGasUsed, gasRefundFromVM, error, gasUsed) =>
      val balanceDelta = UInt256(gasUsed * defaultGasPrice)

      val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit)

      val stx = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))

      val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

      val mockVM = new MockVM(c => createResult(
        context = c,
        gasUsed = execGasUsed,
        gasLimit = defaultGasLimit,
        gasRefund = gasRefundFromVM,
        error = error
      ))

      // FIXME De we need successValidators?
      val testConsensus = newTestConsensus(successValidators, mockVM)

      val execResult = testConsensus.blockPreparator.executeTransaction(stx.tx, stx.senderAddress, header, worldWithMinerAndOriginAccounts)
      val postTxWorld = execResult.worldState

      execResult.gasUsed shouldEqual gasUsed
      postTxWorld.getBalance(originAddress) shouldEqual (initialOriginBalance - balanceDelta)
      postTxWorld.getBalance(minerAddress) shouldEqual (initialMinerBalance + balanceDelta)
    }
  }

  it should "correctly run executeBlock for a valid block without txs" in new BlockchainSetup {

    val table: TableFor2[Int, BigInt] = Table[Int, BigInt](
      ("ommersSize", "ommersBlockDifference"),
      (0, 0),
      (2, 5),
      (1, 3)
    )

    override lazy val vm: VMImpl = new MockVM(c => createResult(
      context = c,
      gasUsed = UInt256(defaultGasLimit),
      gasLimit = UInt256(defaultGasLimit),
      gasRefund = UInt256.Zero,
      logs = defaultLogs,
      addressesToDelete = defaultAddressesToDelete,
      error = Some(OutOfGas)
    ))

    forAll(table){ (ommersSize, ommersBlockDifference) =>
      val ommersAddresses = (0 until ommersSize).map(i => Address(i.toByte +: Hex.decode("10")))

      val blockReward = ledger.blockRewardCalculator.calcBlockMinerReward(validBlockHeader.number, ommersSize)


      val changes = Seq(
        minerAddress -> UpdateBalance(UInt256(blockReward))
      ) ++ ommersAddresses.map { ommerAddress =>
        val ommerReward = ledger.blockRewardCalculator.calcOmmerMinerReward(validBlockHeader.number, validBlockHeader.number - ommersBlockDifference)
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


      val blockExecResult = ledger.blockExecution.executeBlock(block)
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

    val seqFailingValidators = Seq(validatorsOnlyFailsBlockHeaderValidator, validatorsOnlyFailsBlockValidator, validatorsOnlyFailsOmmersValidator)

    override lazy val vm: VMImpl = new MockVM(c => createResult(
      context = c,
      gasUsed = UInt256(defaultGasLimit),
      gasLimit = UInt256(defaultGasLimit),
      gasRefund = UInt256.Zero,
      logs = defaultLogs,
      addressesToDelete = defaultAddressesToDelete,
      error = Some(OutOfGas)
    ))

    val monetaryPolicyConfig: MonetaryPolicyConfig = blockchainConfig.monetaryPolicyConfig
    val byzantiumBlockNumber: BigInt = blockchainConfig.byzantiumBlockNumber
    val blockReward: BigInt = new BlockRewardCalculator(monetaryPolicyConfig, byzantiumBlockNumber).calcBlockMinerReward(validBlockHeader.number, 0)

    val changes = Seq(
      minerAddress -> UpdateBalance(UInt256(blockReward)) // Paying miner for block processing
    )
    val expectedStateRoot: ByteString = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)
    val blockHeader: BlockHeader = validBlockHeader.copy(stateRoot = expectedStateRoot)
    val block = Block(blockHeader, validBlockBodyWithNoTxs)

    assert(seqFailingValidators.forall { validators =>
      val ledger = newTestLedger(validators = validators)
      val blockExecResult = ledger.blockExecution.executeBlock(block)

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
        override def validateBlockAndReceipts(blockHeader: BlockHeader, receipts: Seq[Receipt]) = Left(BlockTransactionsHashError)
      }
    }

    override lazy val vm: VMImpl = new MockVM(c => createResult(
      context = c,
      gasUsed = UInt256(defaultGasLimit),
      gasLimit = UInt256(defaultGasLimit),
      gasRefund = UInt256.Zero,
      logs = defaultLogs,
      addressesToDelete = defaultAddressesToDelete,
      error = Some(OutOfGas)
    ))

    val blockReward: BigInt = new BlockRewardCalculator(blockchainConfig.monetaryPolicyConfig, blockchainConfig.byzantiumBlockNumber)
      .calcBlockMinerReward(validBlockHeader.number, 0)

    val changes = Seq(minerAddress -> UpdateBalance(UInt256(blockReward))) //Paying miner for block processing
    val correctStateRoot: ByteString = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)

    val correctGasUsed: BigInt = 0
    val incorrectStateRoot: ByteString = ((correctStateRoot.head + 1) & 0xFF).toByte +: correctStateRoot.tail
    val table: TableFor3[ByteString, BigInt, Validators] = Table[ByteString, BigInt, Validators](
      ("stateRootHash", "cumulativeGasUsedBlock", "validators"),
      (correctStateRoot, correctGasUsed + 1, new Mocks.MockValidatorsAlwaysSucceed),
      (incorrectStateRoot, correctGasUsed, new Mocks.MockValidatorsAlwaysSucceed),
      (correctStateRoot, correctGasUsed, validatorsFailsBlockValidatorWithReceipts)
    )

    forAll(table){ (stateRootHash, cumulativeGasUsedBlock, validators) =>
      val ledger = newTestLedger(validators = validators)

      val blockHeader: BlockHeader = validBlockHeader.copy(gasUsed = cumulativeGasUsedBlock, stateRoot = stateRootHash)
      val block = Block(blockHeader, validBlockBodyWithNoTxs)

      val blockExecResult = ledger.blockExecution.executeBlock(block)

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

    override lazy val vm: VMImpl = new MockVM(c => createResult(
      context = c,
      gasUsed = UInt256(defaultGasLimit),
      gasLimit = UInt256(defaultGasLimit),
      gasRefund = UInt256.Zero
    ))

    forAll(table) { (origin1Address, receiver1Address, origin2Address, receiver2Address) =>

      def keyPair(address: Address): AsymmetricCipherKeyPair = if(address == originAddress) originKeyPair else receiverKeyPair

      val tx1 = validTx.copy(value = 100, receivingAddress = Some(receiver1Address), gasLimit = defaultGasLimit)
      val tx2 = validTx.copy(value = 50, receivingAddress = Some(receiver2Address), gasLimit = defaultGasLimit * 2,
        nonce = validTx.nonce + (if(origin1Address == origin2Address) 1 else 0)
      )
      val stx1 = SignedTransaction.sign(tx1, keyPair(origin1Address), Some(blockchainConfig.chainId))
      val stx2 = SignedTransaction.sign(tx2, keyPair(origin2Address), Some(blockchainConfig.chainId))

      val validBlockBodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(stx1.tx, stx2.tx))
      val block = Block(validBlockHeader, validBlockBodyWithTxs)

      val txsExecResult = ledger.blockExecution.executeBlockTransactions(block)

      assert(txsExecResult.isRight)
      val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.right.get
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
        origin1Address -> UpdateBalance(-minerPaymentForTx1),     // Origin payment for tx execution and nonce increase
        minerAddress -> UpdateBalance(minerPaymentForTx1)         // Miner reward for tx execution
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
        origin2Address -> UpdateBalance(-minerPaymentForTx2),     // Origin payment for tx execution and nonce increase
        minerAddress -> UpdateBalance(minerPaymentForTx2)         // Miner reward for tx execution
      )
      val expectedStateRootTx2 = applyChanges(expectedStateRootTx1, blockchainStorages, changesTx2)

      val Receipt(rootHashReceipt2, gasUsedReceipt2, logsBloomFilterReceipt2, logsReceipt2) = receipt2
      rootHashReceipt2 shouldBe HashOutcome(expectedStateRootTx2)
      gasUsedReceipt2 shouldBe (transaction1.gasLimit + transaction2.gasLimit)
      logsBloomFilterReceipt2 shouldBe BloomFilter.create(Nil)
      logsReceipt2 shouldBe Nil

      // Check world
      InMemoryWorldStateProxy.persistState(resultingWorldState).stateRootHash shouldBe expectedStateRootTx2

      val blockReward = ledger.blockRewardCalculator.calcBlockMinerReward(block.header.number, 0)
      val changes = Seq(
        minerAddress -> UpdateBalance(UInt256(blockReward))
      )
      val blockExpectedStateRoot = applyChanges(expectedStateRootTx2, blockchainStorages, changes)

      val blockWithCorrectStateAndGasUsed = block.copy(
        header = block.header.copy(stateRoot = blockExpectedStateRoot, gasUsed = gasUsedReceipt2)
      )
      assert(ledger.blockExecution.executeBlock(blockWithCorrectStateAndGasUsed).isRight)
    }
  }

  it should "clear logs only if vm execution results in an error" in new TestSetup {

    val defaultsLogs = Seq(defaultLog)

    val table: TableFor2[Option[ProgramError], Int] = Table[Option[ProgramError], Int](
      ("Execution Error", "Logs size"),
      (Some(InvalidOpCode(1)), 0),
      (Some(OutOfGas), 0),
      (Some(InvalidJump(23)), 0),
      (Some(StackOverflow), 0),
      (Some(StackUnderflow), 0),
      (None, defaultsLogs.size)
    )

    forAll(table) { (maybeError, logsSize) =>
      val initialOriginBalance: UInt256 = 1000000

      val initialOriginNonce = defaultTx.nonce

      val initialWorld = emptyWorld
        .saveAccount(originAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))

      val stx = SignedTransaction.sign(defaultTx, originKeyPair, Some(blockchainConfig.chainId))

      val mockVM = new MockVM(createResult(_, defaultGasLimit, defaultGasLimit, 0, maybeError, bEmpty, defaultsLogs))

      val testConsensus = newTestConsensus(vm = mockVM)

      val txResult = testConsensus.blockPreparator.executeTransaction(stx.tx, stx.senderAddress, defaultBlockHeader, initialWorld)

      txResult.logs.size shouldBe logsSize
    }
  }

  it should "create sender account if it does not exists" in new TestSetup {

    val inputData = ByteString("the payload")

    val newAccountKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
    val newAccountAddress = Address(kec256(newAccountKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))

    override lazy val vm: VMImpl = new MockVM((pc: Ledger.PC) => {
      createResult(pc, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString("contract code"))
    })

    val tx: Transaction = defaultTx.copy(gasPrice = 0, receivingAddress = None, payload = inputData)
    val stx: SignedTransactionWithSender = SignedTransaction.sign(tx, newAccountKeyPair, Some(blockchainConfig.chainId))

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] = ledger.blockExecution.executeTransactions(
      Seq(stx.tx),
      initialWorld,
      defaultBlockHeader
    )

    result shouldBe a[Right[_, BlockResult]]
    result.map(br => br.worldState.getAccount(newAccountAddress)) shouldBe Right(Some(Account(nonce = 1)))
  }

  it should "remember executed transaction in case of many failures in the middle" in new TestSetup {
    val newAccountKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
    val newAccountAddress = Address(kec256(newAccountKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))

    override lazy val vm: VMImpl = new MockVM((pc: Ledger.PC) => {
      createResult(pc, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString.empty)
    })

    override lazy val validators: MockValidatorsAlwaysSucceed = new Mocks.MockValidatorsAlwaysSucceed {
      override val signedTransactionValidator: SignedTransactionValidator =
        (stx: SignedTransaction, _: Account, _: BlockHeader, _: UInt256, _: BigInt) => {
          if (stx.tx.receivingAddress.contains(Address(42))) {
            Right(SignedTransactionValid)
          } else {
            Left(TransactionSignatureError)
          }
        }
    }

    val tx1: Transaction = defaultTx.copy(gasPrice = 42, receivingAddress = Some(Address(42)))
    val tx2: Transaction = defaultTx.copy(gasPrice = 43, receivingAddress = Some(Address(43)))
    val tx3: Transaction = defaultTx.copy(gasPrice = 43, receivingAddress = Some(Address(43)))
    val tx4: Transaction = defaultTx.copy(gasPrice = 42, receivingAddress = Some(Address(42)))
    val stx1: SignedTransactionWithSender = SignedTransaction.sign(tx1, newAccountKeyPair, Some(blockchainConfig.chainId))
    val stx2: SignedTransactionWithSender = SignedTransaction.sign(tx2, newAccountKeyPair, Some(blockchainConfig.chainId))
    val stx3: SignedTransactionWithSender = SignedTransaction.sign(tx3, newAccountKeyPair, Some(blockchainConfig.chainId))
    val stx4: SignedTransactionWithSender = SignedTransaction.sign(tx4, newAccountKeyPair, Some(blockchainConfig.chainId))

    val result: (BlockResult, Seq[SignedTransaction]) = consensus.blockPreparator.executePreparedTransactions(
      Seq(stx1.tx, stx2.tx, stx3.tx, stx4.tx),
      initialWorld,
      defaultBlockHeader)

    result match { case (_, executedTxs) => executedTxs shouldBe Seq(stx1.tx, stx4.tx) }
  }

  it should "produce empty block if all txs fail" in new TestSetup {
    val newAccountKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
    val newAccountAddress = Address(kec256(newAccountKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))

    override lazy val vm = new MockVM((pc: Ledger.PC) => {
      createResult(pc, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString.empty)
    })

    override lazy val validators: Mocks.MockValidatorsAlwaysSucceed = new Mocks.MockValidatorsAlwaysSucceed {
      override val signedTransactionValidator: SignedTransactionValidator =
        (_: SignedTransaction, _: Account, _: BlockHeader, _: UInt256, _: BigInt) => {
          Left(TransactionSignatureError)
        }
    }

    val tx1: Transaction = defaultTx.copy(gasPrice = 42, receivingAddress = Some(Address(42)))
    val tx2: Transaction = defaultTx.copy(gasPrice = 42, receivingAddress = Some(Address(42)))
    val stx1: SignedTransactionWithSender = SignedTransaction.sign(tx1, newAccountKeyPair, Some(blockchainConfig.chainId))
    val stx2: SignedTransactionWithSender = SignedTransaction.sign(tx2, newAccountKeyPair, Some(blockchainConfig.chainId))

    val result: (BlockResult, Seq[SignedTransaction]) = consensus.blockPreparator.executePreparedTransactions(
      Seq(stx1.tx, stx2.tx),
      initialWorld,
      defaultBlockHeader)

    result match { case (_, executedTxs) => executedTxs shouldBe Seq.empty }
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
      (worldState.transfer _).expects(addr, supportDaoForkConfig.refundContract.get, daoAccountsFakeBalance).returning(worldState)
    }

    override lazy val ledger: LedgerImpl = newTestLedger(blockchain = testBlockchain, blockchainConfig = proDaoBlockchainConfig)

    // We don't care about block txs in this test
    ledger.blockExecution.executeBlockTransactions(proDaoBlock.copy(body = proDaoBlock.body.copy(transactionList = Seq.empty)))
  }

  it should "neither drain DAO accounts nor send the funds to refund address if Pro DAO Fork was not configured" in new DaoForkTestSetup {
    // Check we drain all the accounts and send the balance to refund contract
    supportDaoForkConfig.drainList.foreach { _ =>
      (worldState.transfer _).expects(*, *, *).never()
    }

    override lazy val ledger: LedgerImpl = newTestLedger(blockchain = testBlockchain)

    // We don't care about block txs in this test
    ledger.blockExecution.executeBlockTransactions(proDaoBlock.copy(body = proDaoBlock.body.copy(transactionList = Seq.empty)))
  }

  it should "correctly determine current block status" in new BlockchainSetup {
    val testHash: ByteString = validBlockParentHeader.copy(number = validBlockParentHeader.number + 5).hash

    val validBlockHeaderNoParent: BlockHeader = validBlockHeader.copy(parentHash = testHash)

    override lazy val vm: VMImpl = new MockVM(c =>
      createResult(context = c, gasUsed = defaultGasLimit, gasLimit = defaultGasLimit, gasRefund = 0)
    )

    ledger.checkBlockStatus(validBlockParentHeader.hash) shouldEqual InChain

    whenReady(ledger.importBlock(Block(validBlockHeaderNoParent, validBlockBodyWithNoTxs))){result => result shouldEqual BlockEnqueued}

    ledger.checkBlockStatus(validBlockHeaderNoParent.hash) shouldEqual Queued

    ledger.checkBlockStatus(validBlockHeader.hash) shouldEqual UnknownBlock
  }

  it should "properly find minimal required gas limit to execute transaction" in new BinarySimulationChopSetup {
    testGasValues.foreach(minimumRequiredGas =>
      LedgerUtils.binaryChop[TxError](minimalGas, maximalGas)(mockTransaction(minimumRequiredGas)) shouldEqual minimumRequiredGas
    )
  }

  it should "properly assign stateRootHash before byzantium block (exclusive)" in new TestSetup {

    val tx: Transaction = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)
    val stx: SignedTransactionWithSender = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))
    val header: BlockHeader = defaultBlockHeader.copy(number = blockchainConfig.byzantiumBlockNumber - 1)

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
      ledger.blockExecution.executeTransactions(Seq(stx.tx), initialWorld, header)

    result shouldBe a[Right[_, BlockResult]]
    result.map { br =>
      br.receipts.last.postTransactionStateHash shouldBe a[HashOutcome]
    }
  }

  it should "properly assign stateRootHash after byzantium block (inclusive) if operation is a success" in new TestSetup {

    val tx: Transaction = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)
    val stx: SignedTransaction = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId)).tx
    val header: BlockHeader = defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = blockchainConfig.byzantiumBlockNumber)

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
      ledger.blockExecution.executeTransactions(Seq(stx), initialWorld, header)

    result shouldBe a[Right[_, BlockResult]]
    result.map(_.receipts.last.postTransactionStateHash shouldBe SuccessOutcome)
  }

  it should "properly assign stateRootHash after byzantium block (inclusive) if operation is a failure" in new TestSetup {

    val defaultsLogs = Seq(defaultLog)

    lazy val mockVM = new MockVM(createResult(_, defaultGasLimit, defaultGasLimit, 0, Some(RevertOccurs), bEmpty, defaultsLogs))

    val testConsensus: Consensus = newTestConsensus(vm = mockVM)

    val tx: Transaction = defaultTx.copy(gasPrice = defaultGasLimit, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)
    val stx: SignedTransactionWithSender = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))
    val header: BlockHeader = defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = blockchainConfig.byzantiumBlockNumber)

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
      testConsensus.blockPreparator.executeTransactions(Seq(stx.tx), initialWorld, header)

    result shouldBe a[Right[_, BlockResult]]
    result.map(_.receipts.last.postTransactionStateHash shouldBe FailureOutcome)
  }

}
