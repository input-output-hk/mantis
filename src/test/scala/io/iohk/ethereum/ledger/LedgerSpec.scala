package io.iohk.ethereum.ledger


import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.validators.SignedTransactionInvalid.TransactionSignatureError
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.{BlockTransactionsHashError, BlockValid}
import io.iohk.ethereum.consensus.validators.{Validators, _}
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.{ValidationAfterExecError, ValidationBeforeExecError}
import io.iohk.ethereum.ledger.Ledger.{BlockResult, PC, PR, VMImpl}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.utils.DaoForkConfig
import io.iohk.ethereum.vm._
import io.iohk.ethereum.{Fixtures, Mocks}
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.util.encoders.Hex

// scalastyle:off file.size.limit
// scalastyle:off magic.number
class LedgerSpec extends FlatSpec with PropertyChecks with Matchers with MockFactory {

  def createResult(context: PC,
                   gasUsed: BigInt,
                   gasLimit: BigInt,
                   gasRefund: BigInt,
                   error: Option[ProgramError] = None,
                   returnData: ByteString = bEmpty,
                   logs: Seq[TxLogEntry] = Nil,
                   addressesToDelete: Set[Address] = Set.empty): PR =
    ProgramResult(
      returnData = returnData,
      gasRemaining = gasLimit - gasUsed,
      world = context.world,
      addressesToDelete = addressesToDelete,
      logs = logs,
      internalTxs = Nil,
      gasRefund = gasRefund,
      error = error
    )

  sealed trait Changes
  case class UpdateBalance(amount: UInt256) extends Changes
  case object IncreaseNonce extends Changes
  case object DeleteAccount extends Changes

  def applyChanges(stateRootHash: ByteString, blockchainStorages: BlockchainStorages, changes: Seq[(Address, Changes)]): ByteString = {
    val initialWorld = BlockchainImpl(blockchainStorages).getWorldStateProxy(-1, UInt256.Zero, Some(stateRootHash),
      noEmptyAccounts = false, ethCompatibilityMode = true)
    val newWorld = changes.foldLeft[InMemoryWorldStateProxy](initialWorld){ case (recWorld, (address, change)) =>
        change match {
          case UpdateBalance(balanceIncrease) =>
            val accountWithBalanceIncrease = recWorld.getAccount(address).getOrElse(Account.empty()).increaseBalance(balanceIncrease)
            recWorld.saveAccount(address, accountWithBalanceIncrease)
          case IncreaseNonce =>
            val accountWithNonceIncrease = recWorld.getAccount(address).getOrElse(Account.empty()).increaseNonce()
            recWorld.saveAccount(address, accountWithNonceIncrease)
          case DeleteAccount =>
            recWorld.deleteAccount(address)
        }
    }
    InMemoryWorldStateProxy.persistState(newWorld).stateRootHash
  }

  "Ledger" should "correctly calculate the total gas refund to be returned to the sender and paying for gas to the miner" in new TestSetup {

    val table = Table[BigInt, BigInt, Option[ProgramError], BigInt](
      ("execGasUsed", "refundsFromVM", "maybeError", "gasUsed"),
      (25000, 20000, None, 25000 - 12500),
      (25000, 10000, None, 25000 - 10000),
      (125000, 10000, Some(OutOfGas), defaultGasLimit),
      (125000, 100000, Some(OutOfGas), defaultGasLimit)
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

      val execResult = testConsensus.blockPreparator.executeTransaction(stx, header, worldWithMinerAndOriginAccounts)
      val postTxWorld = execResult.worldState

      execResult.gasUsed shouldEqual gasUsed
      postTxWorld.getBalance(originAddress) shouldEqual (initialOriginBalance - balanceDelta)
      postTxWorld.getBalance(minerAddress) shouldEqual (initialMinerBalance + balanceDelta)
    }
  }

  it should "correctly change the nonce when executing a tx that results in contract creation" in new TestSetup {

    val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)

    val stx = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

    val postTxWorld = consensus.blockPreparator.executeTransaction(stx, header, worldWithMinerAndOriginAccounts).worldState

    postTxWorld.getGuaranteedAccount(originAddress).nonce shouldBe (initialOriginNonce + 1)
  }

  it should "correctly change the nonce when executing a tx that results in a message call" in new TestSetup {

    val tx = defaultTx.copy(
      gasPrice = defaultGasPrice, gasLimit = defaultGasLimit,
      receivingAddress = Some(originAddress), payload = ByteString.empty
    )

    val stx = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

    val postTxWorld = consensus.blockPreparator.executeTransaction(stx, header, worldWithMinerAndOriginAccounts).worldState

    postTxWorld.getGuaranteedAccount(originAddress).nonce shouldBe (initialOriginNonce + 1)
  }

  it should "correctly run executeBlockTransactions for a block without txs" in new BlockchainSetup {

    val block = Block(validBlockHeader, validBlockBodyWithNoTxs)

    override lazy val vm: VMImpl = new MockVM(c =>
      createResult(context = c, gasUsed = defaultGasLimit, gasLimit = defaultGasLimit, gasRefund = 0)
    )

    val txsExecResult = ledger.executeBlockTransactions(block)

    assert(txsExecResult.isRight)
    val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.right.get
    resultingGasUsed shouldBe 0
    resultingReceipts shouldBe Nil
    InMemoryWorldStateProxy.persistState(resultingWorldState).stateRootHash shouldBe validBlockHeader.stateRoot
  }

  it should "correctly run executeBlockTransactions for a block with one tx (that produces no errors)" in new BlockchainSetup {

    val table = Table[BigInt, Seq[TxLogEntry], Set[Address], Boolean](
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
      val blockBodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(stx))
      val block = Block(blockHeader, blockBodyWithTxs)

      val mockValidators =
        if (txValidAccordingToValidators) Mocks.MockValidatorsAlwaysSucceed
        else Mocks.MockValidatorsAlwaysFail
      val mockVm = new MockVM(c => createResult(
        context = c,
        gasUsed = UInt256(gasLimit),
        gasLimit = UInt256(gasLimit),
        gasRefund = UInt256.Zero,
        logs = logs,
        addressesToDelete = addressesToDelete
      ))

      // Beware we are not using `ledger`
      val testLedger = newTestLedger(validators = mockValidators, vm = mockVm)

      val txsExecResult = testLedger.executeBlockTransactions(block)

      txsExecResult.isRight shouldBe txValidAccordingToValidators
      if(txsExecResult.isRight){
        val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.right.get

        //Check valid world
        val minerPaymentForTxs = UInt256(stx.tx.gasLimit * stx.tx.gasPrice)
        val changes = Seq(
          originAddress -> IncreaseNonce,
          originAddress -> UpdateBalance(-minerPaymentForTxs),          //Origin payment for tx execution and nonce increase
          minerAddress -> UpdateBalance(minerPaymentForTxs)             //Miner reward for tx execution
        ) ++ addressesToDelete.map(address => address -> DeleteAccount) //Delete all accounts to be deleted
        val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)
        expectedStateRoot shouldBe InMemoryWorldStateProxy.persistState(resultingWorldState).stateRootHash

        //Check valid gasUsed
        resultingGasUsed shouldBe stx.tx.gasLimit

        //Check valid receipts
        resultingReceipts.size shouldBe 1
        val Receipt(rootHashReceipt, gasUsedReceipt, logsBloomFilterReceipt, logsReceipt, _, _) = resultingReceipts.head
        rootHashReceipt shouldBe expectedStateRoot
        gasUsedReceipt shouldBe resultingGasUsed
        logsBloomFilterReceipt shouldBe BloomFilter.create(logs)
        logsReceipt shouldBe logs
      }

    }
  }

  it should "correctly run executeBlockTransactions for a block with one tx (that produces OutOfGas)" in new BlockchainSetup {

    val blockBodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(validStxSignedByOrigin))
    val block = Block(validBlockHeader, blockBodyWithTxs)

    override lazy val vm = new MockVM(c => createResult(
      context = c,
      gasUsed = UInt256(defaultGasLimit),
      gasLimit = UInt256(defaultGasLimit),
      gasRefund = UInt256.Zero,
      logs = defaultLogs,
      addressesToDelete = defaultAddressesToDelete,
      error = Some(OutOfGas)
    ))

    val txsExecResult = ledger.executeBlockTransactions(block)

    assert(txsExecResult.isRight)
    val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.right.get

    //Check valid world
    val minerPaymentForTxs = UInt256(validStxSignedByOrigin.tx.gasLimit * validStxSignedByOrigin.tx.gasPrice)
    val changes = Seq(
      originAddress -> IncreaseNonce,
      originAddress -> UpdateBalance(-minerPaymentForTxs),  //Origin payment for tx execution and nonce increase
      minerAddress -> UpdateBalance(minerPaymentForTxs)     //Miner reward for tx execution
    )
    val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)
    expectedStateRoot shouldBe InMemoryWorldStateProxy.persistState(resultingWorldState).stateRootHash

    //Check valid gasUsed
    resultingGasUsed shouldBe validStxSignedByOrigin.tx.gasLimit

    //Check valid receipts
    resultingReceipts.size shouldBe 1
    val Receipt(rootHashReceipt, gasUsedReceipt, logsBloomFilterReceipt, logsReceipt, _, _) = resultingReceipts.head
    rootHashReceipt shouldBe expectedStateRoot
    gasUsedReceipt shouldBe resultingGasUsed
    logsBloomFilterReceipt shouldBe BloomFilter.create(Nil)
    logsReceipt shouldBe Nil

  }

  it should "correctly run executeBlock for a valid block without txs" in new BlockchainSetup {

    val table = Table[Int, BigInt](
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


      val blockExecResult = ledger.executeBlock(block)
      assert(blockExecResult.isRight)
    }
  }

  it should "fail to run executeBlock if a block is invalid before executing it" in new BlockchainSetup {
    object validatorsOnlyFailsBlockValidator extends Mocks.MockValidatorsAlwaysSucceed {
      override val blockValidator = Mocks.MockValidatorsAlwaysFail.blockValidator
    }

    object validatorsOnlyFailsBlockHeaderValidator extends Mocks.MockValidatorsAlwaysSucceed {
      override val blockHeaderValidator = Mocks.MockValidatorsAlwaysFail.blockHeaderValidator
    }

    object validatorsOnlyFailsOmmersValidator extends Mocks.MockValidatorsAlwaysSucceed {
      override val ommersValidator = Mocks.MockValidatorsAlwaysFail.ommersValidator
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

    val blockReward = new BlockRewardCalculator(blockchainConfig.monetaryPolicyConfig)
      .calcBlockMinerReward(validBlockHeader.number, 0)

    val changes = Seq(
      minerAddress -> UpdateBalance(UInt256(blockReward)) //Paying miner for block processing
    )
    val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)
    val blockHeader: BlockHeader = validBlockHeader.copy(stateRoot = expectedStateRoot)
    val block = Block(blockHeader, validBlockBodyWithNoTxs)


    assert(seqFailingValidators.forall { validators =>
      val ledger = newTestLedger(validators = validators)
      val blockExecResult = ledger.executeBlock(block)

      blockExecResult.left.forall {
        case e: ValidationBeforeExecError => true
        case _ => false
      }
    })
  }

  it should "fail to run executeBlock if a block is invalid after executing it" in new BlockchainSetup {

    object validatorsFailsBlockValidatorWithReceipts extends Mocks.MockValidatorsAlwaysSucceed {
      override val blockValidator = new BlockValidator {
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

    val blockReward = new BlockRewardCalculator(blockchainConfig.monetaryPolicyConfig)
      .calcBlockMinerReward(validBlockHeader.number, 0)

    val changes = Seq(minerAddress -> UpdateBalance(UInt256(blockReward))) //Paying miner for block processing
    val correctStateRoot: ByteString = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)

    val correctGasUsed: BigInt = 0
    val incorrectStateRoot: ByteString = ((correctStateRoot.head + 1) & 0xFF).toByte +: correctStateRoot.tail
    val table = Table[ByteString, BigInt, Validators](
      ("stateRootHash", "cumulativeGasUsedBlock", "validators"),
      (correctStateRoot, correctGasUsed + 1, new Mocks.MockValidatorsAlwaysSucceed),
      (incorrectStateRoot, correctGasUsed, new Mocks.MockValidatorsAlwaysSucceed),
      (correctStateRoot, correctGasUsed, validatorsFailsBlockValidatorWithReceipts)
    )

    forAll(table){ (stateRootHash, cumulativeGasUsedBlock, validators) =>
      val ledger = newTestLedger(validators = validators)

      val blockHeader: BlockHeader = validBlockHeader.copy(gasUsed = cumulativeGasUsedBlock, stateRoot = stateRootHash)
      val block = Block(blockHeader, validBlockBodyWithNoTxs)

      val blockExecResult = ledger.executeBlock(block)

      assert(blockExecResult match {
        case Left(_: ValidationAfterExecError) => true
        case _ => false
      })
    }
  }

  it should "correctly run a block with more than one tx" in new BlockchainSetup {
    val table = Table[Address, Address, Address, Address](
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

      val validBlockBodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(stx1, stx2))
      val block = Block(validBlockHeader, validBlockBodyWithTxs)

      val txsExecResult = ledger.executeBlockTransactions(block)

      assert(txsExecResult.isRight)
      val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.right.get

      //Check valid gasUsed
      resultingGasUsed shouldBe stx1.tx.gasLimit + stx2.tx.gasLimit

      //Check valid receipts
      resultingReceipts.size shouldBe 2
      val Seq(receipt1, receipt2) = resultingReceipts

      //Check receipt1
      val minerPaymentForTx1 = UInt256(stx1.tx.gasLimit * stx1.tx.gasPrice)
      val changesTx1 = Seq(
        origin1Address -> IncreaseNonce,
        origin1Address -> UpdateBalance(-minerPaymentForTx1),     //Origin payment for tx execution and nonce increase
        minerAddress -> UpdateBalance(minerPaymentForTx1)         //Miner reward for tx execution
      )
      val expectedStateRootTx1 = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changesTx1)

      val Receipt(rootHashReceipt1, gasUsedReceipt1, logsBloomFilterReceipt1, logsReceipt1, _, _) = receipt1
      rootHashReceipt1 shouldBe expectedStateRootTx1
      gasUsedReceipt1 shouldBe stx1.tx.gasLimit
      logsBloomFilterReceipt1 shouldBe BloomFilter.create(Nil)
      logsReceipt1 shouldBe Nil

      //Check receipt2
      val minerPaymentForTx2 = UInt256(stx2.tx.gasLimit * stx2.tx.gasPrice)
      val changesTx2 = Seq(
        origin2Address -> IncreaseNonce,
        origin2Address -> UpdateBalance(-minerPaymentForTx2),     //Origin payment for tx execution and nonce increase
        minerAddress -> UpdateBalance(minerPaymentForTx2)         //Miner reward for tx execution
      )
      val expectedStateRootTx2 = applyChanges(expectedStateRootTx1, blockchainStorages, changesTx2)

      val Receipt(rootHashReceipt2, gasUsedReceipt2, logsBloomFilterReceipt2, logsReceipt2, _, _) = receipt2
      rootHashReceipt2 shouldBe expectedStateRootTx2
      gasUsedReceipt2 shouldBe (stx1.tx.gasLimit + stx2.tx.gasLimit)
      logsBloomFilterReceipt2 shouldBe BloomFilter.create(Nil)
      logsReceipt2 shouldBe Nil

      //Check world
      InMemoryWorldStateProxy.persistState(resultingWorldState).stateRootHash shouldBe expectedStateRootTx2

      val blockReward = ledger.blockRewardCalculator.calcBlockMinerReward(block.header.number, 0)
      val changes = Seq(
        minerAddress -> UpdateBalance(UInt256(blockReward))
      )
      val blockExpectedStateRoot = applyChanges(expectedStateRootTx2, blockchainStorages, changes)

      val blockWithCorrectStateAndGasUsed = block.copy(
        header = block.header.copy(stateRoot = blockExpectedStateRoot, gasUsed = gasUsedReceipt2)
      )
      assert(ledger.executeBlock(blockWithCorrectStateAndGasUsed).isRight)
    }
  }

  it should "clear logs only if vm execution results in an error" in new TestSetup {

    val defaultsLogs = Seq(defaultLog)

    val table = Table[Option[ProgramError], Int](
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

      val txResult = testConsensus.blockPreparator.executeTransaction(stx, defaultBlockHeader, initialWorld)

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
    val stx: SignedTransaction = SignedTransaction.sign(tx, newAccountKeyPair, Some(blockchainConfig.chainId))

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] = consensus.blockPreparator.executeTransactions(
      Seq(stx),
      initialWorld,
      defaultBlockHeader)

    result shouldBe a[Right[_, BlockResult]]
    result.map(br => br.worldState.getAccount(newAccountAddress)) shouldBe Right(Some(Account(nonce = 1)))
  }

  it should "remember executed transaction in case of many failures in the middle" in new TestSetup {
    val newAccountKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
    val newAccountAddress = Address(kec256(newAccountKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))

    override lazy val vm: VMImpl = new MockVM((pc: Ledger.PC) => {
      createResult(pc, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString.empty)
    })

    override lazy val validators = new Mocks.MockValidatorsAlwaysSucceed {
      override val signedTransactionValidator =
        (stx: SignedTransaction, _: Account, _: BlockHeader, _: UInt256, _: BigInt) => {
          if (stx.tx.receivingAddress == Some(Address(42))) {
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
    val stx1: SignedTransaction = SignedTransaction.sign(tx1, newAccountKeyPair, Some(blockchainConfig.chainId))
    val stx2: SignedTransaction = SignedTransaction.sign(tx2, newAccountKeyPair, Some(blockchainConfig.chainId))
    val stx3: SignedTransaction = SignedTransaction.sign(tx3, newAccountKeyPair, Some(blockchainConfig.chainId))
    val stx4: SignedTransaction = SignedTransaction.sign(tx4, newAccountKeyPair, Some(blockchainConfig.chainId))

    val result: (BlockResult, Seq[SignedTransaction]) = consensus.blockPreparator.executePreparedTransactions(
      Seq(stx1, stx2, stx3, stx4),
      initialWorld,
      defaultBlockHeader)

    result match { case (_, executedTxs) => executedTxs shouldBe Seq(stx1, stx4) }
  }

  it should "produce empty block if all txs fail" in new TestSetup {
    val newAccountKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
    val newAccountAddress = Address(kec256(newAccountKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))

    override lazy val vm = new MockVM((pc: Ledger.PC) => {
      createResult(pc, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString.empty)
    })

    override lazy val validators = new Mocks.MockValidatorsAlwaysSucceed {
      override val signedTransactionValidator =
        (_: SignedTransaction, _: Account, _: BlockHeader, _: UInt256, _: BigInt) => {
          Left(TransactionSignatureError)
        }
    }

    val tx1: Transaction = defaultTx.copy(gasPrice = 42, receivingAddress = Some(Address(42)))
    val tx2: Transaction = defaultTx.copy(gasPrice = 42, receivingAddress = Some(Address(42)))
    val stx1: SignedTransaction = SignedTransaction.sign(tx1, newAccountKeyPair, Some(blockchainConfig.chainId))
    val stx2: SignedTransaction = SignedTransaction.sign(tx2, newAccountKeyPair, Some(blockchainConfig.chainId))

    val result: (BlockResult, Seq[SignedTransaction]) = consensus.blockPreparator.executePreparedTransactions(
      Seq(stx1, stx2),
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

    override lazy val ledger = newTestLedger(blockchain = testBlockchain, blockchainConfig = proDaoBlockchainConfig)

    ledger.executeBlockTransactions(
      proDaoBlock.copy(body = proDaoBlock.body.copy(transactionList = Seq.empty)) // We don't care about block txs in this test
    )
  }

  it should "neither drain DAO accounts nor send the funds to refund address if Pro DAO Fork was not configured" in new DaoForkTestSetup {
    // Check we drain all the accounts and send the balance to refund contract
    supportDaoForkConfig.drainList.foreach { addr =>
      val daoAccountsFakeBalance = UInt256(1000)
      (worldState.transfer _).expects(*, *, *).never()
    }

    override lazy val ledger = newTestLedger(blockchain = testBlockchain)

    ledger.executeBlockTransactions(
      proDaoBlock.copy(body = proDaoBlock.body.copy(transactionList = Seq.empty)) // We don't care about block txs in this test
    )
  }

  it should "correctly determine current block status" in new BlockchainSetup {
    val testHash = validBlockParentHeader.copy(number = validBlockParentHeader.number + 5).hash

    val validBlockHeaderNoParent = validBlockHeader.copy(parentHash = testHash)

    override lazy val vm: VMImpl = new MockVM(c =>
      createResult(context = c, gasUsed = defaultGasLimit, gasLimit = defaultGasLimit, gasRefund = 0)
    )

    ledger.checkBlockStatus(validBlockParentHeader.hash) shouldEqual InChain

    ledger.importBlock(Block(validBlockHeaderNoParent, validBlockBodyWithNoTxs)) shouldEqual BlockEnqueued

    ledger.checkBlockStatus(validBlockHeaderNoParent.hash) shouldEqual Queued

    ledger.checkBlockStatus(validBlockHeader.hash) shouldEqual UnknownBlock
  }

  it should "properly find minimal required gas limit to execute transaction" in new BinarySimulationChopSetup {
    testGasValues.foreach(minimumRequiredGas =>
      LedgerUtils.binaryChop[TxError](minimalGas, maximalGas)(mockTransaction(minimumRequiredGas)) shouldEqual minimumRequiredGas
    )
  }

  trait TestSetup extends SecureRandomBuilder with EphemBlockchainTestSetup {
    //+ cake overrides
    // Give a more specific type to Ledger, it is needed by the tests
    override lazy val ledger: LedgerImpl = newLedger()
    //- cake overrides

    val originKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
    val receiverKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
    //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of spongycastle encoding
    val originAddress = Address(kec256(originKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))
    val receiverAddress = Address(kec256(receiverKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))
    val minerAddress = Address(666)

    val defaultBlockHeader = BlockHeader(
      parentHash = bEmpty,
      ommersHash = bEmpty,
      beneficiary = bEmpty,
      stateRoot = bEmpty,
      transactionsRoot = bEmpty,
      receiptsRoot = bEmpty,
      logsBloom = bEmpty,
      difficulty = 1000000,
      number = blockchainConfig.homesteadBlockNumber + 1,
      gasLimit = 1000000,
      gasUsed = 0,
      unixTimestamp = 1486752441,
      extraData = bEmpty,
      mixHash = bEmpty,
      nonce = bEmpty
    )

    val defaultTx = Transaction(
      nonce = 42,
      gasPrice = 1,
      gasLimit = 90000,
      receivingAddress = receiverAddress,
      value = 0,
      payload = ByteString.empty)

    val defaultLog = TxLogEntry(
      loggerAddress = originAddress,
      logTopics = Seq(ByteString(Hex.decode("962cd36cf694aa154c5d3a551f19c98f356d906e96828eeb616e16fae6415738"))),
      data = ByteString(Hex.decode("1" * 128))
    )

    val initialOriginBalance: UInt256 = 100000000
    val initialMinerBalance: UInt256 = 2000000

    val initialOriginNonce = defaultTx.nonce

    val defaultAddressesToDelete = Set(Address(Hex.decode("01")), Address(Hex.decode("02")), Address(Hex.decode("03")))
    val defaultLogs = Seq(defaultLog.copy(loggerAddress = defaultAddressesToDelete.head))
    val defaultGasPrice: UInt256 = 10
    val defaultGasLimit: UInt256 = 1000000
    val defaultValue: BigInt = 1000

    val emptyWorld = BlockchainImpl(storagesInstance.storages).getWorldStateProxy(-1, UInt256.Zero, None,
      noEmptyAccounts = false, ethCompatibilityMode = true)

    val worldWithMinerAndOriginAccounts = InMemoryWorldStateProxy.persistState(emptyWorld
      .saveAccount(originAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))
      .saveAccount(receiverAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))
      .saveAccount(minerAddress, Account(balance = initialMinerBalance)))

    val initialWorld = InMemoryWorldStateProxy.persistState(
      defaultAddressesToDelete.foldLeft(worldWithMinerAndOriginAccounts){
        (recWorld, address) => recWorld.saveAccount(address, Account.empty())
      }
    )
  }

  trait BlockchainSetup extends TestSetup {
    val blockchainStorages = storagesInstance.storages

    val validBlockParentHeader: BlockHeader = defaultBlockHeader.copy(
      stateRoot = initialWorld.stateRootHash
    )
    val validBlockHeader: BlockHeader = defaultBlockHeader.copy(
      stateRoot = initialWorld.stateRootHash,
      parentHash = validBlockParentHeader.hash,
      beneficiary = minerAddress.bytes,
      receiptsRoot = Account.EmptyStorageRootHash,
      logsBloom = BloomFilter.EmptyBloomFilter,
      gasLimit = defaultGasLimit,
      gasUsed = 0
    )
    val validBlockBodyWithNoTxs: BlockBody = BlockBody(Nil, Nil)

    blockchain.save(validBlockParentHeader)
    blockchain.save(validBlockParentHeader.hash, validBlockBodyWithNoTxs)
    storagesInstance.storages.appStateStorage.putBestBlockNumber(validBlockParentHeader.number)
    storagesInstance.storages.totalDifficultyStorage.put(validBlockParentHeader.hash, 0)

    val validTx: Transaction = defaultTx.copy(
      nonce = initialOriginNonce,
      gasLimit = defaultGasLimit,
      value = defaultValue
    )
    val validStxSignedByOrigin: SignedTransaction = SignedTransaction.sign(validTx, originKeyPair, Some(blockchainConfig.chainId))
  }

  trait DaoForkTestSetup extends TestSetup {

    lazy val testBlockchain: BlockchainImpl = mock[BlockchainImpl]
    val worldState = mock[InMemoryWorldStateProxy]
    val proDaoBlock = Fixtures.Blocks.ProDaoForkBlock.block

    val supportDaoForkConfig = new DaoForkConfig {
      override val blockExtraData: Option[ByteString] = Some(ByteString("refund extra data"))
      override val range: Int = 10
      override val drainList: Seq[Address] = Seq(Address(1), Address(2), Address(3))
      override val forkBlockHash: ByteString = proDaoBlock.header.hash
      override val forkBlockNumber: BigInt = proDaoBlock.header.number
      override val refundContract: Option[Address] = Some(Address(4))
    }

    val proDaoBlockchainConfig = blockchainConfig.copy(
      chainId = 0x01.toByte,
      daoForkConfig = Some(supportDaoForkConfig),
      customGenesisFileOpt = None,
      eip106BlockNumber = Long.MaxValue,
      maxCodeSize = None
    )

    (testBlockchain.getBlockHeaderByHash _).expects(proDaoBlock.header.parentHash).returning(Some(Fixtures.Blocks.DaoParentBlock.header))
    (testBlockchain.getWorldStateProxy _)
      .expects(proDaoBlock.header.number, proDaoBlockchainConfig.accountStartNonce, Some(Fixtures.Blocks.DaoParentBlock.header.stateRoot), false, true)
      .returning(worldState)
  }

  trait BinarySimulationChopSetup {
    sealed trait TxError
    case object TxError extends TxError

    val minimalGas:BigInt = 20000
    val maximalGas:BigInt = 100000
    val stepGas: BigInt = 625

    val testGasValues = minimalGas.to(maximalGas, stepGas).toList

    val mockTransaction: BigInt => BigInt => Option[TxError] =
      minimalWorkingGas => gasLimit => if (gasLimit >= minimalWorkingGas) None else Some(TxError)

  }
}
