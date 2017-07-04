package io.iohk.ethereum.ledger


import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.{ValidationAfterExecError, ValidationBeforeExecError}
import io.iohk.ethereum.{Mocks, rlp}
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import io.iohk.ethereum.ledger.Ledger.{BlockResult, PC, PR}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.vm.{UInt256, _}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.spongycastle.crypto.params.ECPublicKeyParameters
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.validators.BlockValidator.BlockTransactionsHashError
import io.iohk.ethereum.validators._
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.util.encoders.Hex

class LedgerSpec extends FlatSpec with PropertyChecks with Matchers {

  val blockchainConfig = BlockchainConfig(Config.config)

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
      gasRefund = gasRefund,
      error = error
    )

  sealed trait Changes
  case class UpdateBalance(amount: UInt256) extends Changes
  case object IncreaseNonce extends Changes
  case object DeleteAccount extends Changes

  def applyChanges(stateRootHash: ByteString, blockchainStorages: BlockchainStorages, changes: Seq[(Address, Changes)]): ByteString = {
    val initialWorld = InMemoryWorldStateProxy(blockchainStorages, UInt256.Zero, Some(stateRootHash))
    val newWorld = changes.foldLeft[InMemoryWorldStateProxy](initialWorld){ case (recWorld, (address, change)) =>
        change match {
          case UpdateBalance(balanceIncrease) =>
            val accountWithBalanceIncrease = recWorld.getAccount(address).getOrElse(Account.empty(UInt256.Zero)).increaseBalance(balanceIncrease)
            recWorld.saveAccount(address, accountWithBalanceIncrease)
          case IncreaseNonce =>
            val accountWithNonceIncrease = recWorld.getAccount(address).getOrElse(Account.empty(UInt256.Zero)).increaseNonce
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
      val ledger = new LedgerImpl(mockVM, blockchainConfig)

      val execResult = ledger.executeTransaction(stx, header, worldWithMinerAndOriginAccounts)
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

    val ledger = new LedgerImpl(new MockVM(), blockchainConfig)

    val postTxWorld = ledger.executeTransaction(stx, header, worldWithMinerAndOriginAccounts).worldState

    postTxWorld.getGuaranteedAccount(originAddress).nonce shouldBe (initialOriginNonce + 1)
  }

  it should "correctly change the nonce when executing a tx that results in a message call" in new TestSetup {

    val tx = defaultTx.copy(
      gasPrice = defaultGasPrice, gasLimit = defaultGasLimit,
      receivingAddress = Some(originAddress), payload = ByteString.empty
    )

    val stx = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

    val ledger = new LedgerImpl(new MockVM(), blockchainConfig)

    val postTxWorld = ledger.executeTransaction(stx, header, worldWithMinerAndOriginAccounts).worldState

    postTxWorld.getGuaranteedAccount(originAddress).nonce shouldBe (initialOriginNonce + 1)
  }

  it should "correctly run executeBlockTransactions for a block without txs" in new BlockchainSetup {

    val block = Block(validBlockHeader, validBlockBodyWithNoTxs)

    val ledger = new LedgerImpl(
      new MockVM(c => createResult(context = c, gasUsed = defaultGasLimit, gasLimit = defaultGasLimit, gasRefund = 0)),
      blockchainConfig
    )

    val txsExecResult = ledger.executeBlockTransactions(
      block,
      blockchain,
      storagesInstance.storages,
      (new Mocks.MockValidatorsAlwaysSucceed).signedTransactionValidator
    )

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

      val ledger = new LedgerImpl(new MockVM(c => createResult(
        context = c,
        gasUsed = UInt256(gasLimit),
        gasLimit = UInt256(gasLimit),
        gasRefund = UInt256.Zero,
        logs = logs,
        addressesToDelete = addressesToDelete
      )), blockchainConfig)

      val txsExecResult = ledger.executeBlockTransactions(
        block,
        blockchain,
        blockchainStorages,
        if(txValidAccordingToValidators) (new Mocks.MockValidatorsAlwaysSucceed).signedTransactionValidator
        else Mocks.MockValidatorsAlwaysFail.signedTransactionValidator
      )

      txsExecResult.isRight shouldBe txValidAccordingToValidators
      if(txsExecResult.isRight){
        val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.right.get

        //Check valid world
        val minerPaymentForTxs = UInt256(stx.tx.gasLimit * stx.tx.gasPrice)
        val changes = Seq(
          originAddress -> IncreaseNonce,
          originAddress -> UpdateBalance(-minerPaymentForTxs),          //Origin payment for tx execution and nonce increase
          minerAddress -> UpdateBalance(minerPaymentForTxs),            //Miner reward for tx execution
          originAddress -> UpdateBalance(-UInt256(stx.tx.value)),       //Discount tx.value from originAddress
          receiverAddress -> UpdateBalance(UInt256(stx.tx.value))       //Increase tx.value to recevierAddress
        ) ++ addressesToDelete.map(address => address -> DeleteAccount) //Delete all accounts to be deleted
        val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)
        expectedStateRoot shouldBe InMemoryWorldStateProxy.persistState(resultingWorldState).stateRootHash

        //Check valid gasUsed
        resultingGasUsed shouldBe stx.tx.gasLimit

        //Check valid receipts
        resultingReceipts.size shouldBe 1
        val Receipt(rootHashReceipt, gasUsedReceipt, logsBloomFilterReceipt, logsReceipt) = resultingReceipts.head
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

    val ledger = new LedgerImpl(new MockVM(c => createResult(
      context = c,
      gasUsed = UInt256(defaultGasLimit),
      gasLimit = UInt256(defaultGasLimit),
      gasRefund = UInt256.Zero,
      logs = defaultLogs,
      addressesToDelete = defaultAddressesToDelete,
      error = Some(OutOfGas)
    )), blockchainConfig)

    val txsExecResult = ledger.executeBlockTransactions(
      block,
      blockchain,
      blockchainStorages,
      (new Mocks.MockValidatorsAlwaysSucceed).signedTransactionValidator
    )

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
    val Receipt(rootHashReceipt, gasUsedReceipt, logsBloomFilterReceipt, logsReceipt) = resultingReceipts.head
    rootHashReceipt shouldBe expectedStateRoot
    gasUsedReceipt shouldBe resultingGasUsed
    logsBloomFilterReceipt shouldBe BloomFilter.create(Nil)
    logsReceipt shouldBe Nil

  }

  it should "correctly run executeBlock for a valid block without txs" in new BlockchainSetup {

    val minerOneOmmerReward = BigInt("156250000000000000")
    val minerTwoOmmersReward = BigInt("312500000000000000")
    val ommerThreeBlocksDifferenceReward = BigInt("3125000000000000000")
    val ommerFiveBlocksDifferenceReward = BigInt("1875000000000000000")

    val table = Table[Int, BigInt, BigInt, BigInt](
      ("ommersSize", "ommersBlockDifference", "rewardMinerForOmmers", "rewardOmmer"),
      (0, 0, 0, 0),
      (2, 5, minerTwoOmmersReward, ommerFiveBlocksDifferenceReward),
      (1, 3, minerOneOmmerReward, ommerThreeBlocksDifferenceReward)
    )

    forAll(table){ (ommersSize, ommersBlockDifference, rewardMinerForOmmers, rewardOmmer) =>

      val ommersAddresses = (0 until ommersSize).map(i => Address(i.toByte +: Hex.decode("10")))

      val changes = Seq(
        minerAddress -> UpdateBalance(blockchainConfig.blockReward),
        minerAddress -> UpdateBalance(UInt256(rewardMinerForOmmers))
      ) ++ ommersAddresses.map(ommerAddress => ommerAddress -> UpdateBalance(UInt256(rewardOmmer)))
      val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)

      val blockHeader: BlockHeader = validBlockHeader.copy(stateRoot = expectedStateRoot)
      val blockBodyWithOmmers = validBlockBodyWithNoTxs.copy(
        uncleNodesList = ommersAddresses.map(ommerAddress =>
          defaultBlockHeader.copy(number = blockHeader.number - ommersBlockDifference, beneficiary = ommerAddress.bytes)
        )
      )
      val block = Block(blockHeader, blockBodyWithOmmers)

      val ledger = new LedgerImpl(new MockVM(c => createResult(
        context = c,
        gasUsed = UInt256(defaultGasLimit),
        gasLimit = UInt256(defaultGasLimit),
        gasRefund = UInt256.Zero,
        logs = defaultLogs,
        addressesToDelete = defaultAddressesToDelete,
        error = Some(OutOfGas)
      )), blockchainConfig)

      val blockExecResult = ledger.executeBlock(block, blockchainStorages, new Mocks.MockValidatorsAlwaysSucceed)
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

    val changes = Seq(
      minerAddress -> UpdateBalance(blockchainConfig.blockReward) //Paying miner for block processing
    )
    val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)

    val blockHeader: BlockHeader = validBlockHeader.copy(stateRoot = expectedStateRoot)
    val block = Block(blockHeader, validBlockBodyWithNoTxs)

    val ledger = new LedgerImpl(new MockVM(c => createResult(
      context = c,
      gasUsed = UInt256(defaultGasLimit),
      gasLimit = UInt256(defaultGasLimit),
      gasRefund = UInt256.Zero,
      logs = defaultLogs,
      addressesToDelete = defaultAddressesToDelete,
      error = Some(OutOfGas)
    )), blockchainConfig)

    assert(seqFailingValidators.forall{ validators: Validators =>
      val blockExecResult = ledger.executeBlock(block, blockchainStorages, validators)
      blockExecResult.left.forall {
        case e: ValidationBeforeExecError => true
        case _ => false
      }
    })
  }

  it should "fail to run executeBlock if a block is invalid after executing it" in new BlockchainSetup {

    object validatorsFailsBlockValidatorWithReceipts extends Mocks.MockValidatorsAlwaysSucceed {
      override val blockValidator = new BlockValidator {
        override def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody) = Right(Block(blockHeader, blockBody))
        override def validateBlockAndReceipts(block: Block, receipts: Seq[Receipt]) = Left(BlockTransactionsHashError)
      }
    }

    val changes = Seq(
      minerAddress -> UpdateBalance(blockchainConfig.blockReward) //Paying miner for block processing
    )
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

      val blockHeader: BlockHeader = validBlockHeader.copy(gasUsed = cumulativeGasUsedBlock, stateRoot = stateRootHash)
      val block = Block(blockHeader, validBlockBodyWithNoTxs)

      val ledger = new LedgerImpl(new MockVM(c => createResult(
        context = c,
        gasUsed = UInt256(defaultGasLimit),
        gasLimit = UInt256(defaultGasLimit),
        gasRefund = UInt256.Zero,
        logs = defaultLogs,
        addressesToDelete = defaultAddressesToDelete,
        error = Some(OutOfGas)
      )), blockchainConfig)

      val blockExecResult = ledger.executeBlock(block, blockchainStorages, validators)

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

      val ledger = new LedgerImpl(new MockVM(c => createResult(
        context = c,
        gasUsed = UInt256(defaultGasLimit),
        gasLimit = UInt256(defaultGasLimit),
        gasRefund = UInt256.Zero
      )), blockchainConfig)

      val txsExecResult = ledger.executeBlockTransactions(
        block,
        blockchain,
        blockchainStorages,
        (new Mocks.MockValidatorsAlwaysSucceed).signedTransactionValidator
      )

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
        minerAddress -> UpdateBalance(minerPaymentForTx1),        //Miner reward for tx execution
        origin1Address -> UpdateBalance(-UInt256(stx1.tx.value)), //Discount tx.value from originAddress
        receiver1Address -> UpdateBalance(UInt256(stx1.tx.value)) //Increase tx.value to recevierAddress
      )
      val expectedStateRootTx1 = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changesTx1)

      val Receipt(rootHashReceipt1, gasUsedReceipt1, logsBloomFilterReceipt1, logsReceipt1) = receipt1
      rootHashReceipt1 shouldBe expectedStateRootTx1
      gasUsedReceipt1 shouldBe stx1.tx.gasLimit
      logsBloomFilterReceipt1 shouldBe BloomFilter.create(Nil)
      logsReceipt1 shouldBe Nil

      //Check receipt2
      val minerPaymentForTx2 = UInt256(stx2.tx.gasLimit * stx2.tx.gasPrice)
      val changesTx2 = Seq(
        origin2Address -> IncreaseNonce,
        origin2Address -> UpdateBalance(-minerPaymentForTx2),     //Origin payment for tx execution and nonce increase
        minerAddress -> UpdateBalance(minerPaymentForTx2),        //Miner reward for tx execution
        origin2Address -> UpdateBalance(-UInt256(stx2.tx.value)), //Discount tx.value from originAddress
        receiver2Address -> UpdateBalance(UInt256(stx2.tx.value)) //Increase tx.value to recevierAddress
      )
      val expectedStateRootTx2 = applyChanges(expectedStateRootTx1, blockchainStorages, changesTx2)

      val Receipt(rootHashReceipt2, gasUsedReceipt2, logsBloomFilterReceipt2, logsReceipt2) = receipt2
      rootHashReceipt2 shouldBe expectedStateRootTx2
      gasUsedReceipt2 shouldBe (stx1.tx.gasLimit + stx2.tx.gasLimit)
      logsBloomFilterReceipt2 shouldBe BloomFilter.create(Nil)
      logsReceipt2 shouldBe Nil

      //Check world
      InMemoryWorldStateProxy.persistState(resultingWorldState).stateRootHash shouldBe expectedStateRootTx2

      val changes = Seq(
        minerAddress -> UpdateBalance(blockchainConfig.blockReward)
      )
      val blockExpectedStateRoot = applyChanges(expectedStateRootTx2, blockchainStorages, changes)

      val blockWithCorrectStateAndGasUsed = block.copy(
        header = block.header.copy(stateRoot = blockExpectedStateRoot, gasUsed = gasUsedReceipt2)
      )
      assert(ledger.executeBlock(blockWithCorrectStateAndGasUsed, blockchainStorages, new Mocks.MockValidatorsAlwaysSucceed).isRight)
    }
  }

  it should "allow to create an account and not run out of gas before Homestead" in new TestSetup {

    val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)

    val stx = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = blockchainConfig.homesteadBlockNumber - 1)

    val ledger = new LedgerImpl(new MockVM(c => createResult(
      context = c,
      gasUsed = defaultGasLimit,
      gasLimit = defaultGasLimit,
      gasRefund = 0,
      error = None, returnData = ByteString("contract code")
    )), blockchainConfig)

    val txResult = ledger.executeTransaction(stx, header, worldWithMinerAndOriginAccounts)
    val postTxWorld = txResult.worldState

    val newContractAddress = {
      val hash = kec256(rlp.encode(RLPList(originAddress.bytes, initialOriginNonce)))
      Address(hash)
    }

    postTxWorld.accountExists(newContractAddress) shouldBe true
    postTxWorld.getCode(newContractAddress) shouldBe ByteString()
  }

  it should "run out of gas in contract creation after Homestead" in new TestSetup {

    val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)
    val stx = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = blockchainConfig.homesteadBlockNumber + 1)

    val ledger = new LedgerImpl(new MockVM(c => createResult(
      context = c,
      gasUsed = defaultGasLimit,
      gasLimit = defaultGasLimit,
      gasRefund = 0,
      error = None,
      returnData = ByteString("contract code")
    )), blockchainConfig)

    val txResult = ledger.executeTransaction(stx, header, worldWithMinerAndOriginAccounts)
    val postTxWorld = txResult.worldState

    val newContractAddress = {
      val hash = kec256(rlp.encode(RLPList(originAddress.bytes, initialOriginNonce)))
      Address(hash)
    }

    postTxWorld.accountExists(newContractAddress) shouldBe false
    postTxWorld.getCode(newContractAddress) shouldBe ByteString()
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
      val ledger = new LedgerImpl(mockVM, blockchainConfig)

      val txResult = ledger.executeTransaction(stx, defaultBlockHeader, initialWorld)

      txResult.logs.size shouldBe logsSize
    }
  }

  it should "correctly send the transaction input data whether it's a contract creation or not" in new TestSetup {

    val txPayload = ByteString("the payload")

    val table = Table[Option[Address], ByteString](
      ("Receiving Address", "Input Data"),
      (defaultTx.receivingAddress, txPayload),
      (None, ByteString.empty)
    )

    forAll(table) { (maybeReceivingAddress, inputData) =>

      val initialWorld = emptyWorld
        .saveAccount(originAddress, Account(nonce = UInt256(defaultTx.nonce), balance = UInt256.MaxValue))

      val mockVM = new MockVM((pc: Ledger.PC) => {
        pc.env.inputData shouldEqual inputData
        createResult(pc, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString("contract code"))
      })
      val ledger = new LedgerImpl(mockVM, blockchainConfig)

      val tx = defaultTx.copy(receivingAddress = maybeReceivingAddress, payload = txPayload)
      val stx = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))

      ledger.executeTransaction(stx, defaultBlockHeader, initialWorld)
    }
  }

  it should "should handle pre-existing and new destination accounts when processing a contract init transaction" in new TestSetup {

    val originAccount = Account(nonce = UInt256(0), balance = UInt256.MaxValue)
    val worldWithoutPreexistingAccount = emptyWorld.saveAccount(originAddress, originAccount)

    // In order to get contract address we need to increase the nonce as ledger will do within the first
    // steps of execution
    val contractAddress = worldWithoutPreexistingAccount
      .saveAccount(originAddress, originAccount.increaseNonce)
      .createAddress(originAddress)

    val preExistingAccount = Account(nonce = UInt256(defaultTx.nonce), balance = 1000)
    val worldWithPreexistingAccount = worldWithoutPreexistingAccount
      .saveAccount(contractAddress, preExistingAccount)

    val tx = defaultTx.copy(receivingAddress = None, value = 23)
    val stx = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))


    val table = Table[InMemoryWorldStateProxy, BigInt](
      ("Initial World", "Contract Account Balance"),
      (worldWithoutPreexistingAccount, tx.value),
      (worldWithPreexistingAccount, preExistingAccount.balance + tx.value)
    )

    forAll(table) { (initialWorld, contractAccountBalance) =>
      val mockVM = new MockVM((pc: Ledger.PC) => {
        pc.world.getGuaranteedAccount(contractAddress).balance shouldEqual contractAccountBalance
        createResult(pc, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString("contract code"))
      })
      val ledger = new LedgerImpl(mockVM, blockchainConfig)

      ledger.executeTransaction(stx, defaultBlockHeader, initialWorld)
    }
  }

  it should "create sender account if it does not exists" in new TestSetup {

    val inputData = ByteString("the payload")

    val newAccountKeyPair: AsymmetricCipherKeyPair = generateKeyPair()
    val newAccountAddress = Address(kec256(newAccountKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))

    val mockVM = new MockVM((pc: Ledger.PC) => {
      pc.env.inputData shouldEqual ByteString.empty
      createResult(pc, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString("contract code"))
    })
    val ledger = new LedgerImpl(mockVM, blockchainConfig)

    val tx: Transaction = defaultTx.copy(gasPrice = 0, receivingAddress = None, payload = inputData)
    val stx: SignedTransaction = SignedTransaction.sign(tx, newAccountKeyPair, Some(blockchainConfig.chainId))

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] = ledger.executeTransactions(
      Seq(stx),
      initialWorld,
      defaultBlockHeader,
      (new Mocks.MockValidatorsAlwaysSucceed).signedTransactionValidator)

    result shouldBe a[Right[_, BlockResult]]
    result.map(br => br.worldState.getAccount(newAccountAddress)) shouldBe Right(Some(Account(nonce = 1)))
  }

  trait TestSetup {
    val originKeyPair: AsymmetricCipherKeyPair = generateKeyPair()
    val receiverKeyPair: AsymmetricCipherKeyPair = generateKeyPair()
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

    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages

    val initialOriginBalance: UInt256 = 100000000
    val initialMinerBalance: UInt256 = 2000000

    val initialOriginNonce = defaultTx.nonce

    val defaultAddressesToDelete = Set(Address(Hex.decode("01")), Address(Hex.decode("02")), Address(Hex.decode("03")))
    val defaultLogs = Seq(defaultLog.copy(loggerAddress = defaultAddressesToDelete.head))
    val defaultGasPrice: UInt256 = 10
    val defaultGasLimit: UInt256 = 1000000
    val defaultValue: BigInt = 1000

    val emptyWorld = InMemoryWorldStateProxy(storagesInstance.storages, UInt256.Zero)

    val worldWithMinerAndOriginAccounts = InMemoryWorldStateProxy.persistState(emptyWorld
      .saveAccount(originAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))
      .saveAccount(receiverAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))
      .saveAccount(minerAddress, Account(balance = initialMinerBalance)))

    val initialWorld = InMemoryWorldStateProxy.persistState(
      defaultAddressesToDelete.foldLeft(worldWithMinerAndOriginAccounts){
        (recWorld, address) => recWorld.saveAccount(address, Account.empty(UInt256.Zero))
      }
    )
  }

  trait BlockchainSetup extends TestSetup {
    val blockchain = BlockchainImpl(storagesInstance.storages)
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
    storagesInstance.storages.totalDifficultyStorage.put(validBlockParentHeader.hash, 0)

    val validTx: Transaction = defaultTx.copy(
      nonce = initialOriginNonce,
      gasLimit = defaultGasLimit,
      value = defaultValue
    )
    val validStxSignedByOrigin: SignedTransaction = SignedTransaction.sign(validTx, originKeyPair, Some(blockchainConfig.chainId))
  }

}
