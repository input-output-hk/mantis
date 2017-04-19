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
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.ledger.Ledger.{BlockResult, PC, PR}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.vm._
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

  def createResult(context: PC,
                   gasUsed: UInt256,
                   gasLimit: UInt256,
                   gasRefund: UInt256,
                   addressesToDelete: Seq[Address] = Nil,
                   logs: Seq[TxLogEntry] = Nil,
                   returnData: ByteString = bEmpty,
                   error: Option[ProgramError] = None): PR =
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
    val initialWorld = InMemoryWorldStateProxy(blockchainStorages, blockchainStorages.nodeStorage, Some(stateRootHash))
    val newWorld = changes.foldLeft[InMemoryWorldStateProxy](initialWorld){ case (recWorld, (address, change)) =>
        change match {
          case UpdateBalance(balanceIncrease) =>
            val accountWithBalanceIncrease = recWorld.getAccount(address).getOrElse(Account.Empty).increaseBalance(balanceIncrease)
            recWorld.saveAccount(address, accountWithBalanceIncrease)
          case IncreaseNonce =>
            val accountWithNonceIncrease = recWorld.getAccount(address).getOrElse(Account.Empty).increaseNonce
            recWorld.saveAccount(address, accountWithNonceIncrease)
          case DeleteAccount =>
            recWorld.deleteAccount(address)
        }
    }
    InMemoryWorldStateProxy.persistState(newWorld).stateRootHash
  }

  "Ledger" should "correctly adjust gas used when refunding gas to the sender and paying for gas to the miner" in new TestSetup {

    val table = Table[UInt256, UInt256, Option[ProgramError], UInt256](
      ("gasUsed", "refundsFromVM", "maybeError", "balanceDelta"),
      (25000, 20000, None, (25000 / 2) * defaultGasPrice),
      (25000, 10000, None, (25000 - 10000) * 10),
      (125000, 10000, Some(OutOfGas), defaultGasLimit * defaultGasPrice),
      (125000, 100000, Some(OutOfGas), defaultGasLimit * defaultGasPrice)
    )

    forAll(table) { (gasUsed, gasRefund, error, balanceDelta) =>

      val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit)
      val stx = SignedTransaction.sign(tx, originKeyPair)

      val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

      val mockVM = new MockVM(c => createResult(
        context = c,
        gasUsed = gasUsed,
        gasLimit = defaultGasLimit,
        gasRefund = gasRefund,
        error = error
      ))
      val ledger = new LedgerImpl(mockVM)

      val postTxWorld = ledger.executeTransaction(stx, header, worldWithMinerAndOriginAccounts).worldState

      postTxWorld.getBalance(originAddress) shouldEqual (initialOriginBalance - balanceDelta)
      postTxWorld.getBalance(minerAddress) shouldEqual (initialMinerBalance + balanceDelta)
    }
  }

  it should "correctly change the nonce when executing a tx that results in contract creation" in new TestSetup {

    val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)
    val stx = SignedTransaction.sign(tx, originKeyPair)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

    val ledger = new LedgerImpl(new MockVM())

    val postTxWorld = ledger.executeTransaction(stx, header, worldWithMinerAndOriginAccounts).worldState

    postTxWorld.getGuaranteedAccount(originAddress).nonce shouldBe (initialOriginNonce + 1)
  }

  it should "correctly change the nonce when executing a tx that results in a message call" in new TestSetup {

    val tx = defaultTx.copy(
      gasPrice = defaultGasPrice, gasLimit = defaultGasLimit,
      receivingAddress = Some(originAddress), payload = ByteString.empty
    )
    val stx = SignedTransaction.sign(tx, originKeyPair)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

    val ledger = new LedgerImpl(new MockVM())

    val postTxWorld = ledger.executeTransaction(stx, header, worldWithMinerAndOriginAccounts).worldState

    postTxWorld.getGuaranteedAccount(originAddress).nonce shouldBe (initialOriginNonce + 1)
  }

  it should "correctly run executeBlockTransactions for a block without txs" in new BlockchainSetup {

    val block = Block(validBlockHeader, validBlockBodyWithNoTxs)

    val ledger = new LedgerImpl(new MockVM(c => createResult(context = c, gasUsed = defaultGasLimit, gasLimit = defaultGasLimit, gasRefund = 0)))

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

    val table = Table[BigInt, Seq[TxLogEntry], Seq[Address], Boolean](
      ("gasLimit", "logs", "addressesToDelete", "txValidAccordingToValidators"),
      (defaultGasLimit, Nil, Nil, true),
      (defaultGasLimit / 2, Nil, defaultAddressesToDelete, true),
      (2 * defaultGasLimit, defaultLogs, Nil, true),
      (defaultGasLimit, defaultLogs, defaultAddressesToDelete, true),
      (defaultGasLimit, defaultLogs, defaultAddressesToDelete, false)
    )

    forAll(table) { (gasLimit, logs, addressesToDelete, txValidAccordingToValidators) =>

      val tx = validTx.copy(gasLimit = gasLimit)
      val stx = SignedTransaction.sign(tx, originKeyPair)

      val validUpdateBlockHeader: BlockHeader = validBlockHeader.copy(gasLimit = gasLimit)
      val validBlockBodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(stx))
      val block = Block(validUpdateBlockHeader, validBlockBodyWithTxs)

      val ledger = new LedgerImpl(new MockVM(c => createResult(
        context = c,
        gasUsed = UInt256(gasLimit),
        gasLimit = UInt256(gasLimit),
        gasRefund = UInt256.Zero,
        logs = logs,
        addressesToDelete = addressesToDelete
      )))

      val txsExecResult = ledger.executeBlockTransactions(
        block,
        blockchain,
        blockchainStorages,
        if(txValidAccordingToValidators) (new Mocks.MockValidatorsAlwaysSucceed).signedTransactionValidator
        else Mocks.MockValidatorsAlwaysFail.signedTransactionValidator
      )

      txsExecResult.isRight shouldBe txValidAccordingToValidators
      if(txsExecResult.isRight){
        val minerPaymentForTxs = stx.tx.gasLimit * stx.tx.gasPrice

        val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.right.get

        //Check valid world
        val changes = Seq(
          originAddress -> IncreaseNonce,
          originAddress -> UpdateBalance(-ledger.calculateUpfrontGas(stx.tx)),        //Origin payment for tx execution and nonce increase
          minerAddress -> UpdateBalance(UInt256(stx.tx.gasLimit * stx.tx.gasPrice)), //Miner reward for tx execution
          originAddress -> UpdateBalance(-UInt256(stx.tx.value)),                    //Discount tx.value from originAddress
          receiverAddress -> UpdateBalance(UInt256(stx.tx.value))                    //Increase tx.value to recevierAddress
        ) ++ addressesToDelete.map(address => address -> DeleteAccount)                              //Delete all accounts to be deleted
        val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)
        expectedStateRoot shouldBe InMemoryWorldStateProxy.persistState(resultingWorldState).stateRootHash

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

    val tx = validTx.copy(gasLimit = defaultGasLimit)
    val stx = SignedTransaction.sign(tx, originKeyPair)

    val validUpdateBlockHeader: BlockHeader = validBlockHeader.copy(
      gasLimit = defaultGasLimit,
      number = Config.Blockchain.homesteadBlockNumber + 10
    )
    val validBlockBodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(stx))
    val block = Block(validUpdateBlockHeader, validBlockBodyWithTxs)

    val ledger = new LedgerImpl(new MockVM(c => createResult(
      context = c,
      gasUsed = UInt256(defaultGasLimit),
      gasLimit = UInt256(defaultGasLimit),
      gasRefund = UInt256.Zero,
      logs = defaultLogs,
      addressesToDelete = defaultAddressesToDelete,
      error = Some(OutOfGas)
    )))

    val txsExecResult = ledger.executeBlockTransactions(
      block,
      blockchain,
      blockchainStorages,
      (new Mocks.MockValidatorsAlwaysSucceed).signedTransactionValidator
    )

    assert(txsExecResult.isRight)
    val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.right.get

    //Check valid world
    val changes = Seq(
      originAddress -> IncreaseNonce,
      originAddress -> UpdateBalance(-ledger.calculateUpfrontGas(stx.tx)),      //Origin payment for tx execution and nonce increase
      minerAddress -> UpdateBalance(UInt256(stx.tx.gasLimit * stx.tx.gasPrice)) //Miner reward for tx execution
    )
    val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)
    expectedStateRoot shouldBe InMemoryWorldStateProxy.persistState(resultingWorldState).stateRootHash

    resultingGasUsed shouldBe stx.tx.gasLimit

    //Check valid receipts
    resultingReceipts.size shouldBe 1
    val Receipt(rootHashReceipt, gasUsedReceipt, logsBloomFilterReceipt, logsReceipt) = resultingReceipts.head
    rootHashReceipt shouldBe expectedStateRoot
    gasUsedReceipt shouldBe resultingGasUsed
    logsBloomFilterReceipt shouldBe BloomFilter.create(defaultLogs)
    logsReceipt shouldBe defaultLogs

  }

  it should "correctly run executeBlock for a valid block without txs" in new BlockchainSetup {
    val tx = validTx.copy(gasLimit = defaultGasLimit)
    val stx = SignedTransaction.sign(tx, originKeyPair)

    val changes = Seq(
      minerAddress -> UpdateBalance(Config.Blockchain.blockReward) //Paying miner for block processing
    )
    val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)

    val validUpdateBlockHeader: BlockHeader = validBlockHeader.copy(
      gasLimit = defaultGasLimit,
      gasUsed = 0,
      stateRoot = expectedStateRoot,
      receiptsRoot = Account.EmptyStorageRootHash,
      logsBloom = BloomFilter.EmptyBloomFilter
    )
    val block = Block(validUpdateBlockHeader, validBlockBodyWithNoTxs)

    val ledger = new LedgerImpl(new MockVM(c => createResult(
      context = c,
      gasUsed = UInt256(defaultGasLimit),
      gasLimit = UInt256(defaultGasLimit),
      gasRefund = UInt256.Zero,
      logs = defaultLogs,
      addressesToDelete = defaultAddressesToDelete,
      error = Some(OutOfGas)
    )))

    val blockExecResult = ledger.executeBlock(block, blockchainStorages, new Mocks.MockValidatorsAlwaysSucceed)

    assert(blockExecResult.isRight)
  }

  it should "fail to run executeBlock if a block is invalid before executing it" in new BlockchainSetup {
    object validatorsFailsBlockValidator extends Mocks.MockValidatorsAlwaysSucceed {
      override val blockValidator = Mocks.MockValidatorsAlwaysFail.blockValidator
    }

    object validatorsFailsBlockHeaderValidator extends Mocks.MockValidatorsAlwaysSucceed {
      override val blockHeaderValidator = Mocks.MockValidatorsAlwaysFail.blockHeaderValidator
    }

    object validatorsFailsOmmersValidator extends Mocks.MockValidatorsAlwaysSucceed {
      override val ommersValidator = Mocks.MockValidatorsAlwaysFail.ommersValidator
    }

    val seqFailingValidators: Seq[Validators] = Seq(validatorsFailsBlockHeaderValidator, validatorsFailsBlockValidator, validatorsFailsOmmersValidator)

    val tx = validTx.copy(gasLimit = defaultGasLimit)
    val stx = SignedTransaction.sign(tx, originKeyPair)

    val changes = Seq(
      minerAddress -> UpdateBalance(Config.Blockchain.blockReward) //Paying miner for block processing
    )
    val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)

    val validUpdateBlockHeader: BlockHeader = validBlockHeader.copy(
      gasLimit = defaultGasLimit,
      gasUsed = 0,
      stateRoot = expectedStateRoot,
      receiptsRoot = Account.EmptyStorageRootHash,
      logsBloom = BloomFilter.EmptyBloomFilter
    )
    val block = Block(validUpdateBlockHeader, validBlockBodyWithNoTxs)

    val ledger = new LedgerImpl(new MockVM(c => createResult(
      context = c,
      gasUsed = UInt256(defaultGasLimit),
      gasLimit = UInt256(defaultGasLimit),
      gasRefund = UInt256.Zero,
      logs = defaultLogs,
      addressesToDelete = defaultAddressesToDelete,
      error = Some(OutOfGas)
    )))

    seqFailingValidators.forall{ validators: Validators =>

      val blockExecResult = ledger.executeBlock(block, blockchainStorages, validators)

      blockExecResult.left.forall {
        case e: ValidationBeforeExecError => true
        case _ => false
      }
    } shouldBe true
  }

  it should "fail to run executeBlock if a block is invalid after executing it" in new BlockchainSetup {

    object validatorsFailsBlockHeaderValidator extends Mocks.MockValidatorsAlwaysSucceed {
      override val blockValidator = new BlockValidator {
        override def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody) = Right(Block(blockHeader, blockBody))

        override def validateBlockAndReceipts(block: Block, receipts: Seq[Receipt]) = Left(BlockTransactionsHashError)
      }
    }

    val tx = validTx.copy(gasLimit = defaultGasLimit)
    val stx = SignedTransaction.sign(tx, originKeyPair)

    val changes = Seq(
      minerAddress -> UpdateBalance(Config.Blockchain.blockReward) //Paying miner for block processing
    )
    val expectedStateRoot = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changes)

    val table = Table[ByteString, BigInt, Validators](
      ("stateRootHash", "cumulativeGasUsedBlock", "validators"),
      (expectedStateRoot, 1, new Mocks.MockValidatorsAlwaysSucceed),
      (((expectedStateRoot.head + 1) & 0xFF).toByte +: expectedStateRoot.tail, 0, new Mocks.MockValidatorsAlwaysSucceed),
      (expectedStateRoot, 0, validatorsFailsBlockHeaderValidator)
    )

    forAll(table){ (stateRootHash, cumulativeGasUsedBlock, validators) =>

      val validUpdateBlockHeader: BlockHeader = validBlockHeader.copy(
        gasLimit = defaultGasLimit,
        gasUsed = cumulativeGasUsedBlock,
        stateRoot = stateRootHash,
        receiptsRoot = Account.EmptyStorageRootHash,
        logsBloom = BloomFilter.EmptyBloomFilter
      )
      val block = Block(validUpdateBlockHeader, validBlockBodyWithNoTxs)

      val ledger = new LedgerImpl(new MockVM(c => createResult(
        context = c,
        gasUsed = UInt256(defaultGasLimit),
        gasLimit = UInt256(defaultGasLimit),
        gasRefund = UInt256.Zero,
        logs = defaultLogs,
        addressesToDelete = defaultAddressesToDelete,
        error = Some(OutOfGas)
      )))

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

      def keyPair(address: Address): AsymmetricCipherKeyPair = {
        if(address == originAddress) originKeyPair
        else receiverKeyPair
      }

      val tx1 = validTx.copy(value = 100, receivingAddress = Some(receiver1Address), gasLimit = defaultGasLimit)
      val tx2 = validTx.copy(value = 50, receivingAddress = Some(receiver2Address), gasLimit = defaultGasLimit,
        nonce = validTx.nonce + (if(origin1Address == origin2Address) 1 else 0)
      )
      val stx1 = SignedTransaction.sign(tx1, keyPair(origin1Address))
      val stx2 = SignedTransaction.sign(tx2, keyPair(origin2Address))

      val validBlockBodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(stx1, stx2))
      val block = Block(validBlockHeader, validBlockBodyWithTxs)

      val ledger = new LedgerImpl(new MockVM(c => createResult(
        context = c,
        gasUsed = UInt256(defaultGasLimit),
        gasLimit = UInt256(defaultGasLimit),
        gasRefund = UInt256.Zero
      )))

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
      val changesTx1 = Seq(
        origin1Address -> IncreaseNonce,
        origin1Address -> UpdateBalance(-ledger.calculateUpfrontGas(stx1.tx)),        //Origin payment for tx execution and nonce increase
        minerAddress -> UpdateBalance(UInt256(stx1.tx.gasLimit * stx1.tx.gasPrice)), //Miner reward for tx execution
        origin1Address -> UpdateBalance(-UInt256(stx1.tx.value)),                    //Discount tx.value from originAddress
        receiver1Address -> UpdateBalance(UInt256(stx1.tx.value))                    //Increase tx.value to recevierAddress
      )
      val expectedStateRootTx1 = applyChanges(validBlockParentHeader.stateRoot, blockchainStorages, changesTx1)

      val Receipt(rootHashReceipt1, gasUsedReceipt1, logsBloomFilterReceipt1, logsReceipt1) = receipt1
      rootHashReceipt1 shouldBe expectedStateRootTx1
      gasUsedReceipt1 shouldBe stx1.tx.gasLimit
      logsBloomFilterReceipt1 shouldBe BloomFilter.create(Nil)
      logsReceipt1 shouldBe Nil

      //Check receipt2
      val changesTx2 = Seq(
        origin2Address -> IncreaseNonce,
        origin2Address -> UpdateBalance(-ledger.calculateUpfrontGas(stx2.tx)),        //Origin payment for tx execution and nonce increase
        minerAddress -> UpdateBalance(UInt256(stx2.tx.gasLimit * stx2.tx.gasPrice)), //Miner reward for tx execution
        origin2Address -> UpdateBalance(-UInt256(stx2.tx.value)),                    //Discount tx.value from originAddress
        receiver2Address -> UpdateBalance(UInt256(stx2.tx.value))                    //Increase tx.value to recevierAddress
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
        minerAddress -> UpdateBalance(Config.Blockchain.blockReward)
      )
      val blockExpectedStateRoot = applyChanges(expectedStateRootTx2, blockchainStorages, changes)

      val validBlock = block.copy(
        header = block.header.copy(
          stateRoot = blockExpectedStateRoot,
          gasUsed = gasUsedReceipt2
        )
      )
      assert(ledger.executeBlock(validBlock, blockchainStorages, new Mocks.MockValidatorsAlwaysSucceed).isRight)
    }
  }

  it should "allow to create an account and not run out of gas before Homestead" in new TestSetup {

    val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)
    val stx = SignedTransaction.sign(tx, originKeyPair)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = Config.Blockchain.homesteadBlockNumber - 1)

    val ledger = new LedgerImpl(new MockVM(c => createResult(
      context = c,
      gasUsed = defaultGasLimit,
      gasLimit = defaultGasLimit,
      gasRefund = 0,
      error = None, returnData = ByteString("contract code")
    )))

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
    val stx = SignedTransaction.sign(tx, originKeyPair)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = Config.Blockchain.homesteadBlockNumber + 1)

    val ledger = new LedgerImpl(new MockVM(c => createResult(
      context = c,
      gasUsed = defaultGasLimit,
      gasLimit = defaultGasLimit,
      gasRefund = 0,
      error = None,
      returnData = ByteString("contract code")
    )))

    val txResult = ledger.executeTransaction(stx, header, worldWithMinerAndOriginAccounts)
    val postTxWorld = txResult.worldState

    val newContractAddress = {
      val hash = kec256(rlp.encode(RLPList(originAddress.bytes, initialOriginNonce)))
      Address(hash)
    }

    postTxWorld.accountExists(newContractAddress) shouldBe false
    postTxWorld.getCode(newContractAddress) shouldBe ByteString()
  }

  trait TestSetup {
    val originKeyPair = generateKeyPair()
    val receiverKeyPair = generateKeyPair()
    val originAddress = Address(kec256(originKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false)))
    val receiverAddress = Address(kec256(receiverKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false)))
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
      number = Config.Blockchain.homesteadBlockNumber + 1,
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

    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages

    val initialOriginBalance: UInt256 = 100000000
    val initialMinerBalance: UInt256 = 2000000

    val initialOriginNonce = defaultTx.nonce

    val emptyWorld = InMemoryWorldStateProxy(
      storagesInstance.storages,
      storagesInstance.storages.nodeStorage
    )

    val defaultAddressesToDelete = Seq(Address(Hex.decode("01")), Address(Hex.decode("02")), Address(Hex.decode("03")))
    val defaultLogs = Seq(TxLogEntry(defaultAddressesToDelete.head, Seq(ByteString.empty, ByteString.empty), ByteString.empty))
    val defaultGasPrice: UInt256 = 10
    val defaultGasLimit: UInt256 = 1000000
    val defaultGasUsed: UInt256 = defaultGasLimit / 2
    val defaultGasRemaining = defaultGasLimit - defaultGasUsed
    val defaultValue: BigInt = 1000

    val worldWithMinerAndOriginAccounts = InMemoryWorldStateProxy.persistState(emptyWorld
      .saveAccount(originAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))
      .saveAccount(receiverAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))
      .saveAccount(minerAddress, Account(balance = initialMinerBalance)))

    val initialWorld = InMemoryWorldStateProxy.persistState(
      defaultAddressesToDelete.foldLeft(worldWithMinerAndOriginAccounts){
        (recWorld, address) => recWorld.saveAccount(address, Account.Empty)
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
      number = validBlockParentHeader.number,
      unixTimestamp = validBlockParentHeader.unixTimestamp + 1,
      beneficiary = minerAddress.bytes
    )
    val validBlockBodyWithNoTxs: BlockBody = BlockBody(Nil, Nil)

    blockchain.save(validBlockParentHeader)
    storagesInstance.storages.totalDifficultyStorage.put(validBlockParentHeader.hash, 0)

    def validTx: Transaction = defaultTx.copy(
      nonce = initialOriginNonce,
      gasLimit = defaultGasLimit,
      value = defaultValue
    )
    def validStx: SignedTransaction = SignedTransaction.sign(validTx, originKeyPair)
  }

}
