package io.iohk.ethereum.ledger


import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.{Fixtures, rlp}
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
      val stx = SignedTransaction.sign(tx, keyPair)

      val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

      val mockVM = new MockVM(c => createResult(
        context = c,
        gasUsed = gasUsed,
        gasLimit = defaultGasLimit,
        gasRefund = gasRefund,
        error = error
      ))
      val ledger = new LedgerImpl(mockVM)

      val postTxWorld = ledger.executeTransaction(stx, header, initialWorld).worldState

      postTxWorld.getBalance(originAddress) shouldEqual (initialOriginBalance - balanceDelta)
      postTxWorld.getBalance(minerAddress) shouldEqual (initialMinerBalance + balanceDelta)
    }
  }

  it should "correctly change the nonce when executing a tx that results in contract creation" in new TestSetup {

    val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)
    val stx = SignedTransaction.sign(tx, keyPair)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

    val ledger = new LedgerImpl(new MockVM())

    val postTxWorld = ledger.executeTransaction(stx, header, initialWorld).worldState

    postTxWorld.getGuaranteedAccount(originAddress).nonce shouldBe (initialOriginNonce + 1)
  }

  it should "correctly change the nonce when executing a tx that results in a message call" in new TestSetup {

    val tx = defaultTx.copy(
      gasPrice = defaultGasPrice, gasLimit = defaultGasLimit,
      receivingAddress = Some(originAddress), payload = ByteString.empty
    )
    val stx = SignedTransaction.sign(tx, keyPair)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

    val ledger = new LedgerImpl(new MockVM())

    val postTxWorld = ledger.executeTransaction(stx, header, initialWorld).worldState

    postTxWorld.getGuaranteedAccount(originAddress).nonce shouldBe (initialOriginNonce + 1)
  }

  it should "correctly run executeBlockTransactions for a block without txs" in new BlockchainSetup {

    val validBlock = Block(validBlockHeader, validBlockBody)

    val ledger = new LedgerImpl(new MockVM(c => createResult(context = c, gasUsed = defaultGasLimit, gasLimit = defaultGasLimit, gasRefund = 0)))

    val txsExecResult = ledger.executeBlockTransactions(
      validBlock,
      blockchain,
      storagesInstance.storages,
      Fixtures.EmptyValidators
    )

    assert(txsExecResult.isRight)
    val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.right.get
    resultingGasUsed shouldBe 0
    resultingReceipts shouldBe Nil
    InMemoryWorldStateProxy.persistState(resultingWorldState).stateRootHash shouldBe validBlockHeader.stateRoot
  }

  it should "correctly run executeBlockTransactions for a block with one tx (that produces no errors)" in new BlockchainSetup {

    //FIXME: Try more examples: Non-empty logs, non empty addresses to delete, execution that should fail (change after link.. merge)
    val table = Table[BigInt, Seq[TxLogEntry], Seq[Address], Boolean](
      ("gasUsed", "logs", "addressesToDelete", "executionShouldNotFail"),
      (defaultGasUsed, Nil, Nil, true)
    )

    val validBlockBodyWithTxs: BlockBody = validBlockBody.copy(
      transactionList = Seq(validStx)
    )
    val validBlock = Block(validBlockHeader, validBlockBodyWithTxs)

    forAll(table) { (gasUsed, logs, addressesToDelete, executionShouldNotFail) =>

      val ledger = new LedgerImpl(new MockVM(c => createResult(
        context = c,
        gasUsed = UInt256(gasUsed),
        gasLimit = UInt256(validStx.tx.gasLimit),
        gasRefund = 0,
        logs = logs,
        addressesToDelete = addressesToDelete
      )))

      val txsExecResult = ledger.executeBlockTransactions(
        validBlock,
        blockchain,
        blockchainStorages,
        Fixtures.EmptyValidators
      )

      txsExecResult.isRight shouldBe executionShouldNotFail
      if(txsExecResult.isRight){
        val BlockResult(resultingWorldState, resultingGasUsed, resultingReceipts) = txsExecResult.right.get

        //Check valid world
        val persistedWorld = InMemoryWorldStateProxy.persistState(resultingWorldState)
        addressesToDelete.forall{ persistedWorld.getAccount(_).isEmpty} shouldBe true
        //FIXME: Check payments done (in persistedWorld)

        resultingGasUsed shouldBe gasUsed

        //Check valid receipts
        resultingReceipts.size shouldBe 1 //Check in more detail
        val Receipt(rootHashReceipt, gasUsedReceipt, logsBloomFilterReceipt, logsReceipt) = resultingReceipts.head
        rootHashReceipt shouldBe persistedWorld.stateRootHash
        gasUsedReceipt shouldBe resultingGasUsed
        logsBloomFilterReceipt shouldBe BloomFilter.create(logs)
        logsReceipt shouldBe logs
      }

    }
  }

  it should "correctly run executeBlockTransactions for a block with one tx (that produces OutOfGas)" in new BlockchainSetup {}

  it should "correctly run executeBlockTransactions for a block with more than one tx" in new BlockchainSetup {}
    //FIXME: Try with same receiver and with different ones

  it should "fail running executeBlockTransactions on a block with an invalid tx" in new TestSetup {}
    //FIXME: Try with 2 txs and check that nothing changed in world

  it should "correctly run executeBlock for a valid block without txs" in new TestSetup {}

  it should "correctly run executeBlock for a valid block with txs" in new TestSetup {}

  it should "fail to run executeBlock if a block is invalid before executing it" in new TestSetup {}

  it should "fail to run executeBlock if a block is invalid after executing it" in new TestSetup {}

  it should "correctly run executeBlockTransactions if an OutOfGas error where to happen" in new TestSetup {} //FIXME: Or in more scenarios?

  it should "allow to create an account and not run out of gas before Homestead" in new TestSetup {

    val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)
    val stx = SignedTransaction.sign(tx, keyPair)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = Config.Blockchain.homesteadBlockNumber - 1)

    val ledger = new LedgerImpl(new MockVM(c => createResult(
      context = c,
      gasUsed = defaultGasLimit,
      gasLimit = defaultGasLimit,
      gasRefund = 0,
      error = None, returnData = ByteString("contract code")
    )))

    val txResult = ledger.executeTransaction(stx, header, initialWorld)
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
    val stx = SignedTransaction.sign(tx, keyPair)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = Config.Blockchain.homesteadBlockNumber + 1)

    val ledger = new LedgerImpl(new MockVM(c => createResult(
      context = c,
      gasUsed = defaultGasLimit,
      gasLimit = defaultGasLimit,
      gasRefund = 0,
      error = None,
      returnData = ByteString("contract code")
    )))

    val txResult = ledger.executeTransaction(stx, header, initialWorld)
    val postTxWorld = txResult.worldState

    val newContractAddress = {
      val hash = kec256(rlp.encode(RLPList(originAddress.bytes, initialOriginNonce)))
      Address(hash)
    }

    postTxWorld.accountExists(newContractAddress) shouldBe false
    postTxWorld.getCode(newContractAddress) shouldBe ByteString()
  }

  trait TestSetup {
    val keyPair = generateKeyPair()
    val originAddress = Address(kec256(keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false)))
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
      receivingAddress = Address(123),
      value = 0,
      payload = ByteString.empty)

    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages

    val initialOriginBalance: UInt256 = 1000000
    val initialMinerBalance: UInt256 = 2000000

    val initialOriginNonce = defaultTx.nonce

    val emptyWorld = InMemoryWorldStateProxy(
      storagesInstance.storages,
      storagesInstance.storages.nodeStorage
    )

    val initialWorld = InMemoryWorldStateProxy.persistState(emptyWorld
      .saveAccount(originAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))
      .saveAccount(minerAddress, Account(balance = initialMinerBalance)))

    val defaultGasPrice: UInt256 = 10
    val defaultGasLimit: UInt256 = 1000000
    val defaultGasUsed: UInt256 = defaultGasLimit / 2
    val defaultGasRemaining = defaultGasLimit - defaultGasUsed
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
      unixTimestamp = validBlockParentHeader.unixTimestamp + 1
    )
    val validBlockBody: BlockBody = BlockBody(Nil, Nil)

    blockchain.save(validBlockParentHeader)
    storagesInstance.storages.totalDifficultyStorage.put(validBlockParentHeader.hash, 0)

    def validTx: Transaction = defaultTx.copy(
      nonce = initialOriginNonce,
      gasLimit = defaultGasLimit
    )
    def validStx: SignedTransaction = SignedTransaction.sign(validTx, keyPair)
  }

}
