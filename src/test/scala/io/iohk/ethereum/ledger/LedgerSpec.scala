package io.iohk.ethereum.ledger


import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import io.iohk.ethereum.ledger.Ledger.{PC, PR}
import io.iohk.ethereum.vm.{UInt256, _}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.spongycastle.crypto.params.ECPublicKeyParameters
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import org.spongycastle.util.encoders.Hex

class LedgerSpec extends FlatSpec with PropertyChecks with Matchers {

  val blockchainConfig = BlockchainConfig(Config.config)

  def createResult(context: PC,
                   gasUsed: UInt256,
                   gasLimit: UInt256,
                   gasRefund: UInt256,
                   error: Option[ProgramError],
                   returnData: ByteString = bEmpty,
                   logs: Seq[TxLogEntry] = Nil): PR = ProgramResult(
    returnData = returnData,
    gasRemaining = gasLimit - gasUsed,
    world = context.world,
    Nil,
    logs = logs,
    gasRefund,
    error
  )


  "Ledger" should "correctly calculate the total gas refund to be returned to the sender and paying for gas to the miner" in new TestSetup {

    val initialOriginBalance: UInt256 = 1000000
    val initialMinerBalance: UInt256 = 2000000

    val table = Table[UInt256, UInt256, Option[ProgramError], BigInt](
      ("execGasUsed", "refundsFromVM", "maybeError", "gasUsed"),
      (25000, 20000, None, 25000 - 12500),
      (25000, 10000, None, 25000 - 10000),
      (125000, 10000, Some(OutOfGas), defaultGasLimit),
      (125000, 100000, Some(OutOfGas), defaultGasLimit)
    )

    forAll(table) { (execGasUsed, gasRefundFromVM, error, gasUsed) =>

      val balanceDelta = UInt256(gasUsed * defaultGasPrice)

      val initialWorld = emptyWorld
        .saveAccount(originAddress, Account(nonce = UInt256(defaultTx.nonce), balance = initialOriginBalance))
        .saveAccount(minerAddress, Account(balance = initialMinerBalance))

      val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit)
      val stx = SignedTransaction.sign(tx, keyPair, blockchainConfig.chainId)

      val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

      val mockVM = new MockVM(createResult(_, execGasUsed, defaultGasLimit, gasRefundFromVM, error))
      val ledger = new LedgerImpl(mockVM, blockchainConfig)

      val execResult = ledger.executeTransaction(stx, header, initialWorld)
      val postTxWorld = execResult.worldState

      execResult.gasUsed shouldEqual gasUsed
      postTxWorld.getBalance(originAddress) shouldEqual (initialOriginBalance - balanceDelta)
      postTxWorld.getBalance(minerAddress) shouldEqual (initialMinerBalance + balanceDelta)
    }
  }

  it should "correctly change the nonce when executing a tx that results in contract creation" in new TestSetup {

    val initialOriginBalance: UInt256 = 1000000
    val initialMinerBalance: UInt256 = 2000000

    val initialOriginNonce = defaultTx.nonce

    val initialWorld = emptyWorld
      .saveAccount(originAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))
      .saveAccount(minerAddress, Account(balance = initialMinerBalance))

    val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)
    val stx = SignedTransaction.sign(tx, keyPair, blockchainConfig.chainId)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

    val ledger = new LedgerImpl(new MockVM(), blockchainConfig)

    val postTxWorld = ledger.executeTransaction(stx, header, initialWorld).worldState

    postTxWorld.getGuaranteedAccount(originAddress).nonce shouldBe (initialOriginNonce + 1)
  }

  it should "correctly change the nonce when executing a tx that results in a message call" in new TestSetup {

    val initialOriginBalance: UInt256 = 1000000
    val initialMinerBalance: UInt256 = 2000000

    val initialOriginNonce = defaultTx.nonce

    val initialWorld = emptyWorld
      .saveAccount(originAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))
      .saveAccount(minerAddress, Account(balance = initialMinerBalance))

    val tx = defaultTx.copy(
      gasPrice = defaultGasPrice, gasLimit = defaultGasLimit,
      receivingAddress = Some(originAddress), payload = ByteString.empty
    )
    val stx = SignedTransaction.sign(tx, keyPair, blockchainConfig.chainId)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

    val ledger = new LedgerImpl(new MockVM(), blockchainConfig)

    val postTxWorld = ledger.executeTransaction(stx, header, initialWorld).worldState

    postTxWorld.getGuaranteedAccount(originAddress).nonce shouldBe (initialOriginNonce + 1)
  }

  it should "allow to create an account and not run out of gas before Homestead" in new TestSetup {
    val initialOriginBalance: UInt256 = 1000000

    val initialOriginNonce = defaultTx.nonce

    val initialWorld = emptyWorld
      .saveAccount(originAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))

    val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)
    val stx = SignedTransaction.sign(tx, keyPair, blockchainConfig.chainId)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = blockchainConfig.homesteadBlockNumber - 1)

    val ledger = new LedgerImpl(new MockVM(createResult(_, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString("contract code"))), blockchainConfig)

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
    val initialOriginBalance: UInt256 = 1000000

    val initialOriginNonce = defaultTx.nonce

    val initialWorld = emptyWorld
      .saveAccount(originAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))

    val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)
    val stx = SignedTransaction.sign(tx, keyPair, blockchainConfig.chainId)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = blockchainConfig.homesteadBlockNumber + 1)

    val ledger = new LedgerImpl(new MockVM(createResult(_, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString("contract code"))), blockchainConfig)

    val txResult = ledger.executeTransaction(stx, header, initialWorld)
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

      val stx = SignedTransaction.sign(defaultTx, keyPair, blockchainConfig.chainId)

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
      val stx = SignedTransaction.sign(tx, keyPair, blockchainConfig.chainId)

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
    val stx = SignedTransaction.sign(tx, keyPair, blockchainConfig.chainId)


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


  trait TestSetup {
    val keyPair = generateKeyPair()
    //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of spongycastle encoding
    val originAddress = Address(kec256(keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))
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
      receivingAddress = Address(123),
      value = 0,
      payload = ByteString.empty)

    val defaultLog = TxLogEntry(
      loggerAddress = originAddress,
      logTopics = Seq(ByteString(Hex.decode("962cd36cf694aa154c5d3a551f19c98f356d906e96828eeb616e16fae6415738"))),
      data = ByteString(Hex.decode("1" * 128))
    )

    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages

    val emptyWorld = InMemoryWorldStateProxy(
      storagesInstance.storages,
      storagesInstance.storages.nodeStorage
    )

    val defaultGasPrice: UInt256 = 10
    val defaultGasLimit: UInt256 = 1000000
  }

}
