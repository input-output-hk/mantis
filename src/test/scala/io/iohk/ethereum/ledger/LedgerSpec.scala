package io.iohk.ethereum.ledger


import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.ledger.Ledger.{PC, PR}
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
                   error: Option[ProgramError],
                   returnData: ByteString = bEmpty): PR = ProgramResult(
    returnData = returnData,
    gasRemaining = gasLimit - gasUsed,
    world = context.world,
    Nil,
    Nil,
    gasRefund,
    error
  )


  "Ledger" should "correctly adjust gas used when refunding gas to the sender and paying for gas to the miner" in new TestSetup {

    val initialOriginBalance: UInt256 = 1000000
    val initialMinerBalance: UInt256 = 2000000

    val table = Table[UInt256, UInt256, Option[ProgramError], UInt256](
      ("gasUsed", "refundsFromVM", "maybeError", "balanceDelta"),
      (25000, 20000, None, (25000 / 2) * defaultGasPrice),
      (25000, 10000, None, (25000 - 10000) * 10),
      (125000, 10000, Some(OutOfGas), defaultGasLimit * defaultGasPrice),
      (125000, 100000, Some(OutOfGas), defaultGasLimit * defaultGasPrice)
    )

    forAll(table) { (gasUsed, gasRefund, error, balanceDelta) =>

      val initialWorld = emptyWorld
        .saveAccount(originAddress, Account(nonce = UInt256(defaultTx.nonce), balance = initialOriginBalance))
        .saveAccount(minerAddress, Account(balance = initialMinerBalance))

      val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit)
      val stx = SignedTransaction.sign(tx, keyPair)

      val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

      val mockVM = new MockVM(createResult(_, gasUsed, defaultGasLimit, gasRefund, error))
      val ledger = new LedgerImpl(mockVM)

      val postTxWorld = ledger.executeTransaction(stx, header, initialWorld).worldState

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
    val stx = SignedTransaction.sign(tx, keyPair)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

    val ledger = new LedgerImpl(new MockVM())

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
    val stx = SignedTransaction.sign(tx, keyPair)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

    val ledger = new LedgerImpl(new MockVM())

    val postTxWorld = ledger.executeTransaction(stx, header, initialWorld).worldState

    postTxWorld.getGuaranteedAccount(originAddress).nonce shouldBe (initialOriginNonce + 1)
  }

  it should "allow to create an account and not run out of gas before Homestead" in new TestSetup {
    val initialOriginBalance: UInt256 = 1000000

    val initialOriginNonce = defaultTx.nonce

    val initialWorld = emptyWorld
      .saveAccount(originAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))

    val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit, receivingAddress = None, payload = ByteString.empty)
    val stx = SignedTransaction.sign(tx, keyPair)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = Config.Blockchain.homesteadBlockNumber - 1)

    val ledger = new LedgerImpl(new MockVM(createResult(_, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString("contract code"))))

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
    val stx = SignedTransaction.sign(tx, keyPair)

    val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes, number = Config.Blockchain.homesteadBlockNumber + 1)

    val ledger = new LedgerImpl(new MockVM(createResult(_, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString("contract code"))))

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

    val emptyWorld = InMemoryWorldStateProxy(
      storagesInstance.storages,
      storagesInstance.storages.nodeStorage
    )

    val defaultGasPrice: UInt256 = 10
    val defaultGasLimit: UInt256 = 1000000
  }

}
