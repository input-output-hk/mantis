package io.iohk.ethereum.ledger


import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.ledger.Ledger.{PC, PR}
import io.iohk.ethereum.vm.{Storage, WorldStateProxy, _}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.spongycastle.crypto.params.ECPublicKeyParameters


class LedgerSpec extends FlatSpec with PropertyChecks with Matchers {

  class MockVM(runFn: PC => PR) extends VM {
    override def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] =
      runFn(context.asInstanceOf[PC]).asInstanceOf[ProgramResult[W, S]]
  }


  "Ledger" should "correctly adjust gas used when refunding gas to the sender and paying for gas to the miner" in {
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

    val gasPrice: UInt256 = 10
    val gasLimit: UInt256 = 1000000
    val initialOriginBalance: UInt256 = 1000000
    val initialMinerBalance: UInt256 = 2000000

    val table = Table[UInt256, UInt256, Option[ProgramError], UInt256](
      ("gasUsed", "refundsFromVM", "maybeError", "balanceDelta"),
      (25000, 20000, None, (25000 / 2) * gasPrice),
      (25000, 10000, None, (25000 - 10000) * 10),
      (125000, 10000, Some(OutOfGas), gasLimit * gasPrice),
      (125000, 100000, Some(OutOfGas), gasLimit * gasPrice)
    )

    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages

    val emptyWorld = InMemoryWorldStateProxy(
      storagesInstance.storages,
      storagesInstance.storages.nodeStorage
    )

    forAll(table) { (gasUsed, gasRefund, error, balanceDelta) =>

      val initialWorld = emptyWorld
        .saveAccount(originAddress, Account(nonce = UInt256(defaultTx.nonce), balance = initialOriginBalance))
        .saveAccount(minerAddress, Account(balance = initialMinerBalance))

      val tx = defaultTx.copy(gasPrice = gasPrice, gasLimit = gasLimit)
      val stx = SignedTransaction.sign(tx, keyPair)

      val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

      val result: PR = ProgramResult(
        returnData = bEmpty,
        gasRemaining = gasLimit - gasUsed,
        Ledger.updateSenderAccountBeforeExecution(stx, initialWorld),
        Nil,
        Nil,
        gasRefund,
        error
      )

      val mockVM = new MockVM(_ => result)
      val ledger = new Ledger(mockVM)

      val postTxWorld = ledger.executeTransaction(stx, header, initialWorld).worldState

      postTxWorld.getBalance(originAddress) shouldEqual (initialOriginBalance - balanceDelta)
      postTxWorld.getBalance(minerAddress) shouldEqual (initialMinerBalance + balanceDelta)
    }
  }

}
