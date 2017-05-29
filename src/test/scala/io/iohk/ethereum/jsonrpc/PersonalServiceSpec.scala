package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.DefaultPatience
import io.iohk.ethereum.domain.{Account, Address, Blockchain}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors._
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.keystore.{KeyStore, Wallet}
import io.iohk.ethereum.keystore.KeyStore.IOError
import io.iohk.ethereum.transactions.PendingTransactionsManager.AddTransaction
import org.scalamock.matchers.Matcher
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex


class PersonalServiceSpec extends FlatSpec with Matchers with MockFactory with ScalaFutures with DefaultPatience {

  "PersonalService" should "import private keys" in new TestSetup {
    (keyStore.importPrivateKey _).expects(prvKey, passphrase).returning(Right(address))

    val req = ImportRawKeyRequest(prvKey, passphrase)
    val res = personal.importRawKey(req).futureValue

    res shouldEqual Right(ImportRawKeyResponse(address))
  }

  it should "create new accounts" in new TestSetup {
    (keyStore.newAccount _).expects(passphrase).returning(Right(address))

    val req = NewAccountRequest(passphrase)
    val res = personal.newAccount(req).futureValue

    res shouldEqual Right(NewAccountResponse(address))
  }

  it should "list accounts" in new TestSetup {
    val addresses = List(123, 42, 1).map(Address(_))
    (keyStore.listAccounts _).expects().returning(Right(addresses))

    val res = personal.listAccounts(ListAccountsRequest()).futureValue

    res shouldEqual Right(ListAccountsResponse(addresses))
  }

  it should "translate KeyStore errors to JsonRpc errors" in new TestSetup {
    (keyStore.listAccounts _).expects().returning(Left(IOError("boom!")))
    val res1 = personal.listAccounts(ListAccountsRequest()).futureValue
    res1 shouldEqual Left(LogicError("boom!"))

    (keyStore.unlockAccount _).expects(*, *).returning(Left(KeyStore.KeyNotFound))
    val res2 = personal.unlockAccount(UnlockAccountRequest(Address(42), "passphrase")).futureValue
    res2 shouldEqual Left(KeyNotFound)


    (keyStore.unlockAccount _).expects(*, *).returning(Left(KeyStore.WrongPassphrase))
    val res3 = personal.unlockAccount(UnlockAccountRequest(Address(42), "passphrase")).futureValue
    res3 shouldEqual Left(InvalidPassphrase)
  }

  it should "return an error when trying to import an invalid key" in new TestSetup {
    val invalidKey = prvKey.tail
    val req = ImportRawKeyRequest(invalidKey, passphrase)
    val res = personal.importRawKey(req).futureValue
    res shouldEqual Left(InvalidKey)
  }

  it should "unlock an account given a correct passphrase" in new TestSetup {
    (keyStore.unlockAccount _ ).expects(address, passphrase).returning(Right(wallet))

    val req = UnlockAccountRequest(address, passphrase)
    val res = personal.unlockAccount(req).futureValue

    res shouldEqual Right(UnlockAccountResponse(true))
  }

  it should "send a transaction (given sender address and a passphrase)" in new TestSetup {
    (keyStore.unlockAccount _ ).expects(address, passphrase)
      .returning(Right(wallet))

    (blockchain.getAccount _).expects(address).returning(Some(Account(nonce, 2 * txValue)))

    val req = SendTransactionWithPassphraseRequest(tx, passphrase)
    val res = personal.sendTransaction(req).futureValue

    res shouldEqual Right(SendTransactionWithPassphraseResponse(stx.hash))
    txPool.expectMsg(AddTransaction(stx))
  }

  it should "fail to send a transaction given a wrong passphrase" in new TestSetup {
    (keyStore.unlockAccount _ ).expects(address, passphrase)
      .returning(Left(KeyStore.WrongPassphrase))

    val req = SendTransactionWithPassphraseRequest(tx, passphrase)
    val res = personal.sendTransaction(req).futureValue

    res shouldEqual Left(InvalidPassphrase)
    txPool.expectNoMsg()
  }

  it should "send a transaction (given sender address and using an unlocked account)" in new TestSetup {
    (keyStore.unlockAccount _ ).expects(address, passphrase)
      .returning(Right(wallet))

    personal.unlockAccount(UnlockAccountRequest(address, passphrase)).futureValue

    (blockchain.getAccount _).expects(address).returning(Some(Account(nonce, 2 * txValue)))

    val req = SendTransactionRequest(tx)
    val res = personal.sendTransaction(req).futureValue

    res shouldEqual Right(SendTransactionResponse(stx.hash))
    txPool.expectMsg(AddTransaction(stx))
  }

  it should "fail to send a transaction when account is locked" in new TestSetup {
    val req = SendTransactionRequest(tx)
    val res = personal.sendTransaction(req).futureValue

    res shouldEqual Left(AccountLocked)
    txPool.expectNoMsg()
  }

  it should "lock an unlocked account" in new TestSetup {
    (keyStore.unlockAccount _ ).expects(address, passphrase)
      .returning(Right(wallet))

    personal.unlockAccount(UnlockAccountRequest(address, passphrase)).futureValue

    val lockRes = personal.lockAccount(LockAccountRequest(address)).futureValue
    val txRes = personal.sendTransaction(SendTransactionRequest(tx)).futureValue

    lockRes shouldEqual Right(LockAccountResponse(true))
    txRes shouldEqual Left(AccountLocked)
  }

  trait TestSetup {
    val prvKey = ByteString(Hex.decode("7a44789ed3cd85861c0bbf9693c7e1de1862dd4396c390147ecf1275099c6e6f"))
    val address = Address(123)
    val passphrase = "aaa"

    val nonce = 7
    val txValue = 128000

    val wallet = Wallet(address, prvKey)
    val tx = TransactionRequest(from = address, to = Some(Address(42)), value = Some(txValue))
    val stx = wallet.signTx(tx.toTransaction(nonce))

    implicit val system = ActorSystem("personal-service-test")

    val keyStore = mock[KeyStore]
    val blockchain = mock[Blockchain]
    val txPool = TestProbe()
    val personal = new PersonalService(keyStore, blockchain, txPool.ref)

    def array[T](arr: Array[T]): Matcher[Array[T]] =
      argThat((_: Array[T]) sameElements arr)
  }
}
