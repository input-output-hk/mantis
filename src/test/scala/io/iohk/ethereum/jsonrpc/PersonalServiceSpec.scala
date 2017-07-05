package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.{NormalPatience, Timeouts}
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{Account, Address, Blockchain}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors._
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.keystore.{KeyStore, Wallet}
import io.iohk.ethereum.keystore.KeyStore.{DecryptionFailed, IOError}
import io.iohk.ethereum.transactions.PendingTransactionsManager.{AddTransactions, GetPendingTransactions, PendingTransaction, PendingTransactionsResponse}
import io.iohk.ethereum.utils.{MiningConfig, TxPoolConfig}
import org.scalamock.matchers.Matcher
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.duration.FiniteDuration


class PersonalServiceSpec extends FlatSpec with Matchers with MockFactory with ScalaFutures with NormalPatience {

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


    (keyStore.unlockAccount _).expects(*, *).returning(Left(KeyStore.DecryptionFailed))
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

    (appStateStorage.getBestBlockNumber _).expects().returning(1234)
    (blockchain.getAccount _).expects(address, BigInt(1234)).returning(Some(Account(nonce, 2 * txValue)))

    val req = SendTransactionWithPassphraseRequest(tx, passphrase)
    val res = personal.sendTransaction(req)

    txPool.expectMsg(GetPendingTransactions)
    txPool.reply(PendingTransactionsResponse(Nil))

    res.futureValue shouldEqual Right(SendTransactionWithPassphraseResponse(stx.hash))
    txPool.expectMsg(AddTransactions(stx))
  }

  it should "send a transaction when having pending txs from the same sender" in new TestSetup {
    val newTx = wallet.signTx(tx.toTransaction(nonce + 1))

    (keyStore.unlockAccount _ ).expects(address, passphrase)
      .returning(Right(wallet))

    (appStateStorage.getBestBlockNumber _).expects().returning(1234)
    (blockchain.getAccount _).expects(address, BigInt(1234)).returning(Some(Account(nonce, 2 * txValue)))

    val req = SendTransactionWithPassphraseRequest(tx, passphrase)
    val res = personal.sendTransaction(req)

    txPool.expectMsg(GetPendingTransactions)
    txPool.reply(PendingTransactionsResponse(Seq(PendingTransaction(stx, 0))))

    res.futureValue shouldEqual Right(SendTransactionWithPassphraseResponse(newTx.hash))
    txPool.expectMsg(AddTransactions(newTx))
  }

  it should "fail to send a transaction given a wrong passphrase" in new TestSetup {
    (keyStore.unlockAccount _ ).expects(address, passphrase)
      .returning(Left(KeyStore.DecryptionFailed))

    val req = SendTransactionWithPassphraseRequest(tx, passphrase)
    val res = personal.sendTransaction(req).futureValue

    res shouldEqual Left(InvalidPassphrase)
    txPool.expectNoMsg()
  }

  it should "send a transaction (given sender address and using an unlocked account)" in new TestSetup {
    (keyStore.unlockAccount _ ).expects(address, passphrase)
      .returning(Right(wallet))

    personal.unlockAccount(UnlockAccountRequest(address, passphrase)).futureValue

    (appStateStorage.getBestBlockNumber _).expects().returning(1234)
    (blockchain.getAccount _).expects(address, BigInt(1234)).returning(Some(Account(nonce, 2 * txValue)))

    val req = SendTransactionRequest(tx)
    val res = personal.sendTransaction(req)

    txPool.expectMsg(GetPendingTransactions)
    txPool.reply(PendingTransactionsResponse(Nil))

    res.futureValue shouldEqual Right(SendTransactionResponse(stx.hash))
    txPool.expectMsg(AddTransactions(stx))
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

  it should "sign a message when correct passphrase is sent" in new TestSetup {

    (keyStore.unlockAccount _ ).expects(address, passphrase)
      .returning(Right(wallet))

    val message = ByteString(Hex.decode("deadbeaf"))

    val r = ByteString(Hex.decode("d237344891a90a389b7747df6fbd0091da20d1c61adb961b4491a4c82f58dcd2"))
    val s = ByteString(Hex.decode("5425852614593caf3a922f48a6fe5204066dcefbf6c776c4820d3e7522058d00"))
    val v = ByteString(Hex.decode("1b"))

    val req = SignRequest(message, address, Some(passphrase))

    val res = personal.sign(req).futureValue
    res shouldEqual Right(SignResponse(ECDSASignature(r, s, v)))

    // Account should still be locked after calling sign with passphrase
    val txReq = SendTransactionRequest(tx)
    val txRes = personal.sendTransaction(txReq).futureValue
    txRes shouldEqual Left(AccountLocked)

  }

  it should "sign a message using an unlocked account" in new TestSetup {

    (keyStore.unlockAccount _ ).expects(address, passphrase)
      .returning(Right(wallet))

    val message = ByteString(Hex.decode("deadbeaf"))

    val r = ByteString(Hex.decode("d237344891a90a389b7747df6fbd0091da20d1c61adb961b4491a4c82f58dcd2"))
    val s = ByteString(Hex.decode("5425852614593caf3a922f48a6fe5204066dcefbf6c776c4820d3e7522058d00"))
    val v = ByteString(Hex.decode("1b"))

    val req = SignRequest(message, address, None)

    personal.unlockAccount(UnlockAccountRequest(address, passphrase)).futureValue
    val res = personal.sign(req).futureValue
    res shouldEqual Right(SignResponse(ECDSASignature(r, s, v)))
  }

  it should "return an error if signing a message using a locked account" in new TestSetup {

    val message = ByteString(Hex.decode("deadbeaf"))

    val req = SignRequest(message, address, None)

    val res = personal.sign(req).futureValue
    res shouldEqual Left(AccountLocked)
  }

  it should "return an error when signing a message if passphrase is wrong" in new TestSetup {

    val wrongPassphase = "wrongPassphrase"

    (keyStore.unlockAccount _ ).expects(address, wrongPassphase)
      .returning(Left(DecryptionFailed))

    val message = ByteString(Hex.decode("deadbeaf"))

    val req = SignRequest(message, address, Some(wrongPassphase))

    val res = personal.sign(req).futureValue
    res shouldEqual Left(InvalidPassphrase)
  }

  it should "return an error when signing if unexistent address is sent" in new TestSetup {

    (keyStore.unlockAccount _ ).expects(address, passphrase)
      .returning(Left(KeyStore.KeyNotFound))

    val message = ByteString(Hex.decode("deadbeaf"))

    val req = SignRequest(message, address, Some(passphrase))

    val res = personal.sign(req).futureValue
    res shouldEqual Left(KeyNotFound)
  }

  it should "recover address form signed message" in new TestSetup {
    val sigAddress = Address(ByteString(Hex.decode("12c2a3b877289050FBcfADC1D252842CA742BE81")))

    val message = ByteString(Hex.decode("deadbeaf"))

    val r: ByteString = ByteString(Hex.decode("117b8d5b518dc428d97e5e0c6f870ad90e561c97de8fe6cad6382a7e82134e61"))
    val s: ByteString = ByteString(Hex.decode("396d881ef1f8bc606ef94b74b83d76953b61f1bcf55c002ef12dd0348edff24b"))
    val v: ByteString = ByteString(Hex.decode("1b"))

    val req = EcRecoverRequest(message, ECDSASignature(r, s, v))

    val res = personal.ecRecover(req).futureValue
    res shouldEqual Right(EcRecoverResponse(sigAddress))
  }

  it should "allow to sign and recover the same message" in new TestSetup {

    (keyStore.unlockAccount _ ).expects(address, passphrase)
      .returning(Right(wallet))

    val message = ByteString(Hex.decode("deadbeaf"))

    personal.sign(SignRequest(message, address, Some(passphrase)))
      .futureValue.left.map(_ => fail())
      .map(response => EcRecoverRequest(message, response.signature))
      .foreach{ req =>
        val res = personal.ecRecover(req).futureValue
        res shouldEqual Right(EcRecoverResponse(address))
      }

  }

  trait TestSetup {
    val prvKey = ByteString(Hex.decode("7a44789ed3cd85861c0bbf9693c7e1de1862dd4396c390147ecf1275099c6e6f"))
    val address = Address(Hex.decode("aa6826f00d01fe4085f0c3dd12778e206ce4e2ac"))
    val passphrase = "aaa"

    val nonce = 7
    val txValue = 128000

    val wallet = Wallet(address, prvKey)
    val tx = TransactionRequest(from = address, to = Some(Address(42)), value = Some(txValue))
    val stx = wallet.signTx(tx.toTransaction(nonce))

    implicit val system = ActorSystem("personal-service-test")

    val txPoolConfig = new TxPoolConfig {
      override val txPoolSize: Int = 30
      override val pendingTxManagerQueryTimeout: FiniteDuration = Timeouts.normalTimeout
    }

    val keyStore = mock[KeyStore]
    val blockchain = mock[Blockchain]
    val txPool = TestProbe()
    val appStateStorage = mock[AppStateStorage]
    val personal = new PersonalService(keyStore, blockchain, txPool.ref, appStateStorage, txPoolConfig)

    def array[T](arr: Array[T]): Matcher[Array[T]] =
      argThat((_: Array[T]) sameElements arr)
  }
}
