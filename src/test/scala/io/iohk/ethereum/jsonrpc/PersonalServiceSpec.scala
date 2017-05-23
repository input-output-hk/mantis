package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.DefaultPatience
import io.iohk.ethereum.domain.{Address, Blockchain}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors._
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.keystore.KeyStore.IOError
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
    val res = personal.listAccounts(ListAccountsRequest()).futureValue
    res shouldEqual Left(LogicError("boom!"))

    //TODO: remaining errors when unlockAccount is implemented
  }

  it should "return an error when trying to import an invalid key" in new TestSetup {
    val invalidKey = prvKey.tail
    val req = ImportRawKeyRequest(invalidKey, passphrase)
    val res = personal.importRawKey(req).futureValue
    res shouldEqual Left(InvalidKey)
  }

  trait TestSetup {
    val prvKey = ByteString(Hex.decode("7a44789ed3cd85861c0bbf9693c7e1de1862dd4396c390147ecf1275099c6e6f"))
    val address = Address(123)
    val passphrase = "aaa"

    val keyStore = mock[KeyStore]
    val blockchain = mock[Blockchain]
    val txPool = null
    val personal = new PersonalService(keyStore, blockchain, txPool)

    def array[T](arr: Array[T]): Matcher[Array[T]] =
      argThat((_: Array[T]) sameElements arr)
  }
}
