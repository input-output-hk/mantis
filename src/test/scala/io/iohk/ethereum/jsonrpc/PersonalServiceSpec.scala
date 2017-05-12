package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.JsonRpcErrors._
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.keystore.KeyStore.IOError
import org.scalamock.matchers.Matcher
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex


class PersonalServiceSpec extends FlatSpec with Matchers with MockFactory with ScalaFutures {

  "PersonalService" should "import private keys" in new TestSetup {
    (keyStore.importPrivateKey _).expects(array(Hex.decode(prvKey)), passphrase).returning(Right(address))

    val req = ImportRawKeyRequest(prvKey, passphrase)
    val res = personal.importRawKey(req).futureValue

    res shouldEqual Right(ImportRawKeyResponse(address.toString))
  }

  it should "create new accounts" in new TestSetup {
    (keyStore.newAccount _).expects(passphrase).returning(Right(address))

    val req = NewAccountRequest(passphrase)
    val res = personal.newAccount(req).futureValue

    res shouldEqual Right(NewAccountResponse(address.toString))
  }

  it should "list accounts" in new TestSetup {
    val addresses = List(123, 42, 1).map(Address(_))
    (keyStore.listAccounts _).expects().returning(Right(addresses))

    val res = personal.listAccounts(ListAccountsRequest()).futureValue

    res shouldEqual Right(ListAccountsResponse(addresses.map(_.toString)))
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
    val prvKey = "7a44789ed3cd85861c0bbf9693c7e1de1862dd4396c390147ecf1275099c6e6f"
    val address = Address(123)
    val passphrase = "aaa"

    val keyStore = mock[KeyStore]
    val personal = new PersonalService(keyStore)

    def array[T](arr: Array[T]): Matcher[Array[T]] =
      argThat((_: Array[T]) sameElements arr)
  }
}
