package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.jsonrpc.JsonRpcErrors._
import org.spongycastle.util.encoders.Hex

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

object PersonalService {

  case class ImportRawKeyRequest(prvKey: String, passphrase: String)
  case class ImportRawKeyResponse(address: String)

  case class NewAccountRequest(passphrase: String)
  case class NewAccountResponse(address: String)

  case class ListAccountsRequest()
  case class ListAccountsResponse(addresses: List[String])

  val InvalidKey = InvalidParams("Invalid key provided, expected 32 bytes (64 hex digits)")
  val InvalidAddress = InvalidParams("Invalid address, expected 20 bytes (40 hex digits)")
  val InvalidPassphrase = LogicError("Could not decrypt key with given passphrase")
  val KeyNotFound = LogicError("No key found for the given address")

  val PrivateKeyLength = 32
}

class PersonalService(keyStore: KeyStore) {

  def importRawKey(req: ImportRawKeyRequest): ServiceResponse[ImportRawKeyResponse] = Future {
    for {
      prvKey <- Try(Hex.decode(req.prvKey)).toEither.left.map(_ => InvalidKey)
        .filterOrElse(_.length == PrivateKeyLength, InvalidKey)
      addr <- keyStore.importPrivateKey(prvKey, req.passphrase).left.map(handleError)
    } yield ImportRawKeyResponse(addr.toString)
  }

  def newAccount(req: NewAccountRequest): ServiceResponse[NewAccountResponse] = Future {
    keyStore.newAccount(req.passphrase)
      .map(addr => NewAccountResponse(addr.toString))
      .left.map(handleError)
  }

  def listAccounts(request: ListAccountsRequest): ServiceResponse[ListAccountsResponse] = Future {
    keyStore.listAccounts
      .map(addresses => ListAccountsResponse(addresses.map(_.toString)))
      .left.map(handleError)
  }


  private val handleError: PartialFunction[KeyStore.KeyStoreError, JsonRpcError] = {
    case KeyStore.WrongPassphrase => InvalidPassphrase
    case KeyStore.KeyNotFound => KeyNotFound
    case KeyStore.IOError(msg) => LogicError(msg)
  }
}
