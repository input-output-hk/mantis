package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{Account, Address, Blockchain, BlockchainStorages}
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.keystore.{KeyStore, Wallet}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors._
import io.iohk.ethereum.transactions.PendingTransactionsManager.AddTransactions

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object PersonalService {

  case class ImportRawKeyRequest(prvKey: ByteString, passphrase: String)
  case class ImportRawKeyResponse(address: Address)

  case class NewAccountRequest(passphrase: String)
  case class NewAccountResponse(address: Address)

  case class ListAccountsRequest()
  case class ListAccountsResponse(addresses: List[Address])

  case class UnlockAccountRequest(address: Address, passphrase: String)
  case class UnlockAccountResponse(result: Boolean)

  case class LockAccountRequest(address: Address)
  case class LockAccountResponse(result: Boolean)

  case class SendTransactionWithPassphraseRequest(tx: TransactionRequest, passphrase: String)
  case class SendTransactionWithPassphraseResponse(txHash: ByteString)

  case class SendTransactionRequest(tx: TransactionRequest)
  case class SendTransactionResponse(txHash: ByteString)

  case class EcRecoverRequest(message: ByteString, signature: ECDSASignature)
  case class EcRecoverResponse(address: Address)

  val InvalidKey = InvalidParams("Invalid key provided, expected 32 bytes (64 hex digits)")
  val InvalidAddress = InvalidParams("Invalid address, expected 20 bytes (40 hex digits)")
  val InvalidPassphrase = LogicError("Could not decrypt key with given passphrase")
  val KeyNotFound = LogicError("No key found for the given address")

  val PrivateKeyLength = 32
}

class PersonalService(
  keyStore: KeyStore,
  blockchain: Blockchain,
  txPool: ActorRef,
  blockchainStorages: BlockchainStorages,
  appStateStorage: AppStateStorage) {

  private val unlockedWallets: mutable.Map[Address, Wallet] = mutable.Map.empty

  def importRawKey(req: ImportRawKeyRequest): ServiceResponse[ImportRawKeyResponse] = Future {
    for {
      prvKey <- Right(req.prvKey).filterOrElse(_.length == PrivateKeyLength, InvalidKey)
      addr <- keyStore.importPrivateKey(prvKey, req.passphrase).left.map(handleError)
    } yield ImportRawKeyResponse(addr)
  }

  def newAccount(req: NewAccountRequest): ServiceResponse[NewAccountResponse] = Future {
    keyStore.newAccount(req.passphrase)
      .map(NewAccountResponse.apply)
      .left.map(handleError)
  }

  def listAccounts(request: ListAccountsRequest): ServiceResponse[ListAccountsResponse] = Future {
    keyStore.listAccounts()
      .map(ListAccountsResponse.apply)
      .left.map(handleError)
  }

  def unlockAccount(request: UnlockAccountRequest): ServiceResponse[UnlockAccountResponse] = Future {
    keyStore.unlockAccount(request.address, request.passphrase)
      .left.map(handleError)
      .map { wallet =>
        unlockedWallets += request.address -> wallet
        UnlockAccountResponse(true)
      }
  }

  def lockAccount(request: LockAccountRequest): ServiceResponse[LockAccountResponse] = Future.successful {
    unlockedWallets -= request.address
    Right(LockAccountResponse(true))
  }

  def ecRecover(req: EcRecoverRequest): ServiceResponse[EcRecoverResponse] = Future {
    import req._
    val prefixed: Array[Byte] =
      0x19.toByte +:
        s"Ethereum Signed Message:\n${message.length}".getBytes ++:
        message.toArray[Byte]

    val msg = crypto.kec256(prefixed)
    signature.publicKey(msg).map { publicKey =>
      Right(EcRecoverResponse(Address(crypto.kec256(publicKey))))
    }.getOrElse(Left(InvalidParams("unable to recover address")))
  }

  def sendTransaction(request: SendTransactionWithPassphraseRequest): ServiceResponse[SendTransactionWithPassphraseResponse] = Future {
    keyStore.unlockAccount(request.tx.from, request.passphrase).left.map(handleError).map { wallet =>
      val hash = sendTransaction(request.tx, wallet)
      SendTransactionWithPassphraseResponse(hash)
    }
  }


  def sendTransaction(request: SendTransactionRequest): ServiceResponse[SendTransactionResponse] = Future {
    unlockedWallets.get(request.tx.from) match {
      case Some(wallet) =>
        Right(SendTransactionResponse(sendTransaction(request.tx, wallet)))

      case None =>
        Left(AccountLocked)
    }
  }

  private def sendTransaction(request: TransactionRequest, wallet: Wallet): ByteString = {
    val defaultNonce = getCurrentAccount(request.from).getOrElse(Account.Empty).nonce
    val tx = request.toTransaction(defaultNonce)
    val stx = wallet.signTx(tx)

    txPool ! AddTransactions(stx)

    stx.hash
  }

  private def getCurrentAccount(address: Address): Option[Account] =
    blockchain.getAccount(address, appStateStorage.getBestBlockNumber())


  private val handleError: PartialFunction[KeyStore.KeyStoreError, JsonRpcError] = {
    case KeyStore.WrongPassphrase => InvalidPassphrase
    case KeyStore.KeyNotFound => KeyNotFound
    case KeyStore.IOError(msg) => LogicError(msg)
  }
}
