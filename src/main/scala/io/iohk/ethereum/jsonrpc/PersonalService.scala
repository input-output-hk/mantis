package io.iohk.ethereum.jsonrpc

import akka.pattern.ask
import akka.actor.ActorRef
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{Account, Address, Blockchain}
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.keystore.{KeyStore, Wallet}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors._
import io.iohk.ethereum.transactions.PendingTransactionsManager.AddTransactions
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.{AddOrOverrideTransaction, PendingTransactionsResponse}
import io.iohk.ethereum.utils.TxPoolConfig

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

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

  case class SignRequest(message: ByteString, address: Address, passphrase: Option[String])
  case class SignResponse(signature: ECDSASignature)

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
  appStateStorage: AppStateStorage,
  blockchainConfig: BlockchainConfig,
  txPoolConfig: TxPoolConfig) {

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

  def sign(request: SignRequest): ServiceResponse[SignResponse] = Future {
    import request._

    val accountWallet = {
      if(passphrase.isDefined) keyStore.unlockAccount(address, passphrase.get).left.map(handleError)
      else unlockedWallets.get(request.address).toRight(AccountLocked)
    }

    accountWallet
      .map { wallet =>
        SignResponse(ECDSASignature.sign(getMessageToSign(message), wallet.keyPair))
      }
  }

  def ecRecover(req: EcRecoverRequest): ServiceResponse[EcRecoverResponse] = Future {
    import req._
    signature.publicKey(getMessageToSign(message)).map { publicKey =>
      Right(EcRecoverResponse(Address(crypto.kec256(publicKey))))
    }.getOrElse(Left(InvalidParams("unable to recover address")))
  }

  def sendTransaction(request: SendTransactionWithPassphraseRequest): ServiceResponse[SendTransactionWithPassphraseResponse] = {
    val maybeWalletUnlocked = Future { keyStore.unlockAccount(request.tx.from, request.passphrase).left.map(handleError) }
    maybeWalletUnlocked.flatMap {
      case Right(wallet) =>
        val futureTxHash = sendTransaction(request.tx, wallet)
        futureTxHash.map(txHash => Right(SendTransactionWithPassphraseResponse(txHash)))
      case Left(err) => Future.successful(Left(err))
    }
  }


  def sendTransaction(request: SendTransactionRequest): ServiceResponse[SendTransactionResponse] = {
    unlockedWallets.get(request.tx.from) match {
      case Some(wallet) =>
        val futureTxHash = sendTransaction(request.tx, wallet)
        futureTxHash.map(txHash => Right(SendTransactionResponse(txHash)))

      case None =>
        Future.successful(Left(AccountLocked))
    }
  }

  private def sendTransaction(request: TransactionRequest, wallet: Wallet): Future[ByteString] = {
    implicit val timeout = Timeout(txPoolConfig.pendingTxManagerQueryTimeout)

    val pendingTxsFuture = (txPool ? PendingTransactionsManager.GetPendingTransactions).mapTo[PendingTransactionsResponse]
    val latestPendingTxNonceFuture: Future[Option[BigInt]] = pendingTxsFuture.map { pendingTxs =>
      val senderTxsNonces = pendingTxs.pendingTransactions
        .collect { case ptx if ptx.stx.senderAddress == wallet.address => ptx.stx.tx.nonce }
      Try(senderTxsNonces.max).toOption
    }
    latestPendingTxNonceFuture.map{ maybeLatestPendingTxNonce =>
      val maybeCurrentNonce = getCurrentAccount(request.from).map(_.nonce.toBigInt)
      val maybeNextTxNonce = maybeLatestPendingTxNonce.map(_ + 1) orElse maybeCurrentNonce
      val tx = request.toTransaction(maybeNextTxNonce.getOrElse(blockchainConfig.accountStartNonce))
      val stx = wallet.signTx(tx)

      txPool ! AddOrOverrideTransaction(stx)

      stx.hash
    }
  }

  private def getCurrentAccount(address: Address): Option[Account] =
    blockchain.getAccount(address, appStateStorage.getBestBlockNumber())

  private def getMessageToSign(message: ByteString) = {
    val prefixed: Array[Byte] =
      0x19.toByte +:
        s"Ethereum Signed Message:\n${message.length}".getBytes ++:
        message.toArray[Byte]

    crypto.kec256(prefixed)
  }

  private val handleError: PartialFunction[KeyStore.KeyStoreError, JsonRpcError] = {
    case KeyStore.DecryptionFailed => InvalidPassphrase
    case KeyStore.KeyNotFound => KeyNotFound
    case KeyStore.IOError(msg) => LogicError(msg)
  }
}
