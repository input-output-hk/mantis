package io.iohk.ethereum.jsonrpc

import java.time.Duration

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import cats.data.EitherT
import cats.implicits._
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.JsonRpcErrors._
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.keystore.{KeyStore, Wallet}
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.{AddOrOverrideTransaction, PendingTransactionsResponse}
import io.iohk.ethereum.utils.{BlockchainConfig, TxPoolConfig}
import io.iohk.ethereum.validators.{SignedTransactionError, Validators}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object PersonalService {

  case class ImportRawKeyRequest(prvKey: ByteString, passphrase: String)
  case class ImportRawKeyResponse(address: Address)

  case class NewAccountRequest(passphrase: String)
  case class NewAccountResponse(address: Address)

  case class ListAccountsRequest()
  case class ListAccountsResponse(addresses: List[Address])

  case class UnlockAccountRequest(address: Address, passphrase: String, duration: Option[Duration])
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

  case class DeleteWalletRequest(address: Address)
  case class DeleteWalletResponse(result: Boolean)

  case class ChangePassphraseRequest(address: Address, oldPassphrase: String, newPassphrase: String)
  case class ChangePassphraseResponse()

  val InvalidKey = InvalidParams("Invalid key provided, expected 32 bytes (64 hex digits)")
  val InvalidAddress = InvalidParams("Invalid address, expected 20 bytes (40 hex digits)")
  val InvalidPassphrase = LogicError("Could not decrypt key with given passphrase")
  val KeyNotFound = LogicError("No key found for the given address")

  val PrivateKeyLength = 32
  val defaultUnlockTime = 300
}

class PersonalService(
  keyStore: KeyStore,
  blockchain: Blockchain,
  txPool: ActorRef,
  appStateStorage: AppStateStorage,
  blockchainConfig: BlockchainConfig,
  txPoolConfig: TxPoolConfig,
  validators: Validators) {

  private val unlockedWallets: ExpiringMap[Address, Wallet] = ExpiringMap.empty(Duration.ofSeconds(defaultUnlockTime))

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
        request.duration.fold(unlockedWallets.add(request.address, wallet))(duration =>
          if (duration.isZero)
            unlockedWallets.addForever(request.address, wallet)
          else
            unlockedWallets.add(request.address, wallet, duration)
        )

        UnlockAccountResponse(true)
      }
  }

  def lockAccount(request: LockAccountRequest): ServiceResponse[LockAccountResponse] = Future.successful {
    unlockedWallets.remove(request.address)
    Right(LockAccountResponse(true))
  }

  def sign(request: SignRequest): ServiceResponse[SignResponse] = Future {
    import request._

    val accountWallet = {
      if (passphrase.isDefined) keyStore.unlockAccount(address, passphrase.get).left.map(handleError)
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
    val result = for {
      wallet <- EitherT(unlockOrGet(request.tx.from, Some(request.passphrase)))
      hash   <- EitherT(sendTransaction(request.tx, wallet))
    } yield SendTransactionWithPassphraseResponse(hash)

    result.value
  }

  def sendTransaction(request: SendTransactionRequest): ServiceResponse[SendTransactionResponse] = {
    val result = for {
      wallet <- EitherT(unlockOrGet(request.tx.from, None))
      hash   <- EitherT(sendTransaction(request.tx, wallet))
    } yield SendTransactionResponse(hash)

    result.value
  }

  def unlockOrGet(address: Address, passphrase: Option[String]): ServiceResponse[Wallet] = {
    Future {
      passphrase.fold(unlockedWallets.get(address).toRight(AccountLocked))(passphrase =>
        keyStore.unlockAccount(address, passphrase).left.map(handleError)
      )
    }
  }

  def deleteWallet(request: DeleteWalletRequest): ServiceResponse[DeleteWalletResponse] = Future {
    unlockedWallets.remove(request.address)

    keyStore.deleteWallet(request.address)
      .map(DeleteWalletResponse.apply)
      .left.map(handleError)
  }

  def changePassphrase(request: ChangePassphraseRequest): ServiceResponse[ChangePassphraseResponse] = Future {
    import request._
    keyStore.changePassphrase(address, oldPassphrase, newPassphrase)
      .map(_ => ChangePassphraseResponse())
      .left.map(handleError)
  }

  private def sendTransaction(request: TransactionRequest, wallet: Wallet): ServiceResponse[ByteString] = {
    getSignedTransaction(request, wallet).map {stx =>
      for {
        _<- validators.signedTransactionValidator.validatePreRpc(stx).left.map(handleValidationError)
      } yield {
        txPool ! AddOrOverrideTransaction(stx)
        stx.hash
      }
    }
  }

  private def getSignedTransaction(request: TransactionRequest, wallet: Wallet): Future[SignedTransaction] = {
    val sendingAccount = getCurrentAccount(request.from).getOrElse(Account.empty(blockchainConfig.accountStartNonce))

    getNextTransactionNonce(wallet, sendingAccount).map { nonce =>
      signTx(request.toTransaction(nonce), wallet)
    }
  }

  private def getNextTransactionNonce(wallet: Wallet, account: Account): Future[BigInt] = {
    getWalletPendingTransactions(wallet).map(transactions =>
      if (transactions.isEmpty)
        account.nonce.toBigInt
      else
        transactions.map(_.nonce).max + 1
    )
  }

  private def getWalletPendingTransactions(wallet: Wallet) = {
    implicit val timeout = Timeout(txPoolConfig.pendingTxManagerQueryTimeout)

    (txPool ? PendingTransactionsManager.GetPendingTransactions).mapTo[PendingTransactionsResponse].map( response =>
      response.pendingTransactions.collect {
        case transaction if transaction.stx.senderAddress == wallet.address => transaction.stx.tx
      }
    )
  }

  private def signTx(tx: Transaction, wallet: Wallet): SignedTransaction = {
    if (appStateStorage.getBestBlockNumber() >= blockchainConfig.eip155BlockNumber) {
      wallet.signTx(tx, Some(blockchainConfig.chainId))
    } else {
      wallet.signTx(tx, None)
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
    case KeyStore.DuplicateKeySaved => LogicError("account already exists")
  }

  private val handleValidationError: PartialFunction[SignedTransactionError, JsonRpcError] = {
    case x:SignedTransactionError => LogicError("err")
  }

}
