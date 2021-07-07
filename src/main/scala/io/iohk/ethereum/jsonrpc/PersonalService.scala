package io.iohk.ethereum.jsonrpc

import java.time.Duration

import akka.actor.ActorRef
import akka.util.ByteString
import akka.util.Timeout

import monix.eval.Task

import scala.util.Try

import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.jsonrpc.AkkaTaskOps._
import io.iohk.ethereum.jsonrpc.JsonRpcError._
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.keystore.Wallet
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.AddOrOverrideTransaction
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.ByteStringUtils.ByteStringOps
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.utils.TxPoolConfig

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

  case class SendIeleTransactionRequest(tx: IeleTransactionRequest)

  case class SignRequest(message: ByteString, address: Address, passphrase: Option[String])
  case class SignResponse(signature: ECDSASignature)

  case class EcRecoverRequest(message: ByteString, signature: ECDSASignature)
  case class EcRecoverResponse(address: Address)

  val InvalidKey: JsonRpcError = InvalidParams("Invalid key provided, expected 32 bytes (64 hex digits)")
  val InvalidAddress: JsonRpcError = InvalidParams("Invalid address, expected 20 bytes (40 hex digits)")
  val InvalidPassphrase: JsonRpcError = LogicError("Could not decrypt key with given passphrase")
  val KeyNotFound: JsonRpcError = LogicError("No key found for the given address")
  val PassPhraseTooShort: Int => JsonRpcError = minLength =>
    LogicError(s"Provided passphrase must have at least $minLength characters")

  val PrivateKeyLength = 32
  val defaultUnlockTime = 300
}

class PersonalService(
    keyStore: KeyStore,
    blockchain: Blockchain,
    blockchainReader: BlockchainReader,
    txPool: ActorRef,
    blockchainConfig: BlockchainConfig,
    txPoolConfig: TxPoolConfig
) extends Logger {

  private val unlockedWallets: ExpiringMap[Address, Wallet] = ExpiringMap.empty(Duration.ofSeconds(defaultUnlockTime))

  def importRawKey(req: ImportRawKeyRequest): ServiceResponse[ImportRawKeyResponse] = Task {
    for {
      prvKey <- Right(req.prvKey).filterOrElse(_.length == PrivateKeyLength, InvalidKey)
      addr <- keyStore.importPrivateKey(prvKey, req.passphrase).left.map(handleError)
    } yield ImportRawKeyResponse(addr)
  }

  def newAccount(req: NewAccountRequest): ServiceResponse[NewAccountResponse] = Task {
    keyStore
      .newAccount(req.passphrase)
      .map(NewAccountResponse.apply)
      .left
      .map(handleError)
  }

  def listAccounts(request: ListAccountsRequest): ServiceResponse[ListAccountsResponse] = Task {
    keyStore
      .listAccounts()
      .map(ListAccountsResponse.apply)
      .left
      .map(handleError)
  }

  def unlockAccount(request: UnlockAccountRequest): ServiceResponse[UnlockAccountResponse] = Task {
    keyStore
      .unlockAccount(request.address, request.passphrase)
      .left
      .map(handleError)
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

  def lockAccount(request: LockAccountRequest): ServiceResponse[LockAccountResponse] = Task {
    unlockedWallets.remove(request.address)
    Right(LockAccountResponse(true))
  }

  def sign(request: SignRequest): ServiceResponse[SignResponse] = Task {
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

  def ecRecover(req: EcRecoverRequest): ServiceResponse[EcRecoverResponse] = Task {
    import req._
    signature
      .publicKey(getMessageToSign(message))
      .map { publicKey =>
        Right(EcRecoverResponse(Address(crypto.kec256(publicKey))))
      }
      .getOrElse(Left(InvalidParams("unable to recover address")))
  }

  def sendTransaction(
      request: SendTransactionWithPassphraseRequest
  ): ServiceResponse[SendTransactionWithPassphraseResponse] = {
    val maybeWalletUnlocked = Task {
      keyStore.unlockAccount(request.tx.from, request.passphrase).left.map(handleError)
    }

    maybeWalletUnlocked.flatMap {
      case Right(wallet) =>
        val futureTxHash = sendTransaction(request.tx, wallet)
        futureTxHash.map(txHash => Right(SendTransactionWithPassphraseResponse(txHash)))
      case Left(err) => Task.now(Left(err))
    }
  }

  def sendTransaction(request: SendTransactionRequest): ServiceResponse[SendTransactionResponse] =
    Task(unlockedWallets.get(request.tx.from)).flatMap {
      case Some(wallet) =>
        val futureTxHash = sendTransaction(request.tx, wallet)
        futureTxHash.map(txHash => Right(SendTransactionResponse(txHash)))

      case None => Task.now(Left(AccountLocked))
    }

  def sendIeleTransaction(request: SendIeleTransactionRequest): ServiceResponse[SendTransactionResponse] = {
    import request.tx

    val args = tx.arguments.getOrElse(Nil)
    val dataEither = (tx.function, tx.contractCode) match {
      case (Some(function), None)     => Right(rlp.encode(RLPList(function, args)))
      case (None, Some(contractCode)) => Right(rlp.encode(RLPList(contractCode, args)))
      case _                          => Left(JsonRpcError.InvalidParams("Iele transaction should contain either functionName or contractCode"))
    }

    dataEither match {
      case Right(data) =>
        sendTransaction(
          SendTransactionRequest(
            TransactionRequest(tx.from, tx.to, tx.value, tx.gasLimit, tx.gasPrice, tx.nonce, Some(ByteString(data)))
          )
        )
      case Left(error) =>
        Task.now(Left(error))
    }
  }

  private def sendTransaction(request: TransactionRequest, wallet: Wallet): Task[ByteString] = {
    implicit val timeout = Timeout(txPoolConfig.pendingTxManagerQueryTimeout)

    val pendingTxsFuture =
      txPool.askFor[PendingTransactionsResponse](PendingTransactionsManager.GetPendingTransactions)
    val latestPendingTxNonceFuture: Task[Option[BigInt]] = pendingTxsFuture.map { pendingTxs =>
      val senderTxsNonces = pendingTxs.pendingTransactions
        .collect { case ptx if ptx.stx.senderAddress == wallet.address => ptx.stx.tx.tx.nonce }
      Try(senderTxsNonces.max).toOption
    }
    latestPendingTxNonceFuture.map { maybeLatestPendingTxNonce =>
      val maybeCurrentNonce = getCurrentAccount(request.from).map(_.nonce.toBigInt)
      val maybeNextTxNonce = maybeLatestPendingTxNonce.map(_ + 1).orElse(maybeCurrentNonce)
      val tx = request.toTransaction(maybeNextTxNonce.getOrElse(blockchainConfig.accountStartNonce))

      val stx = if (blockchainReader.getBestBlockNumber() >= blockchainConfig.forkBlockNumbers.eip155BlockNumber) {
        wallet.signTx(tx, Some(blockchainConfig.chainId))
      } else {
        wallet.signTx(tx, None)
      }
      log.debug("Trying to add personal transaction: {}", stx.tx.hash.toHex)

      txPool ! AddOrOverrideTransaction(stx.tx)

      stx.tx.hash
    }
  }

  private def getCurrentAccount(address: Address): Option[Account] =
    blockchain.getAccount(address, blockchainReader.getBestBlockNumber())

  private def getMessageToSign(message: ByteString) = {
    val prefixed: Array[Byte] =
      0x19.toByte +:
        s"Ethereum Signed Message:\n${message.length}".getBytes ++:
        message.toArray[Byte]

    crypto.kec256(prefixed)
  }

  private val handleError: PartialFunction[KeyStore.KeyStoreError, JsonRpcError] = {
    case KeyStore.DecryptionFailed              => InvalidPassphrase
    case KeyStore.KeyNotFound                   => KeyNotFound
    case KeyStore.PassPhraseTooShort(minLength) => PassPhraseTooShort(minLength)
    case KeyStore.IOError(msg)                  => LogicError(msg)
    case KeyStore.DuplicateKeySaved             => LogicError("account already exists")
  }
}
