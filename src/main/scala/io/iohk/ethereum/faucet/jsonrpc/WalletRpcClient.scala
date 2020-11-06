package io.iohk.ethereum.faucet.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, Transaction}
import io.iohk.ethereum.faucet.{FaucetConfig, FaucetStatus, FaucetWalletStatus}
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.mallet.common.Err
import io.iohk.ethereum.mallet.service.RpcClient
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import io.iohk.ethereum.rlp
import io.iohk.ethereum.utils.{ByteStringUtils, Logger}
import monix.eval.Task

class WalletRpcClient(rpcClient: RpcClient, keyStore: KeyStore, config: FaucetConfig) extends Logger {

  private val wallet = keyStore.unlockAccount(config.walletAddress, config.walletPassword) match {
    case Right(w) => w
    case Left(err) =>
      throw new RuntimeException(s"Cannot unlock wallet for use in faucet (${config.walletAddress}), because of $err")
  }

  def sendFunds(addressTo: Address): Task[Either[Err, ByteString]] = {
    val res = for {
      nonce <- rpcClient.getNonce(wallet.address)
      txId <- rpcClient.sendTransaction(prepareTx(addressTo, nonce))
    } yield txId

    Task { res match {
      case Right(txId) =>
        val txIdHex = s"0x${ByteStringUtils.hash2string(txId)}"
        log.info(s"Sending ${config.txValue} ETH to $addressTo in tx: $txIdHex.")
        Right(txId)

      case Left(error) =>
        log.error(s"An error occurred while using faucet", error)
        Left(error)
    }}
  }

  private def prepareTx(targetAddress: Address, nonce: BigInt): ByteString = {
    val transaction =
      Transaction(nonce, config.txGasPrice, config.txGasLimit, Some(targetAddress), config.txValue, ByteString())

    val stx = wallet.signTx(transaction, None)
    ByteString(rlp.encode(stx.tx.toRLPEncodable))
  }

  //TODO: status from address?
  def getWalletStatus(): Task[FaucetWalletStatus] = {
    Task {rpcClient.listAccounts()}
      .map {
        case Right(Nil) =>
          log.info("Status: Wallet does not exist")
          FaucetStatus.WalletDoesNotExist
        case Right(address) =>  //TODO... comare address if contained?
          log.info("Status: Wallet is locked")
          FaucetStatus.WalletIsUnlocked
        case Left(error) =>
          log.error(s"Wallet not responds", error)
          FaucetStatus.WalletNotResponds
      }
  }

  //TODO: It is right? allways true?
  def unlockWallet(): Task[Either[Err, Boolean]] = Task {
    rpcClient.unlockAccount(config.walletAddress, config.walletPassword) match {
      case Right(w) =>
        log.info(s"wallet unlocked for use in faucet (${config.walletAddress})")
        Right(w)
      case Left(err) =>
        log.error(s"Cannot unlock wallet for use in faucet (${config.walletAddress}), because of $err")
        Left(err)
    }
  }

  //TODO: status from address?
  def restoreWallet(): Task[Either[Err, Address]] =
    Task{rpcClient.importRawKey(config.walletAddress.bytes, config.walletPassword)}
      .map {
        case Right(address) =>
          log.info("Successfully restored wallet to use in faucet")
          Right(address)
        /*case Right(false) =>
          log.error("Cannot restore wallet for use in faucet")
          false*/
        case Left(error) =>
          log.error(s"Cannot restore wallet for use in faucet, because of $error")
          Left(error)
      }
}