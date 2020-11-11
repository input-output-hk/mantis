package io.iohk.ethereum.faucet.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, Transaction}
import io.iohk.ethereum.faucet.FaucetConfig
import io.iohk.ethereum.keystore.KeyStore.KeyStoreError
import io.iohk.ethereum.keystore.{KeyStore, Wallet}
import io.iohk.ethereum.mallet.common.Err
import io.iohk.ethereum.mallet.service.RpcClient
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import io.iohk.ethereum.rlp
import io.iohk.ethereum.utils.{ByteStringUtils, Logger}
import monix.eval.Task

class WalletService(rpcClient: RpcClient, keyStore: KeyStore, config: FaucetConfig) extends Logger {

  def sendFunds(wallet: Wallet, addressTo: Address): Task[Either[Err, ByteString]] = {
    val res = for {
      nonce <- rpcClient.getNonce(wallet.address)
      txId <- rpcClient.sendTransaction(prepareTx(wallet, addressTo, nonce))
    } yield txId

    Task {
      res match {
        case Right(txId) =>
          val txIdHex = s"0x${ByteStringUtils.hash2string(txId)}"
          log.info(s"Sending ${config.txValue} ETH to $addressTo in tx: $txIdHex.")
          Right(txId)

        case Left(error) =>
          log.error(s"An error occurred while using faucet", error)
          Left(error)
      }
    }
  }

  private def prepareTx(wallet: Wallet, targetAddress: Address, nonce: BigInt): ByteString = {
    val transaction =
      Transaction(nonce, config.txGasPrice, config.txGasLimit, Some(targetAddress), config.txValue, ByteString())

    val stx = wallet.signTx(transaction, None)
    ByteString(rlp.encode(stx.tx.toRLPEncodable))
  }

  def getWallet: Task[Either[KeyStoreError, Wallet]] = Task.now {
    keyStore.unlockAccount(config.walletAddress, config.walletPassword) match {
      case Right(w) =>
        log.info(s"unlock wallet for use in faucet (${config.walletAddress})")
        Right(w)
      case Left(err) =>
        log.error(s"Cannot unlock wallet for use in faucet (${config.walletAddress}), because of $err")
        Left(err)
    }
  }

}
