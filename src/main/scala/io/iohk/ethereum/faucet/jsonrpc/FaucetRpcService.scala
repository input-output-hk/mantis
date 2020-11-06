package io.iohk.ethereum.faucet.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, Transaction}
import io.iohk.ethereum.faucet.FaucetConfig
import io.iohk.ethereum.faucet.FaucetStatus.FaucetUnavailable
import io.iohk.ethereum.faucet.jsonrpc.FaucetDomain.{SendFundsRequest, SendFundsResponse, StatusRequest, StatusResponse}
import io.iohk.ethereum.jsonrpc.{JsonRpcError, ServiceResponse}
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.mallet.service.RpcClient
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import io.iohk.ethereum.rlp
import io.iohk.ethereum.utils.{ByteStringUtils, Logger}
import monix.eval.Task

class FaucetRpcService(rpcClient: RpcClient, keyStore: KeyStore, config: FaucetConfig) extends Logger {

  private val wallet = keyStore.unlockAccount(config.walletAddress, config.walletPassword) match {
    case Right(w) =>
      log.info(s"wallet unlocked for use in faucet (${config.walletAddress})")
      w
    case Left(err) =>
      throw new RuntimeException(s"Cannot unlock wallet for use in faucet (${config.walletAddress}), because of $err")
  }

  def sendFunds(sendFundsRequest: SendFundsRequest): ServiceResponse[SendFundsResponse] = {
    val res = for {
      nonce <- rpcClient.getNonce(wallet.address)
      txId <- rpcClient.sendTransaction(prepareTx(sendFundsRequest.address, nonce))
    } yield txId

    res match {
      case Right(txId) =>
        val txIdHex = s"0x${ByteStringUtils.hash2string(txId)}"
        log.info(s"Sending ${config.txValue} ETC to ${sendFundsRequest.address} in tx: $txIdHex.")
        Task.now(Right(SendFundsResponse(txId)))

      case Left(err) =>
        log.error(s"An error occurred while using faucet: $err")
        Task.now(Left(JsonRpcError.InternalError))
    }
  }

  private def prepareTx(targetAddress: Address, nonce: BigInt): ByteString = {
    val transaction =
      Transaction(nonce, config.txGasPrice, config.txGasLimit, Some(targetAddress), config.txValue, ByteString())

    val stx = wallet.signTx(transaction, None)
    ByteString(rlp.encode(stx.tx.toRLPEncodable))
  }

  def status(statusRequest: StatusRequest): ServiceResponse[StatusResponse] = {
    //TODO: build status in task ETCM-252
    Task.now(Right(StatusResponse(FaucetUnavailable)))
  }
}
