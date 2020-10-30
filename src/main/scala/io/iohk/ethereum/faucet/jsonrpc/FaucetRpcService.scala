package io.iohk.ethereum.faucet.jsonrpc

import java.time.Clock

import akka.http.scaladsl.model.RemoteAddress
import akka.util.ByteString
import com.twitter.util.LruMap
import io.iohk.ethereum.domain.{Address, Transaction}
import io.iohk.ethereum.faucet.FaucetConfig
import io.iohk.ethereum.faucet.jsonrpc.FaucetDomain.{SendFundsRequest, SendFundsResponse, StatusRequest, StatusResponse}
import io.iohk.ethereum.jsonrpc.ServiceResponse
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.mallet.service.RpcClient
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import io.iohk.ethereum.rlp
import io.iohk.ethereum.utils.{ByteStringUtils, Logger}

import scala.concurrent.Future

class FaucetRpcService(rpcClient: RpcClient, keyStore: KeyStore, config: FaucetConfig, clock: Clock = Clock.systemUTC())
  extends Logger {

  private val latestRequestTimestamps = new LruMap[RemoteAddress, Long](config.latestTimestampCacheSize)

  private val wallet = keyStore.unlockAccount(config.walletAddress, config.walletPassword) match {
    case Right(w) => w
    case Left(err) =>
      throw new RuntimeException(s"Cannot unlock wallet for use in faucet (${config.walletAddress}), because of $err")
  }

  def sendFunds(sendFundsRequest: SendFundsRequest): ServiceResponse[SendFundsResponse] = {
    val clientAddr: RemoteAddress = RemoteAddress.Unknown //TODO: ??
    val targetAddress: Address = sendFundsRequest.address
    val timeMillis = clock.instant().toEpochMilli
    val latestRequestTimestamp = latestRequestTimestamps.getOrElse(clientAddr, 0L)
    if (latestRequestTimestamp + config.minRequestInterval.toMillis  > 0) {
      latestRequestTimestamps.put(clientAddr, timeMillis)

      val res = for {
        nonce <- rpcClient.getNonce(wallet.address)
        txId <- rpcClient.sendTransaction(prepareTx(targetAddress, nonce))
      } yield txId

      res match {
        case Right(txId) =>
          val txIdHex = s"0x${ByteStringUtils.hash2string(txId)}"
          log.info(s"Sending ${config.txValue} ETH to $targetAddress in tx: $txIdHex. Requested by $clientAddr")
          //complete(StatusCodes.OK, txIdHex)
          Future.successful(Right(SendFundsResponse(txId))) //TODO: change...

        case Left(err) =>
          log.error(s"An error occurred while using faucet: $err")
          Future.successful(???)
          //complete(StatusCodes.InternalServerError)
      }
    } else Future.successful(???)//complete(StatusCodes.TooManyRequests)
  }

  private def prepareTx(targetAddress: Address, nonce: BigInt): ByteString = {
    val transaction =
      Transaction(nonce, config.txGasPrice, config.txGasLimit, Some(targetAddress), config.txValue, ByteString())

    val stx = wallet.signTx(transaction, None)
    ByteString(rlp.encode(stx.tx.toRLPEncodable))
  }

  def status(statusRequest: StatusRequest): ServiceResponse[StatusResponse]  = {
    Future.successful(Right(StatusResponse("ok")))
  }
}
