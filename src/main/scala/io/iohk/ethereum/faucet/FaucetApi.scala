package io.iohk.ethereum.faucet

import java.time.Clock

import akka.http.scaladsl.model.{RemoteAddress, StatusCodes}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.util.ByteString
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import com.twitter.util.LruMap
import io.iohk.ethereum.domain.{Address, Transaction}
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.mallet.service.RpcClient
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.rlp
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import org.spongycastle.util.encoders.Hex

class FaucetApi(
    rpcClient: RpcClient,
    keyStore: KeyStore,
    config: FaucetConfig,
    clock: Clock = Clock.systemUTC())
  extends Logger {

  private val latestRequestTimestamps = new LruMap[RemoteAddress, Long](config.latestTimestampCacheSize)

  private val wallet = keyStore.unlockAccountFromKeyfile(config.keyfile, config.walletPassword) match {
    case Right(w) =>
      log.info(s"Successfully unlocked wallet: ${w.address} to use in faucet")
      w
    case Left(err) => throw new RuntimeException(s"Cannot unlock wallet for use in faucet (${config.keyfile}), because of $err")
  }

  private val corsSettings = CorsSettings.defaultSettings.copy(
    allowGenericHttpRequests = true,
    allowedOrigins = config.corsAllowedOrigins)

  val route: Route = cors(corsSettings) {
    (path("faucet" / "healthcheck") & pathEndOrSingleSlash & get) {
      complete(StatusCodes.OK)
    } ~
    (path("faucet") & pathEndOrSingleSlash & post & parameter('address)) { targetAddress =>
      extractClientIP { clientAddr =>
        handleRequest(clientAddr, targetAddress)
      }
    }
  }

  private def handleRequest(clientAddr: RemoteAddress, targetAddress: String) = {
    val timeMillis = clock.instant().toEpochMilli
    val latestRequestTimestamp = latestRequestTimestamps.getOrElse(clientAddr, 0L)
    if (latestRequestTimestamp + config.minRequestInterval.toMillis < timeMillis) {
      latestRequestTimestamps.put(clientAddr, timeMillis)

      val res = for {
        nonce <- rpcClient.getNonce(wallet.address)
        txId <- rpcClient.sendTransaction(prepareTx(Address(targetAddress), nonce))
      } yield txId

      res match {
        case Right(txId) =>
          log.info(s"Sending ${config.txValue} ETH to $targetAddress in tx: $txId. Requested by $clientAddr")
          complete(StatusCodes.OK, s"0x${Hex.toHexString(txId.toArray[Byte])}")

        case Left(err) =>
          log.error(s"An error occurred while using faucet: $err")
          complete(StatusCodes.InternalServerError)
      }
    } else complete(StatusCodes.TooManyRequests)
  }

  private def prepareTx(targetAddress: Address, nonce: BigInt): ByteString = {
    val transaction = Transaction(
      nonce,
      config.txGasPrice,
      config.txGasLimit,
      Some(targetAddress),
      config.txValue,
      ByteString())

    val stx = wallet.signTx(transaction, None)
    ByteString(rlp.encode(stx.toRLPEncodable))
  }

}
