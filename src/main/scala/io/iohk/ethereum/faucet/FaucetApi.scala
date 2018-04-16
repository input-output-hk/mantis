package io.iohk.ethereum.faucet

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.util.ByteString
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
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
    config: FaucetConfig)
  extends Json4sSupport
    with Logger {

  private val wallet = keyStore.unlockAccount(config.walletAddress, config.walletPassword) match {
    case Right(w) => w
    case Left(err) => throw new RuntimeException(s"Cannot unlock wallet for use in faucet (${config.walletAddress}), because of $err")
  }

  private val corsSettings = CorsSettings.defaultSettings.copy(
    allowGenericHttpRequests = true,
    allowedOrigins = config.corsAllowedOrigins)

  val route: Route = cors(corsSettings) {
    (path("faucet") & pathEndOrSingleSlash & post & parameter('address)) { targetAddress =>
      handleRequest(targetAddress)
    }
  }

  private def handleRequest(targetAddress: String) = {
    val res = for {
      nonce <- rpcClient.getNonce(wallet.address)
      txId <- rpcClient.sendTransaction(prepareTx(Address(targetAddress), nonce))
    } yield txId

    res match {
      case Right(txId) =>
        complete(StatusCodes.OK, s"0x${Hex.toHexString(txId.toArray[Byte])}")

      case Left(err) =>
        log.error(s"An error occurred while using faucet: $err")
        complete(StatusCodes.InternalServerError)
    }
  }

  private def prepareTx(targetAddress: Address, nonce: BigInt): ByteString = {
    val transaction = Transaction(
      nonce + 1,
      config.txGasPrice,
      config.txGasLimit,
      Some(targetAddress),
      config.txValue,
      ByteString())

    val stx = wallet.signTx(transaction, None)
    ByteString(rlp.encode(stx.toRLPEncodable))
  }

}
