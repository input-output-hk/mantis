package io.iohk.ethereum.faucet

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.HttpOriginRange
import akka.http.scaladsl.server.Directives.{as, entity, pathEndOrSingleSlash}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import io.iohk.ethereum.domain.{Address, Transaction}
import io.iohk.ethereum.jsonrpc.{JsonRpcRequest, TransactionRequest}
import io.iohk.ethereum.jsonrpc.PersonalService.SendTransactionRequest
import io.iohk.ethereum.keystore.{KeyStore, Wallet}
import io.iohk.ethereum.utils.Logger
import org.spongycastle.util.encoders.Hex

class FaucetApi(keyStore: KeyStore) extends Json4sSupport with Logger {

  val walletAddress = Address(0x00)

  val wallet = keyStore.unlockAccount(walletAddress, "") match {
    case Right(w) => w
    case Left(err) => throw new RuntimeException(s"Cannot unlock wallet for use in faucet ($walletAddress), because of $err")
  }

  val corsSettings = CorsSettings.defaultSettings.copy(
    allowGenericHttpRequests = true,
    allowedOrigins = HttpOriginRange("*"))

  val route: Route = cors(corsSettings) {
    (path("faucet") & post & parameter('address)) { address =>
      handleRequest(address)
    }
  }

  private def handleRequest(address: String) = {
    // make a transactions object
    val transactionRequest = SendTransactionRequest(TransactionRequest(
      from = walletAddress,
      to = Some(Address(Hex.decode(address))),
      value = Some(100000)))

    // send this request to node via rpc
    complete(StatusCodes.OK)
  }

}
