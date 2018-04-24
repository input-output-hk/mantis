package io.iohk.ethereum.faucet

import akka.http.scaladsl.model.headers.HttpOriginRange
import com.typesafe.config.{Config => TypesafeConfig}
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.Config

import scala.collection.JavaConverters._
import scala.concurrent.duration.{FiniteDuration, _}
import scala.util.Try

case class FaucetConfig(
    walletAddress: Address,
    walletPassword: String,
    txGasPrice: BigInt,
    txGasLimit: BigInt,
    txValue: BigInt,
    corsAllowedOrigins: HttpOriginRange,
    rpcAddress: String,
    keyStoreDir: String,
    listenInterface: String,
    listenPort: Int,
    minRequestInterval: FiniteDuration,
    latestTimestampCacheSize: Int)

object FaucetConfig {
  def apply(mantisConfig: TypesafeConfig): FaucetConfig = {
    val faucetConfig = mantisConfig.getConfig("faucet")

    val corsAllowedOrigins: HttpOriginRange =
      (Try(Config.Network.Rpc.parseMultipleOrigins(faucetConfig.getStringList("cors-allowed-origins").asScala)) recoverWith {
        case _ => Try(Config.Network.Rpc.parseSingleOrigin(faucetConfig.getString("cors-allowed-origins")))
      }).get

    FaucetConfig(
      walletAddress = Address(faucetConfig.getString("wallet-address")),
      walletPassword = faucetConfig.getString("wallet-password"),
      txGasPrice = faucetConfig.getLong("tx-gas-price"),
      txGasLimit = faucetConfig.getLong("tx-gas-limit"),
      txValue = faucetConfig.getLong("tx-value"),
      corsAllowedOrigins = corsAllowedOrigins,
      rpcAddress = faucetConfig.getString("rpc-address"),
      keyStoreDir = faucetConfig.getString("keystore-dir"),
      listenInterface = faucetConfig.getString("listen-interface"),
      listenPort = faucetConfig.getInt("listen-port"),
      minRequestInterval = faucetConfig.getDuration("min-request-interval").toMillis.millis,
      latestTimestampCacheSize = faucetConfig.getInt("latest-timestamp-cache-size"))
  }
}
