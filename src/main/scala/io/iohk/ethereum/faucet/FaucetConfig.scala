package io.iohk.ethereum.faucet

import akka.http.scaladsl.model.headers.HttpOriginRange
import akka.util.ByteString
import com.typesafe.config.{Config => TypesafeConfig}
import io.iohk.ethereum.utils.ConfigUtils
import org.spongycastle.util.encoders.Hex

import scala.concurrent.duration.{FiniteDuration, _}

case class FaucetConfig(
    keyfile: String,
    keyStoreDir: String,
    walletPassword: String,
    txGasPrice: BigInt,
    txGasLimit: BigInt,
    txValue: BigInt,
    txData: ByteString,
    corsAllowedOrigins: HttpOriginRange,
    rpcAddress: String,
    listenInterface: String,
    listenPort: Int,
    minRequestInterval: FiniteDuration,
    latestTimestampCacheSize: Int)

object FaucetConfig {
  def apply(typesafeConfig: TypesafeConfig): FaucetConfig = {
    val faucetConfig = typesafeConfig.getConfig("faucet")

    val corsAllowedOrigins = ConfigUtils.parseCorsAllowedOrigins(faucetConfig, "cors-allowed-origins")

    FaucetConfig(
      keyfile = faucetConfig.getString("keyfile"),
      keyStoreDir = faucetConfig.getString("keystore-dir"),
      walletPassword = faucetConfig.getString("wallet-password"),
      txGasPrice = faucetConfig.getLong("tx-gas-price"),
      txGasLimit = faucetConfig.getLong("tx-gas-limit"),
      txValue = faucetConfig.getLong("tx-value"),
      txData = ByteString(Hex.decode(faucetConfig.getString("tx-data"))),
      corsAllowedOrigins = corsAllowedOrigins,
      rpcAddress = faucetConfig.getString("rpc-address"),
      listenInterface = faucetConfig.getString("listen-interface"),
      listenPort = faucetConfig.getInt("listen-port"),
      minRequestInterval = faucetConfig.getDuration("min-request-interval").toMillis.millis,
      latestTimestampCacheSize = faucetConfig.getInt("latest-timestamp-cache-size"))
  }
}
