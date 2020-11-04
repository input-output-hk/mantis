package io.iohk.ethereum.faucet

import ch.megard.akka.http.cors.scaladsl.model.HttpOriginMatcher
import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.ConfigUtils

import scala.concurrent.duration.{FiniteDuration, _}

trait FaucetConfigBuilder {
  lazy val rawConfig: TypesafeConfig = ConfigFactory.load()
  lazy val rawMantisConfig: TypesafeConfig = rawConfig.getConfig("mantis")
  lazy val faucetConfig: FaucetConfig = FaucetConfig(rawConfig)
}

case class FaucetConfig(
    walletAddress: Address,
    walletPassword: String,
    txGasPrice: BigInt,
    txGasLimit: BigInt,
    txValue: BigInt,
    corsAllowedOrigins: HttpOriginMatcher,
    rpcAddress: String,
    keyStoreDir: String,
    listenInterface: String,
    listenPort: Int,
    minRequestInterval: FiniteDuration
)

object FaucetConfig {
  def apply(typesafeConfig: TypesafeConfig): FaucetConfig = {
    val faucetConfig = typesafeConfig.getConfig("faucet")

    val corsAllowedOrigins = ConfigUtils.parseCorsAllowedOrigins(faucetConfig, "cors-allowed-origins")

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
      minRequestInterval = faucetConfig.getDuration("min-request-interval").toMillis.millis
    )
  }
}
