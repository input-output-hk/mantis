package io.iohk.ethereum.faucet

import com.typesafe.config.{ConfigFactory, Config}
import io.iohk.ethereum.domain.Address

import scala.concurrent.duration.{FiniteDuration, _}

trait FaucetConfigBuilder {
  lazy val rawConfig: Config = ConfigFactory.load()
  lazy val rawMantisConfig: Config = rawConfig.getConfig("mantis")
  lazy val faucetConfig: FaucetConfig = FaucetConfig(rawConfig)
}

case class FaucetConfig(
    walletAddress: Address,
    walletPassword: String,
    txGasPrice: BigInt,
    txGasLimit: BigInt,
    txValue: BigInt,
    rpcAddress: String,
    keyStoreDir: String,
    minRequestInterval: FiniteDuration,
    handlerTimeout: FiniteDuration,
    responseTimeout: FiniteDuration,
    supervisor: SupervisorConfig,
    shutdownTimeout: FiniteDuration
)

object FaucetConfig {
  def apply(typesafeConfig: Config): FaucetConfig = {
    val faucetConfig = typesafeConfig.getConfig("faucet")

    FaucetConfig(
      walletAddress = Address(faucetConfig.getString("wallet-address")),
      walletPassword = faucetConfig.getString("wallet-password"),
      txGasPrice = faucetConfig.getLong("tx-gas-price"),
      txGasLimit = faucetConfig.getLong("tx-gas-limit"),
      txValue = faucetConfig.getLong("tx-value"),
      rpcAddress = faucetConfig.getString("rpc-address"),
      keyStoreDir = faucetConfig.getString("keystore-dir"),
      minRequestInterval = faucetConfig.getDuration("min-request-interval").toMillis.millis,
      handlerTimeout = faucetConfig.getDuration("handler-timeout").toMillis.millis,
      responseTimeout = faucetConfig.getDuration("response-timeout").toMillis.millis,
      supervisor = SupervisorConfig(faucetConfig),
      shutdownTimeout = faucetConfig.getDuration("shutdown-timeout").toMillis.millis
    )
  }
}
