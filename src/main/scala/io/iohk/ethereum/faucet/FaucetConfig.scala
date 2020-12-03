package io.iohk.ethereum.faucet

import com.typesafe.config.{Config, ConfigFactory}
import io.iohk.ethereum.domain.Address

import scala.concurrent.duration.{FiniteDuration, _}

trait FaucetConfigBuilder {
  lazy val rawConfig: Config = ConfigFactory.load()
  lazy val rawMantisConfig: Config = rawConfig.getConfig("mantis")
  lazy val faucetConfig: FaucetConfig = FaucetConfig(rawConfig)
}

case class RpcClientConfig(
    address: String,
    timeout: FiniteDuration
)

object RpcClientConfig {
  def apply(rpcClientConfig: Config): RpcClientConfig = {

    RpcClientConfig(
      address = rpcClientConfig.getString("rpc-address"),
      timeout = rpcClientConfig.getDuration("timeout").toMillis.millis
    )
  }
}

case class FaucetConfig(
    walletAddress: Address,
    walletPassword: String,
    txGasPrice: BigInt,
    txGasLimit: BigInt,
    txValue: BigInt,
    rpcClient: RpcClientConfig,
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
      rpcClient = RpcClientConfig(faucetConfig.getConfig("rpc-client")),
      keyStoreDir = faucetConfig.getString("keystore-dir"),
      minRequestInterval = faucetConfig.getDuration("min-request-interval").toMillis.millis,
      handlerTimeout = faucetConfig.getDuration("handler-timeout").toMillis.millis,
      responseTimeout = faucetConfig.getDuration("response-timeout").toMillis.millis,
      supervisor = SupervisorConfig(faucetConfig),
      shutdownTimeout = faucetConfig.getDuration("shutdown-timeout").toMillis.millis
    )
  }
}
