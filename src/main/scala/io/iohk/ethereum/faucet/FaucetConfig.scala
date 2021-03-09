package io.iohk.ethereum.faucet

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.domain.Address

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

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
  def apply(rpcClientConfig: Config): RpcClientConfig =
    RpcClientConfig(
      address = rpcClientConfig.getString("rpc-address"),
      timeout = rpcClientConfig.getDuration("timeout").toMillis.millis
    )
}

case class FaucetConfig(
    walletAddress: Address,
    walletPassword: String,
    txGasPrice: BigInt,
    txGasLimit: BigInt,
    txValue: BigInt,
    rpcClient: RpcClientConfig,
    keyStoreDir: String,
    handlerTimeout: FiniteDuration,
    actorCommunicationMargin: FiniteDuration,
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
      handlerTimeout = faucetConfig.getDuration("handler-timeout").toMillis.millis,
      actorCommunicationMargin = faucetConfig.getDuration("actor-communication-margin").toMillis.millis,
      supervisor = SupervisorConfig(faucetConfig),
      shutdownTimeout = faucetConfig.getDuration("shutdown-timeout").toMillis.millis
    )
  }
}
