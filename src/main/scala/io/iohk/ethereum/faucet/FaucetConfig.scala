package io.iohk.ethereum.faucet

import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}
import io.iohk.ethereum.domain.Address

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
    rpcAddress: String,
    keyStoreDir: String,
    minRequestInterval: FiniteDuration
)

object FaucetConfig {
  def apply(typesafeConfig: TypesafeConfig): FaucetConfig = {
    val faucetConfig = typesafeConfig.getConfig("faucet")

    FaucetConfig(
      walletAddress = Address(faucetConfig.getString("wallet-address")),
      walletPassword = faucetConfig.getString("wallet-password"),
      txGasPrice = faucetConfig.getLong("tx-gas-price"),
      txGasLimit = faucetConfig.getLong("tx-gas-limit"),
      txValue = faucetConfig.getLong("tx-value"),
      rpcAddress = faucetConfig.getString("rpc-address"),
      keyStoreDir = faucetConfig.getString("keystore-dir"),
      minRequestInterval = faucetConfig.getDuration("min-request-interval").toMillis.millis
    )
  }
}
