package io.iohk.ethereum.jsonrpc.security

import com.typesafe.config.Config

case class SSLConfig(
    keyStorePath: String,
    keyStoreType: String,
    passwordFile: String
)

object SSLConfig {

  val key = "certificate"

  def apply(config: Config): Option[SSLConfig] = {
    if (config.getIsNull(key))
      None
    else {
      Some(
        SSLConfig(
          keyStorePath = config.getString(s"$key.keystore-path"),
          keyStoreType = config.getString(s"$key.keystore-type"),
          passwordFile = config.getString(s"$key.password-file")
        )
      )
    }
  }

}
