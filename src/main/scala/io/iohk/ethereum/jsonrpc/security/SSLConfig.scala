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
          keyStorePath = config.getString("certificate.keystore-path"),
          keyStoreType = config.getString("certificate.keystore-type"),
          passwordFile = config.getString("certificate.password-file")
        )
      )
    }
  }

}
