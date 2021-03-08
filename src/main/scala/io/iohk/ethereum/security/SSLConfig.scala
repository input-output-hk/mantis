package io.iohk.ethereum.security

import com.typesafe.config.Config

case class SSLConfig(
    keyStorePath: String,
    keyStoreType: String,
    passwordFile: String
)

object SSLConfig {

  val key = "certificate"

  def apply(config: Config): Option[SSLConfig] =
    if (config.getIsNull(key))
      None
    else {
      val certificateConfig = config.getConfig(key)
      Some(
        SSLConfig(
          keyStorePath = certificateConfig.getString("keystore-path"),
          keyStoreType = certificateConfig.getString("keystore-type"),
          passwordFile = certificateConfig.getString("password-file")
        )
      )
    }

}
