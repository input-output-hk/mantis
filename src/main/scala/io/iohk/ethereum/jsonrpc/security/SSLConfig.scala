package io.iohk.ethereum.jsonrpc.security

import com.typesafe.config.Config

import scala.util.Try

case class SSLConfig(
  certificateKeyStorePath: Option[String],
  certificateKeyStoreType: Option[String],
  certificatePasswordFile: Option[String]
)

object SSLConfig {
  def apply(config: Config): SSLConfig = {
    SSLConfig(
      certificateKeyStorePath = Try(
      config.getString("certificate-keystore-path")
    ).toOption,
      certificateKeyStoreType = Try(
      config.getString("certificate-keystore-type")
    ).toOption,
      certificatePasswordFile = Try(
      config.getString("certificate-password-file")
    ).toOption)
  }

}
