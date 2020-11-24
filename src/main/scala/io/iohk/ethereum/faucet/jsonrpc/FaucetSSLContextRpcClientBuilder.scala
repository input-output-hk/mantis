package io.iohk.ethereum.faucet.jsonrpc

import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.security.{SSLConfig, SSLContextFactory, SSLError, SecureRandomBuilder}
import javax.net.ssl.SSLContext

trait FaucetSSLContextRpcClientBuilder {
  self: SecureRandomBuilder =>

  private lazy val sslConfig: Option[SSLConfig] = SSLConfig(ConfigFactory.load().getConfig("faucet"))

  lazy val sslContextRPCClient: Either[SSLError, SSLContext] =
    sslConfig
      .toRight(SSLError("No SSL config present"))
      .flatMap(SSLContextFactory().createSSLContext(_, secureRandom)) match {
      case Right(sslConfig) =>
        log.debug("Loaded ssl config successful")
        Right(sslConfig)
      case Left(error) =>
        log.error(s"Loaded ssl config failure - $error")
        Left(error)
    }

}
