package io.iohk.ethereum.security
import com.typesafe.config.ConfigFactory
import javax.net.ssl.SSLContext

case class SSLError(reason: String)

trait SSLContextBuilder { self: SecureRandomBuilder =>

  private lazy val rpcHttpConfig = ConfigFactory.load().getConfig("mantis.network.rpc.http")
  private lazy val sslConfig: Option[SSLConfig] = SSLConfig(rpcHttpConfig)

  lazy val sslContext: Either[SSLError, SSLContext] =
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
