package io.iohk.ethereum.security
import com.typesafe.config.ConfigFactory

import javax.net.ssl.SSLContext

case class SSLError(reason: String)

trait SSLContextBuilder { self: SecureRandomBuilder =>

  def sslContext(key: String): Either[SSLError, SSLContext] =
    SSLConfig(ConfigFactory.load().getConfig(key))
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
