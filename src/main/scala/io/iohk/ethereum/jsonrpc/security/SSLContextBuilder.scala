package io.iohk.ethereum.jsonrpc.security
import com.typesafe.config.ConfigFactory
import javax.net.ssl.SSLContext

case class SSLError(reason: String) //TODO: throwable exception

trait SSLContextBuilder { self: SecureRandomBuilder =>

  private lazy val rpcHttpConfig = ConfigFactory.load().getConfig("mantis.network.rpc.http")
  private lazy val sSLConfig: SSLConfig = SSLConfig(rpcHttpConfig)

  lazy val sslContext: Either[SSLError, SSLContext] = SSLContextFactory.createSSLContext(sSLConfig, secureRandom)

}
