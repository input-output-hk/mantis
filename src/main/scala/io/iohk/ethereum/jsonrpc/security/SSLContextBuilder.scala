package io.iohk.ethereum.jsonrpc.security
//import com.typesafe.config.Config
//import com.typesafe.config.ConfigFactory
//import javax.net.ssl.SSLContext


case class SSLError(reason: String) //TODO: throwable exception

/*trait SSLConfigBuilder {
  lazy val networkConfig: Config = ConfigFactory.load().getConfig("mantis.network")
  lazy val sslConfig: Option[SSLConfig] = SSLConfig(networkConfig)
}

class SSLContextBuilder { self: SSLConfigBuilder with SecureRandomBuilder =>
  lazy val sslContext: Either[SSLError, SSLContext] =
    sslConfig
      .toRight(SSLError("No SSL config present"))
      .flatMap(SSLContextFactory.createSSLContext(_, secureRandom))

}*/
