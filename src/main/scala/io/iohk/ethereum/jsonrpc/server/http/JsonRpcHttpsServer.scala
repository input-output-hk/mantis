package io.iohk.ethereum.jsonrpc.server.http

import java.security.SecureRandom

import akka.actor.ActorSystem
import akka.http.scaladsl.{ConnectionContext, Http}
import ch.megard.akka.http.cors.scaladsl.model.HttpOriginMatcher
import io.iohk.ethereum.jsonrpc.JsonRpcHealthChecker
import io.iohk.ethereum.jsonrpc.security.SSLContextFactory
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.utils.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class JsonRpcHttpsServer(
    val jsonRpcController: JsonRpcBaseController,
    val jsonRpcHealthChecker: JsonRpcHealthChecker,
    config: JsonRpcHttpServerConfig,
    secureRandom: SecureRandom
)(implicit val actorSystem: ActorSystem)
    extends JsonRpcHttpServer
    with Logger {

  def run(): Unit = {
    val maybeSslContext = SSLContextFactory.createSSLContext(config.sSLConfig, secureRandom)
    /*val maybeSslContext = validateCertificateFiles(
      config.certificateKeyStorePath,
      config.certificateKeyStoreType,
      config.certificatePasswordFile
    ).flatMap { case (keystorePath, keystoreType, passwordFile) =>
      val passwordReader = Source.fromFile(passwordFile)
      try {
        val password = passwordReader.getLines().mkString
        obtainSSLContext(keystorePath, keystoreType, password)
      } finally {
        passwordReader.close()
      }
    }*/

    val maybeHttpsContext = maybeSslContext.map(sslContext => ConnectionContext.httpsServer(sslContext))

    maybeHttpsContext match {
      case Right(httpsContext) =>
        val bindingResultF = Http().newServerAt(config.interface, config.port).enableHttps(httpsContext).bind(route)

        bindingResultF onComplete {
          case Success(serverBinding) => log.info(s"JSON RPC HTTPS server listening on ${serverBinding.localAddress}")
          case Failure(ex) => log.error("Cannot start JSON HTTPS RPC server", ex)
        }
      case Left(error) => log.error(s"Cannot start JSON HTTPS RPC server due to: $error")
    }
  }


  /*private def obtainSSLContext(
      certificateKeyStorePath: String,
      certificateKeyStoreType: String,
      password: String
  ): HttpsSetupResult[SSLContext] = {
    val passwordCharArray: Array[Char] = password.toCharArray

    val maybeKeyStore: HttpsSetupResult[KeyStore] = Try(KeyStore.getInstance(certificateKeyStoreType)).toOption
      .toRight(s"Certificate keystore invalid type set: $certificateKeyStoreType")
    val keyStoreInitResult: HttpsSetupResult[KeyStore] = maybeKeyStore.flatMap { keyStore =>
      val keyStoreFileCreationResult = Option(new FileInputStream(certificateKeyStorePath))
        .toRight("Certificate keystore file creation failed")
      keyStoreFileCreationResult.flatMap { keyStoreFile =>
        Try(keyStore.load(keyStoreFile, passwordCharArray)) match {
          case Success(_) => Right(keyStore)
          case Failure(err) => Left(err.getMessage)
        }
      }
    }

    keyStoreInitResult.map { ks =>
      val keyManagerFactory: KeyManagerFactory = KeyManagerFactory.getInstance("SunX509")
      keyManagerFactory.init(ks, passwordCharArray)

      val tmf: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
      tmf.init(ks)

      val sslContext: SSLContext = SSLContext.getInstance("TLS")
      sslContext.init(keyManagerFactory.getKeyManagers, tmf.getTrustManagers, secureRandom)
      sslContext
    }

  }


  private def validateCertificateFiles(
      maybeKeystorePath: Option[String],
      maybeKeystoreType: Option[String],
      maybePasswordFile: Option[String]
  ): HttpsSetupResult[(String, String, String)] =
    (maybeKeystorePath, maybeKeystoreType, maybePasswordFile) match {
      case (Some(keystorePath), Some(keystoreType), Some(passwordFile)) =>
        val keystoreDirMissing = !new File(keystorePath).isFile
        val passwordFileMissing = !new File(passwordFile).isFile
        if (keystoreDirMissing && passwordFileMissing)
          Left("Certificate keystore path and password file configured but files are missing")
        else if (keystoreDirMissing)
          Left("Certificate keystore path configured but file is missing")
        else if (passwordFileMissing)
          Left("Certificate password file configured but file is missing")
        else
          Right((keystorePath, keystoreType, passwordFile))
      case _ =>
        Left(
          "HTTPS requires: certificate-keystore-path, certificate-keystore-type and certificate-password-file to be configured"
        )
    }*/

  override def corsAllowedOrigins: HttpOriginMatcher = config.corsAllowedOrigins
}

/*object JsonRpcHttpsServer {
  type HttpsSetupResult[T] = Either[String, T]
}*/
