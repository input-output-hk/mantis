package io.iohk.ethereum.jsonrpc.server

import java.io.{File, FileInputStream}
import java.security.{KeyStore, SecureRandom}
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}

import akka.actor.ActorSystem
import akka.http.scaladsl.model.headers.HttpOriginRange
import akka.http.scaladsl.{ConnectionContext, Http}
import akka.stream.ActorMaterializer
import io.iohk.ethereum.jsonrpc.JsonRpcController
import io.iohk.ethereum.jsonrpc.server.JsonRpcHttpsServer.HttpsSetupResult
import io.iohk.ethereum.jsonrpc.server.JsonRpcServer.JsonRpcServerConfig
import io.iohk.ethereum.utils.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.util.{Failure, Success, Try}

class JsonRpcHttpsServer(val jsonRpcController: JsonRpcController, config: JsonRpcServerConfig,
                         secureRandom: SecureRandom)(implicit val actorSystem: ActorSystem)
  extends JsonRpcServer with Logger {

  def run(): Unit = {
    implicit val materializer = ActorMaterializer()

    val maybeSslContext = validateCertificateFiles(config.certificateKeyStorePath, config.certificateKeyStoreType, config.certificatePasswordFile).flatMap{
      case (keystorePath, keystoreType, passwordFile) =>
        val passwordReader = Source.fromFile(passwordFile)
        try {
          val password = passwordReader.getLines().mkString
          obtainSSLContext(keystorePath, keystoreType, password)
        } finally {
          passwordReader.close()
        }
    }

    val maybeHttpsContext = maybeSslContext.map(sslContext => ConnectionContext.https(sslContext))

    maybeHttpsContext match {
      case Right(httpsContext) =>
        Http().setDefaultServerHttpContext(httpsContext)
        val bindingResultF = Http().bindAndHandle(route, config.interface, config.port, connectionContext = httpsContext)

        bindingResultF onComplete {
          case Success(serverBinding) => log.info(s"JSON RPC HTTPS server listening on ${serverBinding.localAddress}")
          case Failure(ex) => log.error("Cannot start JSON HTTPS RPC server", ex)
        }
      case Left(error) => log.error(s"Cannot start JSON HTTPS RPC server due to: $error")
    }
  }

  /**
    * Constructs the SSL context given a certificate
    *
    * @param certificateKeyStorePath, path to the keystore where the certificate is stored
    * @param password for accessing the keystore with the certificate
    * @return the SSL context with the obtained certificate or an error if any happened
    */
  private def obtainSSLContext(certificateKeyStorePath: String, certificateKeyStoreType: String, password: String): HttpsSetupResult[SSLContext] = {
    val passwordCharArray: Array[Char] = password.toCharArray

    val maybeKeyStore: HttpsSetupResult[KeyStore] = Try(KeyStore.getInstance(certificateKeyStoreType))
      .toOption.toRight(s"Certificate keystore invalid type set: $certificateKeyStoreType")
    val keyStoreInitResult: HttpsSetupResult[KeyStore] = maybeKeyStore.flatMap{ keyStore =>
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

  /**
    * Validates that the keystore certificate file and password file were configured and that the files exists
    *
    * @param maybeKeystorePath, with the path to the certificate keystore if it was configured
    * @param maybePasswordFile, with the path to the password file if it was configured
    * @return the certificate path and password file or the error detected
    */
  private def validateCertificateFiles(maybeKeystorePath: Option[String],
                                       maybeKeystoreType: Option[String],
                                       maybePasswordFile: Option[String]): HttpsSetupResult[(String, String, String)] =
    (maybeKeystorePath, maybeKeystoreType, maybePasswordFile) match {
      case (Some(keystorePath), Some(keystoreType), Some(passwordFile)) =>
        val keystoreDirMissing = !new File(keystorePath).isFile
        val passwordFileMissing = !new File(passwordFile).isFile
        if(keystoreDirMissing && passwordFileMissing)
          Left("Certificate keystore path and password file configured but files are missing")
        else if(keystoreDirMissing)
          Left("Certificate keystore path configured but file is missing")
        else if(passwordFileMissing)
          Left("Certificate password file configured but file is missing")
        else
          Right((keystorePath, keystoreType, passwordFile))
      case _ =>
        Left("HTTPS requires: certificate-keystore-path, certificate-keystore-type and certificate-password-file to be configured")
    }

  override def corsAllowedOrigins: HttpOriginRange = config.corsAllowedOrigins
}

object JsonRpcHttpsServer {
  type HttpsSetupResult[T] = Either[String, T]
}
