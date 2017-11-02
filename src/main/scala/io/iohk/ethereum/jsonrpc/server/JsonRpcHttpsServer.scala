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

    val maybeSslContext = validateCertificateFiles(config.certificateKeyStorePath, config.certificatePasswordFile).flatMap{
      case (certificatePath, passwordFile) =>
        val passwordReader = Source.fromFile(passwordFile)
        try {
          val password = passwordReader.getLines().mkString
          obtainSSLContext(certificatePath, password)
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
  private def obtainSSLContext(certificateKeyStorePath: String, password: String): HttpsSetupResult[SSLContext] = {
    val passwordCharArray: Array[Char] = password.toCharArray

    val ks: KeyStore = KeyStore.getInstance("JKS")
    val keyStoreInitResult: HttpsSetupResult[Unit] = Option(new FileInputStream(certificateKeyStorePath))
      .toRight("Certificate keystore creation failed")
      .flatMap { keyStore =>
        Try(ks.load(keyStore, passwordCharArray)).toEither.left.map( _.getMessage) }

    keyStoreInitResult.map { _ =>
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
    * @param maybeCertificatePath, with the path to the keystore certificate if it was configured
    * @param maybePasswordFile, with the path to the password file if it was configured
    * @return the certificate path and password file or the error detected
    */
  private def validateCertificateFiles(maybeCertificatePath: Option[String],
                                       maybePasswordFile: Option[String]): HttpsSetupResult[(String, String)] =
    (maybeCertificatePath, maybePasswordFile) match {
      case (Some(certificatePath), Some(passwordFile)) =>
        val certificateFileMissing = !new File(certificatePath).exists()
        val passwordFileMissing = !new File(passwordFile).exists()
        if(certificateFileMissing && passwordFileMissing)
          Left("Certificate keystore path and password file configured but files are missing")
        else if(certificateFileMissing)
          Left("Certificate keystore path configured but file is missing")
        else if(passwordFileMissing)
          Left("Certificate password file configured but file is missing")
        else
          Right(certificatePath -> passwordFile)
      case (None, None) => Left("Certificate keystore path and password file configuration required")
      case (None, _) => Left("Certificate keystore path configuration required")
      case (_, None) => Left("Certificate password file configuration required")
    }

  override def corsAllowedOrigins: HttpOriginRange = config.corsAllowedOrigins
}

object JsonRpcHttpsServer {
  type HttpsSetupResult[T] = Either[String, T]
}
