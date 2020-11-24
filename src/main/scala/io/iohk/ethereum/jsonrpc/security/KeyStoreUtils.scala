package io.iohk.ethereum.jsonrpc.security

import java.io.FileInputStream
import java.security.KeyStore

import io.iohk.ethereum.utils.Logger
import javax.net.ssl.{KeyManager, KeyManagerFactory, TrustManager, TrustManagerFactory}

import scala.util.Try

trait KeyStoreUtils extends Logger {

  lazy val algorithm: String = "SunX509"

  def loadKeyStore(
      keyStoreFile: FileInputStream,
      passwordCharArray: Array[Char],
      keyStore: KeyStore
  ): Either[Throwable, Unit] =
    Try(keyStore.load(keyStoreFile, passwordCharArray)).toEither

  def getKeyManager(keyStore: KeyStore, passwordCharArray: Array[Char]): Either[Throwable, Array[KeyManager]] =
    Try {
      val keyManagerFactory: KeyManagerFactory = KeyManagerFactory.getInstance(algorithm)
      keyManagerFactory.init(keyStore, passwordCharArray)
      keyManagerFactory.getKeyManagers
    }.toEither match {
      case Right(keyManager) => Right(keyManager)
      case Left(error) =>
        log.error("getKeyManagers failure", error)
        Left(error)
    }

  def getTrustManager(keyStore: KeyStore): Either[Throwable, Array[TrustManager]] = Try {
    val tmf: TrustManagerFactory = TrustManagerFactory.getInstance(algorithm)
    tmf.init(keyStore)
    tmf.getTrustManagers
  }.toEither match {
    case Right(trustManager) => Right(trustManager)
    case Left(error) =>
      log.error("getTrustManagers failure, error")
      Left(error)
  }
}
