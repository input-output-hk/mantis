package io.iohk.ethereum.jsonrpc.security

import java.io.{File, FileInputStream}
import java.security.KeyStore

import io.iohk.ethereum.utils.Logger
import javax.net.ssl.{KeyManager, KeyManagerFactory, TrustManager, TrustManagerFactory}

import scala.io.{BufferedSource, Source}
import scala.util.Try

trait FileUtils extends Logger {

  def exist(pathName: String): Boolean = new File(pathName).isFile

  def createFileInputStream(pathName: String): Either[Throwable, FileInputStream] =
    Try(new FileInputStream(pathName)).toEither match {
      case Right(fileInputStream) =>
        Option(fileInputStream).map(Right(_)).getOrElse {
          log.error("empty fileInputStream")
          Left(new RuntimeException("empty fileInputStream"))
        }
      case Left(error) =>
        log.error("create file input stream failed", error)
        Left(error)
    }

  //TODO: remove...
  def getPasswordReader(passwordFile: String): BufferedSource = Source.fromFile(passwordFile)

  def loadKeyStore(
      keyStoreFile: FileInputStream,
      passwordCharArray: Array[Char],
      keyStore: KeyStore
  ): Either[Throwable, Unit] =
    Try(keyStore.load(keyStoreFile, passwordCharArray)).toEither

  def getKeyManager(keyStore: KeyStore, passwordCharArray: Array[Char]): Either[Throwable, Array[KeyManager]] =
    Try {
      val keyManagerFactory: KeyManagerFactory = KeyManagerFactory.getInstance("SunX509")
      keyManagerFactory.init(keyStore, passwordCharArray)
      keyManagerFactory.getKeyManagers
    }.toEither match {
      case Right(keyManager) => Right(keyManager)
      case Left(error) =>
        log.error("") //TODO..
        Left(error)
    }

  def getTrustManager(keyStore: KeyStore): Either[Throwable, Array[TrustManager]] = Try {
    val tmf: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
    tmf.init(keyStore)
    tmf.getTrustManagers
  }.toEither match {
    case Right(trustManager) => Right(trustManager)
    case Left(error) =>
      log.error("") //TODO..
      Left(error)
  }

}
