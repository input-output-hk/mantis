package io.iohk.ethereum.jsonrpc.security

import java.io.FileInputStream
import java.security.{KeyStore, SecureRandom}

import javax.net.ssl.SSLContext

import scala.util.Try

case class SSLContextFactory() extends FileUtils {

  def createSSLContext(sSLConfig: SSLConfig, secureRandom: SecureRandom): Either[SSLError, SSLContext] = {
    validateCertificateFiles(
      sSLConfig.keyStorePath,
      sSLConfig.keyStoreType,
      sSLConfig.passwordFile
    ).flatMap { case (keystorePath, keystoreType, passwordFile) =>
      val passwordReader = getPasswordReader(passwordFile)
      try {
        val password = passwordReader.getLines().mkString
        obtainSSLContext(secureRandom, keystorePath, keystoreType, password)
      } finally {
        passwordReader.close()
      }
    }
  }

  /**
    * Validates that the keystore certificate file and password file were configured and that the files exists
    *
    * @param keystorePath with the path to the certificate keystore if it was configured
    * @param keystoreType for accessing the keystore with the certificate
    * @param passwordFile with the path to the password file if it was configured
    * @return the certificate path and password file or the error detected
    */
  private def validateCertificateFiles(
      keystorePath: String,
      keystoreType: String,
      passwordFile: String
  ): Either[SSLError, (String, String, String)] = {
    val keystoreDirMissing = !exist(keystorePath)
    val passwordFileMissing = !exist(passwordFile)
    if (keystoreDirMissing && passwordFileMissing)
      Left(SSLError("Certificate keystore path and password file configured but files are missing"))
    else if (keystoreDirMissing)
      Left(SSLError("Certificate keystore path configured but file is missing"))
    else if (passwordFileMissing)
      Left(SSLError("Certificate password file configured but file is missing"))
    else
      Right((keystorePath, keystoreType, passwordFile))
  }

  /**
    * Constructs the SSL context given a certificate
    *
    * @param secureRandom
    * @param keyStorePath path to the keystore where the certificate is stored
    * @param keyStoreType for accessing the keystore with the certificate
    * @param password
    * @return the SSL context with the obtained certificate or an error if any happened
    */
  private def obtainSSLContext(
      secureRandom: SecureRandom,
      keyStorePath: String,
      keyStoreType: String,
      password: String
  ): Either[SSLError, SSLContext] = {
    val passwordCharArray: Array[Char] = password.toCharArray

    val maybeKeyStore: Either[SSLError, KeyStore] = Try(KeyStore.getInstance(keyStoreType)).toOption
      .toRight(SSLError(s"Certificate keystore invalid type set: $keyStoreType"))
    val keyStoreInitResult: Either[SSLError, KeyStore] = maybeKeyStore.flatMap { keyStore =>
      val keyStoreFileCreationResult: Either[SSLError, FileInputStream] =
        createFileInputStream(keyStorePath).toOption.toRight(SSLError("Certificate keystore file creation failed"))
      keyStoreFileCreationResult.flatMap { keyStoreFile =>
        loadKeyStore(keyStoreFile, passwordCharArray, keyStore) match {
          case Right(_) =>
            Right(keyStore)
          case Left(err) =>
            Left(SSLError(err.getMessage))
        }
      }
    }

    keyStoreInitResult.flatMap { ks =>
      (for {
        km <- getKeyManager(ks, passwordCharArray)
        tm <- getTrustManager(ks)
      } yield (km, tm)) match {
        case Right((km, tm)) =>
          val sslContext: SSLContext = SSLContext.getInstance("TLS")
          sslContext.init(km, tm, secureRandom)
          Right(sslContext)
        case Left(error) =>
          log.error("Invalid Certificate keystore", error)
          Left(SSLError("Invalid Certificate keystore"))
      }
    }
  }

}
