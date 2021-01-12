package io.iohk.ethereum.security

import java.io.{ByteArrayInputStream, File, FileInputStream, FileOutputStream}
import java.security.{KeyStore, SecureRandom}

import javax.net.ssl.{KeyManager, TrustManager}
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.BufferedSource

class SSLContextFactorySpec extends AnyFlatSpec with Matchers with MockFactory with BeforeAndAfterAll {

  val fileName: String = "temp.txt"
  var file: File = _

  override def beforeAll(): Unit = {
    new FileOutputStream(fileName, false).getFD
    file = new File(fileName)
  }

  override def afterAll(): Unit = {
    file.delete()
  }

  val keyStorePath = "mantisCA.p12"
  val keyStoreType = "pkcs12"
  val passwordFile = "password"

  it should "createSSLContext" in new TestSetup(
    existingFiles = List(keyStorePath, passwordFile),
    fCreateFileInputStream = () => Right(new FileInputStream(file)),
    fLoadKeyStore = () => Right(()),
    fGetKeyManager = () => Right(Array.empty),
    fGetTrustManager = () => Right(Array.empty)
  ) {

    val sslConfig = SSLConfig(
      keyStorePath = keyStorePath,
      keyStoreType = keyStoreType,
      passwordFile = passwordFile
    )
    sSLContextFactory.createSSLContext(sslConfig, new SecureRandom()) match {
      case Right(ssl) =>
        ssl.getProtocol shouldBe "TLS"
      case Left(error) => fail(error.reason)
    }
  }

  it should "return a Error because keystore path and password are missing" in new TestSetup(
    existingFiles = Nil,
    fCreateFileInputStream = () => Right(new FileInputStream(file)),
    fLoadKeyStore = () => Right(()),
    fGetKeyManager = () => Right(Array.empty),
    fGetTrustManager = () => Right(Array.empty)
  ) {

    val sslConfig = SSLConfig(
      keyStorePath = keyStorePath,
      keyStoreType = keyStoreType,
      passwordFile = passwordFile
    )
    val response = sSLContextFactory.createSSLContext(sslConfig, new SecureRandom())
    response shouldBe Left(SSLError("Certificate keystore path and password file configured but files are missing"))
  }

  it should "return a Error because keystore path is missing" in new TestSetup(
    existingFiles = List(passwordFile),
    fCreateFileInputStream = () => Right(new FileInputStream(file)),
    fLoadKeyStore = () => Right(()),
    fGetKeyManager = () => Right(Array.empty),
    fGetTrustManager = () => Right(Array.empty)
  ) {

    val sslConfig = SSLConfig(
      keyStorePath = keyStorePath,
      keyStoreType = keyStoreType,
      passwordFile = passwordFile
    )
    val response = sSLContextFactory.createSSLContext(sslConfig, new SecureRandom())
    response shouldBe Left(SSLError("Certificate keystore path configured but file is missing"))
  }

  it should "return a Error because password file is missing" in new TestSetup(
    existingFiles = List(keyStorePath),
    fCreateFileInputStream = () => Right(new FileInputStream(file)),
    fLoadKeyStore = () => Right(()),
    fGetKeyManager = () => Right(Array.empty),
    fGetTrustManager = () => Right(Array.empty)
  ) {

    val sslConfig = SSLConfig(
      keyStorePath = keyStorePath,
      keyStoreType = keyStoreType,
      passwordFile = passwordFile
    )
    val response = sSLContextFactory.createSSLContext(sslConfig, new SecureRandom())
    response shouldBe Left(SSLError("Certificate password file configured but file is missing"))
  }

  it should "return a Error because invalid KeyStore Type" in new TestSetup(
    existingFiles = List(keyStorePath, passwordFile),
    fCreateFileInputStream = () => Right(new FileInputStream(file)),
    fLoadKeyStore = () => Right(()),
    fGetKeyManager = () => Right(Array.empty),
    fGetTrustManager = () => Right(Array.empty)
  ) {

    val invalidKeyStoreType = "invalidkeyStoreType"
    val sslConfig = SSLConfig(
      keyStorePath = keyStorePath,
      keyStoreType = invalidKeyStoreType,
      passwordFile = passwordFile
    )
    val response = sSLContextFactory.createSSLContext(sslConfig, new SecureRandom())
    response shouldBe Left(SSLError(s"Certificate keystore invalid type set: $invalidKeyStoreType"))
  }

  it should "return a Error because keystore file creation failed" in new TestSetup(
    existingFiles = List(keyStorePath, passwordFile),
    fCreateFileInputStream = () => Left(new RuntimeException("Certificate keystore file creation failed")),
    fLoadKeyStore = () => Right(()),
    fGetKeyManager = () => Right(Array.empty),
    fGetTrustManager = () => Right(Array.empty)
  ) {

    val sslConfig = SSLConfig(
      keyStorePath = keyStorePath,
      keyStoreType = keyStoreType,
      passwordFile = passwordFile
    )
    val response = sSLContextFactory.createSSLContext(sslConfig, new SecureRandom())
    response shouldBe Left(SSLError("Certificate keystore file creation failed"))
  }

  it should "return a Error because failed to load keystore" in new TestSetup(
    existingFiles = List(keyStorePath, passwordFile),
    fCreateFileInputStream = () => Right(new FileInputStream(file)),
    fLoadKeyStore = () => Left(new RuntimeException("Failed to load keyStore")),
    fGetKeyManager = () => Right(Array.empty),
    fGetTrustManager = () => Right(Array.empty)
  ) {

    val sslConfig = SSLConfig(
      keyStorePath = keyStorePath,
      keyStoreType = keyStoreType,
      passwordFile = passwordFile
    )
    val response = sSLContextFactory.createSSLContext(sslConfig, new SecureRandom())
    response shouldBe Left(SSLError("Failed to load keyStore"))
  }

  it should "return a Error because KeyManager failure" in new TestSetup(
    existingFiles = List(keyStorePath, passwordFile),
    fCreateFileInputStream = () => Right(new FileInputStream(file)),
    fLoadKeyStore = () => Right(()),
    fGetKeyManager = () => Left(new RuntimeException("Failed to get KeyManager")),
    fGetTrustManager = () => Right(Array.empty)
  ) {

    val sslConfig = SSLConfig(
      keyStorePath = keyStorePath,
      keyStoreType = keyStoreType,
      passwordFile = passwordFile
    )
    val response = sSLContextFactory.createSSLContext(sslConfig, new SecureRandom())
    response shouldBe Left(SSLError("Invalid Certificate keystore"))
  }

  it should "return a Error because TrustManager failure" in new TestSetup(
    existingFiles = List(keyStorePath, passwordFile),
    fCreateFileInputStream = () => Right(new FileInputStream(file)),
    fLoadKeyStore = () => Right(()),
    fGetKeyManager = () => Right(Array.empty),
    fGetTrustManager = () => Left(new RuntimeException("Failed to get TrustManager"))
  ) {

    val sslConfig = SSLConfig(
      keyStorePath = keyStorePath,
      keyStoreType = keyStoreType,
      passwordFile = passwordFile
    )
    val response = sSLContextFactory.createSSLContext(sslConfig, new SecureRandom())
    response shouldBe Left(SSLError("Invalid Certificate keystore"))
  }

  class TestSetup(
      existingFiles: List[String],
      fCreateFileInputStream: () => Either[Throwable, FileInputStream],
      fLoadKeyStore: () => Either[Throwable, Unit],
      fGetKeyManager: () => Either[Throwable, Array[KeyManager]],
      fGetTrustManager: () => Either[Throwable, Array[TrustManager]]
  ) {

    val sSLContextFactory = new SSLContextFactory {

      override def exist(pathName: String): Boolean = existingFiles.contains(pathName)

      override def createFileInputStream(pathName: String): Either[Throwable, FileInputStream] =
        fCreateFileInputStream()

      override def getReader(passwordFile: String): BufferedSource = new BufferedSource(
        new ByteArrayInputStream("password".getBytes)
      )

      override def loadKeyStore(
          keyStoreFile: FileInputStream,
          passwordCharArray: Array[Char],
          keyStore: KeyStore
      ): Either[Throwable, Unit] =
        fLoadKeyStore()

      override def getKeyManager(
          keyStore: KeyStore,
          passwordCharArray: Array[Char]
      ): Either[Throwable, Array[KeyManager]] = fGetKeyManager()

      override def getTrustManager(keyStore: KeyStore): Either[Throwable, Array[TrustManager]] =
        fGetTrustManager()
    }
  }
}
