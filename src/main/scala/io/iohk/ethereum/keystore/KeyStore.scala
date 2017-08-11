package io.iohk.ethereum.keystore

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.security.SecureRandom
import java.time.format.DateTimeFormatter
import java.time.{ZoneOffset, ZonedDateTime}

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.Logger

import scala.util.Try


object KeyStore {
  sealed trait KeyStoreError
  case object KeyNotFound extends KeyStoreError
  case object DecryptionFailed extends KeyStoreError
  case object InvalidKeyFormat extends KeyStoreError
  case class IOError(msg: String) extends KeyStoreError
  case object DuplicateKeySaved extends KeyStoreError
}

import io.iohk.ethereum.keystore.KeyStore._

trait KeyStore {
  def newAccount(passphrase: String): Either[KeyStoreError, Address]

  def importPrivateKey(key: ByteString, passphrase: String): Either[KeyStoreError, Address]

  def listAccounts(): Either[KeyStoreError, List[Address]]

  def unlockAccount(address: Address, passphrase: String): Either[KeyStoreError, Wallet]
}

class KeyStoreImpl(keyStoreDir: String, secureRandom: SecureRandom) extends KeyStore with Logger {

  init()

  def newAccount(passphrase: String): Either[KeyStoreError, Address] = {
    val keyPair = generateKeyPair(secureRandom)
    val (prvKey, _) = keyPairToByteStrings(keyPair)
    val encKey = EncryptedKey(prvKey, passphrase, secureRandom)
    save(encKey).map(_ => encKey.address)
  }

  def importPrivateKey(prvKey: ByteString, passphrase: String): Either[KeyStoreError, Address] = {
    val encKey = EncryptedKey(prvKey, passphrase, secureRandom)
    save(encKey).map(_ => encKey.address)
  }

  def listAccounts(): Either[KeyStoreError, List[Address]] = {
    val dir = new File(keyStoreDir)
    Try {
      if (!dir.exists() || !dir.isDirectory())
        Left(IOError(s"Could not read $keyStoreDir"))
      else
        listFiles().map(_.flatMap(load(_).toOption).map(_.address))
    }.toEither.left.map(ioError).flatMap(identity)
  }

  def unlockAccount(address: Address, passphrase: String): Either[KeyStoreError, Wallet] =
    load(address).flatMap(_.decrypt(passphrase).left.map(_ => DecryptionFailed)).map(key => Wallet(address, key))

  private def init(): Unit = {
    val dir = new File(keyStoreDir)
    val res = Try(dir.isDirectory || dir.mkdirs()).filter(identity)
    require(res.isSuccess, s"Could not initialise keystore directory ($dir): ${res.failed.get}")
  }

  private def save(encKey: EncryptedKey): Either[KeyStoreError, Unit] = {
    val json = EncryptedKeyJsonCodec.toJson(encKey)
    val name = fileName(encKey)
    val path = Paths.get(keyStoreDir, name)

    containsAccount(encKey).flatMap { alreadyInKeyStore =>
      if(alreadyInKeyStore)
        Left(DuplicateKeySaved)
      else {
        Try {
          Files.write(path, json.getBytes(StandardCharsets.UTF_8))
          ()
        }.toEither.left.map(ioError)
      }
    }
  }

  private def load(address: Address): Either[KeyStoreError, EncryptedKey] = {
    for {
      files <- listFiles()

      matching <- files.find(_.endsWith(address.toUnprefixedString))
        .map(Right(_)).getOrElse(Left(KeyNotFound))

      key <- load(matching)
    } yield key
  }

  private def load(path: String): Either[KeyStoreError, EncryptedKey] =
    for {
      json <- Try(new String(Files.readAllBytes(Paths.get(keyStoreDir, path)), StandardCharsets.UTF_8))
        .toEither.left.map(ioError)

      key <- EncryptedKeyJsonCodec.fromJson(json)
        .left.map(_ => InvalidKeyFormat)
        .filterOrElse(k => path.endsWith(k.address.toUnprefixedString), InvalidKeyFormat)
    } yield key

  private def listFiles(): Either[KeyStoreError, List[String]] = {
    val dir = new File(keyStoreDir)
    Try {
      if (!dir.exists() || !dir.isDirectory())
        Left(IOError(s"Could not read $keyStoreDir"))
      else
        Right(dir.listFiles().toList.map(_.getName))
    }.toEither.left.map(ioError).flatMap(identity)
  }

  private def ioError(ex: Throwable): IOError =
    IOError(ex.toString)

  private def fileName(encKey: EncryptedKey) = {
    val dateStr = ZonedDateTime.now(ZoneOffset.UTC).format(DateTimeFormatter.ISO_DATE_TIME).replace(':', '-')
    val addrStr = encKey.address.toUnprefixedString
    s"UTC--$dateStr--$addrStr"
  }

  private def containsAccount(encKey: EncryptedKey): Either[KeyStoreError, Boolean] = load(encKey.address) match {
    case Right(_) => Right(true)
    case Left(KeyNotFound) => Right(false)
    case Left(err) => Left(err)
  }
}
