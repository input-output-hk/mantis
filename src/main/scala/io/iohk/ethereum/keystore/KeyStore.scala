package io.iohk.ethereum.keystore

import java.io.File
import java.nio.file.{Files, Paths}

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.Logger
import org.spongycastle.util.encoders.Hex

import scala.util.Try


object KeyStore {
  sealed trait KeyStoreError
  case object WrongPassphrase extends KeyStoreError
  case object KeyNotFound extends KeyStoreError
  case class IOError(msg: String) extends KeyStoreError
}

import io.iohk.ethereum.keystore.KeyStore._

trait KeyStore {
  def newAccount(passphrase: String): Either[KeyStoreError, Address]

  def importPrivateKey(key: Array[Byte], passphrase: String): Either[KeyStoreError, Address]

  def listAccounts(): Either[KeyStoreError, List[Address]]

  def unlockAccount(address: Address, passphrase: String): Either[KeyStoreError, Wallet]
}

class KeyStoreImpl(keyStoreDir: String) extends KeyStore with Logger {

  init()

  def newAccount(passphrase: String): Either[KeyStoreError, Address] = {
    val keyPair = generateKeyPair()
    val (prvKey, pubKey) = keyPairToByteArrays(keyPair)
    val address = Address(kec256(pubKey))
    save(address, prvKey, passphrase).map(_ => address)
  }

  def importPrivateKey(prvKey: Array[Byte], passphrase: String): Either[KeyStoreError, Address] = {
    val pubKey = pubKeyFromPrvKey(prvKey)
    val address = Address(kec256(pubKey))
    save(address, prvKey, passphrase).map(_ => address)
  }

  def listAccounts(): Either[KeyStoreError, List[Address]] = {
    val dir = new File(keyStoreDir)
    Try {
      if (!dir.exists() || !dir.isDirectory())
        Left(IOError(s"Could not read $keyStoreDir"))
      else
        Right(
          dir.listFiles().toList.flatMap(f =>
            Try(Address(Hex.decode(f.getName))).toOption
          )
        )
    }.toEither.left.map(ioError).flatMap(identity)
  }

  def unlockAccount(address: Address, passphrase: String): Either[KeyStoreError, Wallet] =
    load(address, passphrase).map(bytes => Wallet(address, ByteString(bytes)))

  private def init(): Unit = {
    val dir = new File(keyStoreDir)
    val res = Try(dir.mkdirs()).filter(identity)
    res.failed.foreach(ex => log.error(s"Could not initialise keystore directory ($dir): $ex"))
  }

  private def save(address: Address, key: Array[Byte], passphrase: String): Either[KeyStoreError, Unit] = {
    val addrString = Hex.toHexString(address.toArray)
    val prvKeyString = Hex.encode(key)
    val path = Paths.get(keyStoreDir, addrString)
    Try {
      Files.write(path, prvKeyString)
      ()
    }.toEither.left.map(ioError)
  }

  private def load(address: Address, passphrase: String): Either[KeyStoreError, Array[Byte]] = {
    val addrString = Hex.toHexString(address.toArray)
    val path = Paths.get(keyStoreDir, addrString)

    if (!path.toFile.isFile)
      Left(KeyNotFound)
    else
      Try {
        Hex.decode(Files.readAllBytes(path))
      }.toEither.left.map(ioError)
  }

  private def ioError(ex: Throwable): IOError =
    IOError(ex.toString)
}
