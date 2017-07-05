package io.iohk.ethereum.keystore

import java.io.File

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.keystore.KeyStore.{DecryptionFailed, IOError, KeyNotFound}
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.utils.Config
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex
import org.apache.commons.io.FileUtils

class KeyStoreImplSpec extends FlatSpec with Matchers with BeforeAndAfter with SecureRandomBuilder {

  before(clearKeyStore())

  "KeyStoreImpl" should "import and list accounts" in new TestSetup {
    val listBeforeImport = keyStore.listAccounts().right.get
    listBeforeImport shouldEqual Nil

    val res1 = keyStore.importPrivateKey(key1, "aaa").right.get
    val res2 = keyStore.importPrivateKey(key2, "bbb").right.get

    res1 shouldEqual addr1
    res2 shouldEqual addr2

    val listAfterImport = keyStore.listAccounts().right.get
    listAfterImport.toSet shouldEqual Set(addr1, addr2)
    listAfterImport.length shouldEqual 2
  }

  it should "create new accounts" in new TestSetup {
    val newAddr1 = keyStore.newAccount("aaa").right.get
    val newAddr2 = keyStore.newAccount("bbb").right.get

    val listOfNewAccounts = keyStore.listAccounts().right.get
    listOfNewAccounts.toSet shouldEqual Set(newAddr1, newAddr2)
    listOfNewAccounts.length shouldEqual 2
  }

  it should "return an error when the keystore dir cannot be read or written" in new TestSetup {
    val badKeyStore = new KeyStoreImpl("/root/keystore", secureRandom)

    val key = ByteString(Hex.decode("7a44789ed3cd85861c0bbf9693c7e1de1862dd4396c390147ecf1275099c6e6f"))
    val res1 = badKeyStore.importPrivateKey(key, "aaa")
    res1 should matchPattern { case Left(IOError(_)) => }

    val res2 = badKeyStore.newAccount("aaa")
    res2 should matchPattern { case Left(IOError(_)) => }

    val res3 = badKeyStore.listAccounts()
    res3 should matchPattern { case Left(IOError(_)) => }
  }

  it should "unlock an account provided a correct passphrase" in new TestSetup {
    val passphrase = "aaa"
    keyStore.importPrivateKey(key1, passphrase)
    val wallet = keyStore.unlockAccount(addr1, passphrase).right.get
    wallet shouldEqual Wallet(addr1, key1)
  }

  it should "return an error when unlocking an account with a wrong passphrase" in new TestSetup {
    keyStore.importPrivateKey(key1, "aaa")
    val res = keyStore.unlockAccount(addr1, "bbb")
    res shouldEqual Left(DecryptionFailed)
  }

  it should "return an error when trying to unlock an unknown account" in new TestSetup {
    val res = keyStore.unlockAccount(addr1, "bbb")
    res shouldEqual Left(KeyNotFound)
  }

  trait TestSetup {
    val keyStore = new KeyStoreImpl(Config.keyStoreDir, secureRandom)

    val key1 = ByteString(Hex.decode("7a44789ed3cd85861c0bbf9693c7e1de1862dd4396c390147ecf1275099c6e6f"))
    val addr1 = Address(Hex.decode("aa6826f00d01fe4085f0c3dd12778e206ce4e2ac"))
    val key2 = ByteString(Hex.decode("ee9fb343c34856f3e64f6f0b5e2abd1b298aaa76d0ffc667d00eac4582cb69ca"))
    val addr2 = Address(Hex.decode("f1c8084f32b8ef2cee7099446d9a6a185d732468"))
  }

  def clearKeyStore(): Unit = {
    FileUtils.deleteDirectory(new File(Config.keyStoreDir))
  }
}
