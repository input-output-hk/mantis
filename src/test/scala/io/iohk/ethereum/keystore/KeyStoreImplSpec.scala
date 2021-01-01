package io.iohk.ethereum.keystore

import java.io.File
import java.nio.file.{FileSystemException, FileSystems, Files, Path}

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.keystore.KeyStore.{DecryptionFailed, IOError, KeyNotFound, PassPhraseTooShort}
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.{Config, KeyStoreConfig}
import org.apache.commons.io.FileUtils
import org.bouncycastle.util.encoders.Hex
import org.scalatest.BeforeAndAfter

import scala.util.Try
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class KeyStoreImplSpec extends AnyFlatSpec with Matchers with BeforeAndAfter with SecureRandomBuilder {

  before(clearKeyStore())

  "KeyStoreImpl" should "import and list accounts" in new TestSetup {
    val listBeforeImport = keyStore.listAccounts().toOption.get
    listBeforeImport shouldEqual Nil

    // We sleep between imports so that dates of keyfiles' names are different
    val res1 = keyStore.importPrivateKey(key1, "aaaaaaaa").toOption.get
    Thread.sleep(1005)
    val res2 = keyStore.importPrivateKey(key2, "bbbbbbbb").toOption.get
    Thread.sleep(1005)
    val res3 = keyStore.importPrivateKey(key3, "cccccccc").toOption.get

    res1 shouldEqual addr1
    res2 shouldEqual addr2
    res3 shouldEqual addr3

    val listAfterImport = keyStore.listAccounts().toOption.get
    // result should be ordered by creation date
    listAfterImport shouldEqual List(addr1, addr2, addr3)
  }

  it should "fail to import a key twice" in new TestSetup {
    val resAfterFirstImport = keyStore.importPrivateKey(key1, "aaaaaaaa")
    val resAfterDupImport = keyStore.importPrivateKey(key1, "aaaaaaaa")

    resAfterFirstImport shouldEqual Right(addr1)
    resAfterDupImport shouldBe Left(KeyStore.DuplicateKeySaved)

    //Only the first import succeeded
    val listAfterImport = keyStore.listAccounts().toOption.get
    listAfterImport.toSet shouldEqual Set(addr1)
    listAfterImport.length shouldEqual 1
  }

  it should "create new accounts" in new TestSetup {
    val newAddr1 = keyStore.newAccount("aaaaaaaa").toOption.get
    val newAddr2 = keyStore.newAccount("bbbbbbbb").toOption.get

    val listOfNewAccounts = keyStore.listAccounts().toOption.get
    listOfNewAccounts.toSet shouldEqual Set(newAddr1, newAddr2)
    listOfNewAccounts.length shouldEqual 2
  }

  it should "fail to create account with too short passphrase" in new TestSetup {
    val res1 = keyStore.newAccount("aaaaaaa")
    res1 shouldEqual Left(PassPhraseTooShort(keyStoreConfig.minimalPassphraseLength))
  }

  it should "allow 0 length passphrase when configured" in new TestSetup {
    val res1 = keyStore.newAccount("")
    assert(res1.isRight)
  }

  it should "not allow 0 length passphrase when configured" in new TestSetup {
    val newKeyStore = getKeyStore(noEmptyAllowedConfig)
    val res1 = newKeyStore.newAccount("")
    res1 shouldBe Left(PassPhraseTooShort(noEmptyAllowedConfig.minimalPassphraseLength))
  }

  it should "not allow too short password, when empty is allowed" in new TestSetup {
    val newKeyStore = getKeyStore(noEmptyAllowedConfig)
    val res1 = newKeyStore.newAccount("asdf")
    res1 shouldBe Left(PassPhraseTooShort(noEmptyAllowedConfig.minimalPassphraseLength))
  }

  it should "allow to create account with proper length passphrase, when empty is allowed" in new TestSetup {
    val newKeyStore = getKeyStore(noEmptyAllowedConfig)
    val res1 = newKeyStore.newAccount("aaaaaaaa")
    assert(res1.isRight)
  }

  it should "return an error when the keystore dir cannot be initialized" in new TestSetup {
    assertThrows[FileSystemException] {
      new KeyStoreImpl(testFailingPathConfig, secureRandom)
    }
  }

  it should "return an error when the keystore dir cannot be read or written" in new TestSetup {
    clearKeyStore()

    val key = ByteString(Hex.decode("7a44789ed3cd85861c0bbf9693c7e1de1862dd4396c390147ecf1275099c6e6f"))
    val res1 = keyStore.importPrivateKey(key, "aaaaaaaa")
    res1 should matchPattern { case Left(IOError(_)) => }

    val res2 = keyStore.newAccount("aaaaaaaa")
    res2 should matchPattern { case Left(IOError(_)) => }

    val res3 = keyStore.listAccounts()
    res3 should matchPattern { case Left(IOError(_)) => }
  }

  it should "unlock an account provided a correct passphrase" in new TestSetup {
    val passphrase = "aaaaaaaa"
    keyStore.importPrivateKey(key1, passphrase)
    val wallet = keyStore.unlockAccount(addr1, passphrase).toOption.get
    wallet shouldEqual Wallet(addr1, key1)
  }

  it should "return an error when unlocking an account with a wrong passphrase" in new TestSetup {
    keyStore.importPrivateKey(key1, "aaaaaaaa")
    val res = keyStore.unlockAccount(addr1, "bbb")
    res shouldEqual Left(DecryptionFailed)
  }

  it should "return an error when trying to unlock an unknown account" in new TestSetup {
    val res = keyStore.unlockAccount(addr1, "bbb")
    res shouldEqual Left(KeyNotFound)
  }

  trait TestSetup {
    val keyStoreConfig = KeyStoreConfig(Config.config)

    object testFailingPathConfig extends KeyStoreConfig {

      override val allowNoPassphrase: Boolean = keyStoreConfig.allowNoPassphrase
      override val keyStoreDir: String = {
        val tmpDir: Path = Files.createTempDirectory("mentis-keystore")
        val principalLookupService = FileSystems.getDefault.getUserPrincipalLookupService
        val rootOrAdminPrincipal = Try { principalLookupService.lookupPrincipalByName("root") }.orElse(Try {
          principalLookupService.lookupPrincipalByName("Administrator")
        })
        Files.setOwner(tmpDir, rootOrAdminPrincipal.get)
        tmpDir.toString
      }
      override val minimalPassphraseLength: Int = keyStoreConfig.minimalPassphraseLength
    }
    object noEmptyAllowedConfig extends KeyStoreConfig {
      override val allowNoPassphrase: Boolean = false
      override val keyStoreDir: String = keyStoreConfig.keyStoreDir
      override val minimalPassphraseLength: Int = keyStoreConfig.minimalPassphraseLength
    }

    val keyStore = new KeyStoreImpl(keyStoreConfig, secureRandom)

    def getKeyStore(config: KeyStoreConfig): KeyStoreImpl = {
      new KeyStoreImpl(config, secureRandom)
    }

    val key1 = ByteString(Hex.decode("7a44789ed3cd85861c0bbf9693c7e1de1862dd4396c390147ecf1275099c6e6f"))
    val addr1 = Address(Hex.decode("aa6826f00d01fe4085f0c3dd12778e206ce4e2ac"))
    val key2 = ByteString(Hex.decode("ee9fb343c34856f3e64f6f0b5e2abd1b298aaa76d0ffc667d00eac4582cb69ca"))
    val addr2 = Address(Hex.decode("f1c8084f32b8ef2cee7099446d9a6a185d732468"))
    val key3 = ByteString(Hex.decode("ed341f91661a05c249c36b8c9f6d3b796aa9f629f07ddc73b04b9ffc98641a50"))
    val addr3 = Address(Hex.decode("d2ecb1332a233d314c30fe3b53f44541b7a07a9e"))
  }

  def clearKeyStore(): Unit = {
    FileUtils.deleteDirectory(new File(KeyStoreConfig(Config.config).keyStoreDir))
  }
}
