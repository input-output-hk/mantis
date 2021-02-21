package io.iohk.ethereum.cli

import io.iohk.ethereum.keystore.EncryptedKeyJsonCodec
import io.iohk.ethereum.utils.ByteStringUtils
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CliCommandsSpec extends AnyFlatSpec with Matchers with EitherValues {

  import CliCommands._
  import Fixture._

  behavior of generatePrivateKeyCommand
  it should "generate correct private key" in {
    api.parse(Seq(generatePrivateKeyCommand)) shouldBe a[Right[_, _]]
  }

  behavior of deriveAddressCommand
  it should "derive address from private key" in {
    api.parse(Seq(deriveAddressCommand, privateKey)).value shouldBe address
  }

  it should "return an error when called without private key" in {
    api.parse(Seq(deriveAddressCommand)) shouldBe a[Left[_, _]]
  }

  behavior of generateAllocsCommand
  it should "generate correct alloc using private key" in {
    api
      .parse(
        Seq(
          generateAllocsCommand,
          argument(keyOption, Some(privateKey)),
          argument(balanceOption, Some(requestedBalance))
        )
      )
      .value shouldBe s""""alloc": {$address: { "balance": $requestedBalance }}"""
  }

  it should "generate more than one alloc" in {
    api
      .parse(
        Seq(
          generateAllocsCommand,
          argument(keyOption, Some(privateKey)),
          argument(keyOption, Some(privateKey2)),
          argument(balanceOption, Some(requestedBalance))
        )
      )
      .value shouldBe s""""alloc": {$address: { "balance": $requestedBalance }, $address2: { "balance": $requestedBalance }}"""
  }

  it should "generate allocs using addresses" in {
    api
      .parse(
        Seq(
          generateAllocsCommand,
          argument(addressOption, Some(address)),
          argument(addressOption, Some(address2)),
          argument(balanceOption, Some(requestedBalance))
        )
      )
      .value shouldBe s""""alloc": {$address: { "balance": $requestedBalance }, $address2: { "balance": $requestedBalance }}"""
  }

  it should "generate allocs using both keys and addresses" in {
    api
      .parse(
        Seq(
          generateAllocsCommand,
          argument(addressOption, Some(address)),
          argument(keyOption, Some(privateKey2)),
          argument(addressOption, Some(address3)),
          argument(balanceOption, Some(requestedBalance))
        )
      )
      .value shouldBe s""""alloc": {$address: { "balance": $requestedBalance }, $address3: { "balance": $requestedBalance }, $address2: { "balance": $requestedBalance }}"""
  }

  behavior of generateKeyPairsCommand
  it should "generate one key pair when passed no args" in {
    val result = api.parse(Seq(generateKeyPairsCommand))
    result shouldBe a[Right[_, _]]
    val stringSplit = result.toOption.get.split("\\n\\n")
    stringSplit.length shouldEqual 1
  }

  it should "generate multiple key-pair when passed correct args" in {
    val numOfKeys = "5"
    val numOfKeysAsInt = numOfKeys.toInt
    val result = api.parse(Seq(generateKeyPairsCommand, numOfKeys))
    result shouldBe a[Right[_, _]]
    val stringSplit = result.toOption.get.split("\\n\\n")
    stringSplit.length shouldEqual numOfKeysAsInt
  }

  behavior of encryptKeyCommand
  it should "encrypt private key (without passphrase)" in {
    val json = api.parse(Seq(encryptKeyCommand, privateKey)).value

    val decrypted = (for {
      encrypted <- EncryptedKeyJsonCodec.fromJson(json)
      decrypted <- encrypted.decrypt("")
    } yield decrypted).value

    ByteStringUtils.hash2string(decrypted) shouldBe privateKey
  }

  it should "encrypt private key (with passphrase)" in {
    val pass = "pass"
    val json = api.parse(Seq(encryptKeyCommand, argument(passphraseOption, Some(pass)), privateKey)).value

    val decrypted = (for {
      encrypted <- EncryptedKeyJsonCodec.fromJson(json)
      decrypted <- encrypted.decrypt(pass)
    } yield decrypted).value

    ByteStringUtils.hash2string(decrypted) shouldBe privateKey
  }
}

object Fixture {

  def argument(name: String, value: Option[Any] = None): String = s"--$name${value.fold("")(v => s"=${v.toString}")}"

  val privateKey = "00b11c32957057651d56cd83085ef3b259319057e0e887bd0fdaee657e6f75d0"
  val privateKey2 = "00b11c32957057651d56cd83085ef3b259319057e0e887bd0fdaee657e6f75d1"
  val privateKey3 = "00b11c32957057651d56cd83085ef3b259319057e0e887bd0fdaee657e6f75d2"

  val address = "c28e15ebdcbb0bdcb281d9d5084182c9c66d5d12"
  val address2 = "58240d8acb282aae883043990d8a2d7e2e75cf3b"
  val address3 = "604542f9a9fb55d3e8004ff122f662f88eb32b4a"

  val requestedBalance = 42

}
