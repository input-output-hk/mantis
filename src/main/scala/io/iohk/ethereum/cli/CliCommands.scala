package io.iohk.ethereum.cli

import cats.implicits._
import com.monovore.decline.{Command, Opts}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.ByteStringUtils
import java.security.SecureRandom
import org.bouncycastle.util.encoders.Hex

object CliCommands {

  val generatePrivateKeyCommand = "generate-private-key"
  val deriveAddressCommand = "derive-address"
  val generateAllocsCommand = "generate-allocs"
  val balanceOption = "balance"
  val keyOption = "key"
  val addressOption = "address"

  private val GeneratePrivateKeyCommand: Command[String] =
    Command(name = generatePrivateKeyCommand, header = "Generate private key") {
      Opts.unit.map { _ =>
        val keyPair = generateKeyPair(new SecureRandom())
        val (prvKey, _) = keyPairToByteStrings(keyPair)
        ByteStringUtils.hash2string(prvKey)
      }
    }

  private val DeriveAddressFromPrivateKey: Command[String] =
    Command(name = deriveAddressCommand, header = "Derive address from private key") {

      Opts
        .argument[String]("private-key")
        .map(Hex.decode)
        .map(privKeyToAddress)
    }

  private val GenerateAllocs: Command[String] =
    Command(name = generateAllocsCommand, header = "Generate genesis allocs") {

      val keysOpt: Opts[List[String]] =
        Opts
          .options[String](long = keyOption, help = "Private key")
          .map(_.map(key => privKeyToAddress(Hex.decode(key))))
          .orEmpty

      val addressesOpt: Opts[List[String]] =
        Opts.options[String](long = addressOption, help = "Address").orEmpty

      val balanceOpt =
        Opts.option[BigInt](long = balanceOption, help = "Initial balance for account", metavar = "balance")

      (keysOpt, addressesOpt, balanceOpt).mapN { (addressesFromKeys, addresses, balance) =>
        allocs(addresses ++ addressesFromKeys, balance)
      }
    }

  private def allocs(addresses: List[String], balance: BigInt): String =
    s""""alloc": ${addresses
      .map(address => s"""$address: { "balance": $balance }""")
      .mkString("{", ", ", "}")}"""

  private def privKeyToAddress(privKey: Array[Byte]): String = {
    val pubKey = pubKeyFromPrvKey(privKey)
    val address = Address(crypto.kec256(pubKey))

    address.toUnprefixedString
  }

  val api: Command[String] = Command.apply(name = "cli", header = "Mantis CLI") {
    Opts.subcommands(GeneratePrivateKeyCommand, DeriveAddressFromPrivateKey, GenerateAllocs)
  }
}
