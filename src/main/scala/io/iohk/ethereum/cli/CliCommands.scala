package io.iohk.ethereum.cli

import cats.implicits._
import cats.data.NonEmptyList
import com.monovore.decline.{Command, Opts}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.utils.ByteStringUtils
import java.security.SecureRandom
import org.bouncycastle.util.encoders.Hex

object CliCommands {

  val generatePrivateKeyCommand = "generate-private-key"
  val deriveAddressCommand = "derive-address"
  val generateAllocsCommand = "generate-allocs"
  val useAddressesFlag = "useAddresses"
  val balanceOption = "balance"

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

      val useAddressesOpt: Opts[Boolean] = Opts
        .flag(long = useAddressesFlag, help = "Use addresses instead of private keys")
        .orFalse

      val keysOpt: Opts[NonEmptyList[String]] = Opts.arguments[String]("key")

      val balanceOpt =
        Opts.option[BigInt](long = balanceOption, help = "Initial balance for account", metavar = "balance")

      val addresesOpt = (useAddressesOpt, keysOpt).mapN {
        case (false, keys) => keys.map(key => privKeyToAddress(Hex.decode(key)))
        case (true, keys) => keys
      }

      (addresesOpt, balanceOpt).mapN { (addresses, balance) =>
        allocs(addresses.toList, balance)
      }
    }

  private def allocs(addresses: List[String], balance: BigInt): String =
    s""""alloc": ${addresses
      .map(address => s"""$address: { "balance": $balance }""")
      .mkString("{", ", ", "}")}"""

  private def privKeyToAddress(privKey: Array[Byte]): String = {
    val pubKey = pubKeyFromPrvKey(privKey)
    val address = crypto.kec256(pubKey)
    Hex.toHexString(address)
  }

  val api: Command[String] = Command.apply(name = "mantis-cli", header = "Mantis CLI") {
    Opts.subcommands(GeneratePrivateKeyCommand, DeriveAddressFromPrivateKey, GenerateAllocs)
  }
}
