package io.iohk.ethereum

import io.iohk.ethereum.cli.CliLauncher
import io.iohk.ethereum.crypto.EcKeyGen
import io.iohk.ethereum.crypto.SignatureValidator
import io.iohk.ethereum.extvm.VmServerApp
import io.iohk.ethereum.faucet.Faucet
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Logger

object App extends Logger {

  def main(args: Array[String]): Unit = {

    val launchMantis = "mantis"
    val launchKeytool = "keytool"
    val downloadBootstrap = "bootstrap"
    val vmServer = "vm-server"
    val faucet = "faucet"
    val ecKeyGen = "eckeygen"
    val cli = "cli"
    val sigValidator = "signature-validator"

    args.headOption match {
      case None                  => Mantis.main(args)
      case Some(`launchMantis`)  => Mantis.main(args.tail)
      case Some(`launchKeytool`) => KeyTool.main(args.tail)
      case Some(`downloadBootstrap`) =>
        Config.Db.dataSource match {
          case "rocksdb" => BootstrapDownload.main(args.tail :+ Config.Db.RocksDb.path)
        }
      case Some(`vmServer`)     => VmServerApp.main(args.tail)
      case Some(`faucet`)       => Faucet.main(args.tail)
      case Some(`ecKeyGen`)     => EcKeyGen.main(args.tail)
      case Some(`sigValidator`) => SignatureValidator.main(args.tail)
      case Some(`cli`)          => CliLauncher.main(args.tail)
      case Some(unknown) =>
        log.error(
          s"Unrecognised launcher option $unknown, " +
            s"first parameter must be $launchKeytool, $downloadBootstrap, $launchMantis, " +
            s"$faucet, $vmServer, $ecKeyGen, $sigValidator or $cli"
        )
    }

  }
}
