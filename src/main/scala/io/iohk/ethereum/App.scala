package io.iohk.ethereum

import io.iohk.ethereum.utils.{Config, Logger}


object App extends Logger {

  def main(args: Array[String]): Unit = {

    val launchMantis = "mantis"
    val launchKeytool = "keytool"
    val downloadBootstrap = "bootstrap"

      args.headOption match {
        case None => Mantis.main(args)
        case Some(`launchMantis`) => Mantis.main(args.tail)
        case Some(`launchKeytool`) => KeyTool.main(args.tail)
        case Some(`downloadBootstrap`) => BootstrapDownload.main(args.tail :+ Config.Db.LevelDb.path)
        case Some(unknown) =>
          log.error(s"Unrecognised launcher option, " +
            s"first parameter must be $launchKeytool, $downloadBootstrap or $launchMantis")
      }


  }
}
