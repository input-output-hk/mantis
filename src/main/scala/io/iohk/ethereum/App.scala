package io.iohk.ethereum

import io.iohk.ethereum.utils.Logger


object App extends Logger {

  def main(args: Array[String]): Unit = {

    val launchMantis = "mantis"
    val launchKeytool = "keytool"

      args.headOption match {
        case None => Mantis.main(args)
        case Some(`launchMantis`) => Mantis.main(args.tail)
        case Some(`launchKeytool`) => KeyTool.main(args.tail)
        case Some(unknown) =>
          log.error(s"Unrecognised launcher option, " +
            s"first parameter must be $launchKeytool or $launchMantis")
      }


  }
}
