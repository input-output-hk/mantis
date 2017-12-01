package io.iohk.ethereum


object App {

  def main(args: Array[String]): Unit = {

    val launchMantis = "mantis"
    val launchKeytool = "keytool"

      args.headOption match {
        case None => Mantis.main(args)
        case Some(`launchMantis`) => Mantis.main(args.tail)
        case Some(`launchKeytool`) => KeyTool.main(args.tail)
        case Some(unknown) =>
          println(s"Unrecognised launcher option, " +
            s"first parameter must be $launchKeytool or $launchMantis")
      }


  }
}
