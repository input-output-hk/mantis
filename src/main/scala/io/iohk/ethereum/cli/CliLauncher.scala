package io.iohk.ethereum.cli

import com.monovore.decline._

//scalastyle:off
object CliLauncher extends App {

  CliCommands.api.map(println).parse(PlatformApp.ambientArgs getOrElse args, sys.env) match {
    case Left(help) => System.err.println(help)
    case Right(_) => ()
  }

}
