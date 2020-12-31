package io.iohk.ethereum.cli

import com.monovore.decline._

import scala.collection.immutable.ArraySeq

//scalastyle:off
object CliLauncher extends App {

  private val arguments: Seq[String] = PlatformApp.ambientArgs getOrElse ArraySeq.unsafeWrapArray(args)
  CliCommands.api.map(println).parse(arguments, sys.env) match {
    case Left(help) => System.err.println(help)
    case Right(_) => ()
  }

}
