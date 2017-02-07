package io.iohk.ethereum.vmrunner


import scala.annotation.tailrec

// scalastyle:off magic.number
object EvmRunner extends App {

  @tailrec
  def loop(): Unit = {
    Shell.readLine() match {
      case Some(line) =>
        val result = (
          for {
            cmd <- CmdParser(line)
            result <- Interpreter(cmd)
          } yield result
        ).left.map(_.msg).merge

        println(result)
        loop()

      case None =>
    }
  }

  loop()
}
