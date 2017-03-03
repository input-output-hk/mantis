package io.iohk.ethereum.vmrunner


import scala.annotation.tailrec

object EvmRunner extends App {

  @tailrec
  def loop(): Unit = {
    Shell.readLine() match {
      case Some(line) =>
        val result = (
          for {
            cmd <- CmdParser(line)
            res <- Interpreter(cmd)
          } yield res
        ).left.map(_.msg).merge

        Shell.printLine(withNewLine(result))

        loop()

      case None =>
    }
  }

  def withNewLine(s: String): String = {
    val rightTrimmed = s.reverse.dropWhile(_.isWhitespace).reverse
    if (rightTrimmed.isEmpty) rightTrimmed else rightTrimmed + "\n"
  }

  loop()
}
