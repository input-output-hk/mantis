package io.iohk.ethereum.vmrunner

import java.nio.file.{Path, Paths}

import org.jline.reader.{EndOfFileException, LineReader, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.TerminalBuilder

import scala.annotation.tailrec

object Shell {
  val terminal = TerminalBuilder.terminal()

  val reader = LineReaderBuilder.builder()
    .terminal(terminal)
    .variable(LineReader.HISTORY_FILE, Paths.get(System.getProperty("user.dir"), ".evm-runner_history").toString)
    .variable(LineReader.HISTORY_FILE_SIZE, "10000")
    .build()


  @tailrec
  def readLine(): Option[String] = {
    lazy val line = Option(reader.readLine("evm-runner> "))
    try { line } catch {
      case eof: EndOfFileException =>
        None

      case int: UserInterruptException =>
        println()
        readLine()
    }
  }
}
