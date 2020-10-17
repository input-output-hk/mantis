package io.iohk.ethereum.mallet.main

import java.nio.file.Paths

import io.iohk.ethereum.mallet.common.Constants
import io.iohk.ethereum.mallet.interpreter.Commands.ImportPrivateKey
import io.iohk.ethereum.mallet.service.PasswordReader
import org.jline.reader.impl.history.DefaultHistory
import org.jline.reader.{EndOfFileException, LineReader, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.TerminalBuilder

import scala.annotation.tailrec
import scala.util.matching.Regex

/** The interactive shell used to read commands and passwords */
class Shell(dataDir: String) extends PasswordReader {

  private val terminal = TerminalBuilder.terminal()

  private val historyFile: String = Paths.get(dataDir, ".history").toString

  /** List of regex patterns to ignore from history */
  private val secretPatterns: List[Regex] = List(
    s"^${ImportPrivateKey.name}".r
  )

  /** [[DefaultHistory]] uses custom and undocumented pattern matching for ignoring input lines
    * so I prefer to rely on standard regexes
    */
  private val history = new DefaultHistory {
    override protected def matchPatterns(_unused: String, line: String): Boolean =
      secretPatterns.exists(_.findFirstIn(line.trim).isDefined)
  }

  private val reader = LineReaderBuilder
    .builder()
    .terminal(terminal)
    .variable(LineReader.HISTORY_FILE, historyFile)
    .variable(LineReader.HISTORY_FILE_SIZE, "100000")
    .history(history)
    .build()

  @tailrec
  final def readLine(): Option[String] = {
    lazy val line = Option(reader.readLine(s"${Constants.AppName}> "))
    try { line }
    catch {
      case eof: EndOfFileException =>
        None

      case int: UserInterruptException =>
        printLine()
        readLine()
    }
  }

  def readPassword(): Option[String] = {
    readSecret("Enter password: ")
  }

  def readPasswordTwice(): Option[String] = {
    val prompt1 = "Enter password: "
    val prompt2 = "Repeat password: "
    readSecret(prompt1).flatMap(p1 => readSecret(prompt2).map(p2 => (p1, p2))) match {
      case Some((p1, p2)) if p1 == p2 =>
        Some(p1)

      case Some(_) =>
        printLine("Passwords did not match")
        None

      case None =>
        None
    }
  }

  private def readSecret(prompt: String): Option[String] = {
    val line = Option(reader.readLine(prompt, '\u0000'))
    try { line }
    catch {
      case eof: EndOfFileException =>
        None

      case int: UserInterruptException =>
        None
    }
  }

  def printLine(x: Any): Unit = {
    //scalastyle:off regex
    println(x)
  }

  def printLine(): Unit =
    printLine("")
}
