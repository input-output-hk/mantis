package io.iohk.ethereum.mallet.main

import java.io.File
import java.nio.file.Paths

import io.iohk.ethereum.mallet.common.Constants
import io.iohk.ethereum.mallet.service.PasswordReader
import org.jline.reader.{EndOfFileException, LineReader, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.TerminalBuilder

import scala.annotation.tailrec

class Shell(dataDir: File) extends PasswordReader {

  private val terminal = TerminalBuilder.terminal()

  private val historyFile: String = Paths.get(dataDir.getAbsolutePath, ".history").toString

  private val reader = LineReaderBuilder.builder()
    .terminal(terminal)
    .variable(LineReader.HISTORY_FILE, historyFile)
    .variable(LineReader.HISTORY_FILE_SIZE, "100000")
    .build()


  @tailrec
  final def readLine(): Option[String] = {
    lazy val line = Option(reader.readLine(s"${Constants.AppName}> "))
    try { line } catch {
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
    try { line } catch {
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
