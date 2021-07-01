package io.iohk.ethereum.security

import java.io.File
import java.io.FileInputStream

import scala.io.BufferedSource
import scala.io.Source
import scala.util.Try

import io.iohk.ethereum.utils.Logger

trait FileUtils extends Logger {

  def exist(pathName: String): Boolean = new File(pathName).isFile

  def createFileInputStream(pathName: String): Either[Throwable, FileInputStream] =
    Try(new FileInputStream(pathName)).toEither match {
      case Right(fileInputStream) =>
        Option(fileInputStream).map(Right(_)).getOrElse {
          log.error("empty fileInputStream")
          Left(new IllegalStateException("empty fileInputStream"))
        }
      case Left(error) =>
        log.error("create file input stream failed", error)
        Left(error)
    }

  def getReader(file: String): BufferedSource = Source.fromFile(file)

}
