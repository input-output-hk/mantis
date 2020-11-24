package io.iohk.ethereum.security

import java.io.{File, FileInputStream}

import io.iohk.ethereum.utils.Logger

import scala.io.{BufferedSource, Source}
import scala.util.Try

trait FileUtils extends Logger {

  def exist(pathName: String): Boolean = new File(pathName).isFile

  def createFileInputStream(pathName: String): Either[Throwable, FileInputStream] =
    Try(new FileInputStream(pathName)).toEither match {
      case Right(fileInputStream) =>
        Option(fileInputStream).map(Right(_)).getOrElse {
          log.error("empty fileInputStream")
          Left(new RuntimeException("empty fileInputStream"))
        }
      case Left(error) =>
        log.error("create file input stream failed", error)
        Left(error)
    }

  def getReader(file: String): BufferedSource = Source.fromFile(file)

}
