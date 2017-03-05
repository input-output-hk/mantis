package io.iohk.ethereum.vmrunner

import java.io.File

import scala.io.Source

import akka.util.ByteString
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.Error

object Utils {

  def loadContractCodeFromFile(file: File): ByteString = {
    val raw = Source.fromFile(file).mkString
    ByteString(raw.trim.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)
  }

  def loadContractAbiFromFile(file: File): Either[Error, List[ABI]] = {
    val raw = Source.fromFile(file).mkString
    implicit val config = Configuration.default.withDefaults
    decode[List[ABI]](raw)
  }

}
