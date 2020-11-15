package io.iohk.ethereum.jsonrpc.security

import java.security.SecureRandom

import io.iohk.ethereum.utils.{Config, Logger}

import scala.util.{Failure, Success, Try}

trait SecureRandomBuilder extends Logger {

  //TODO: work with this config...
  lazy val secureRandom: SecureRandom =
    Config.secureRandomAlgo
      .flatMap(name =>
        Try(SecureRandom.getInstance(name)) match {
          case Failure(exception) =>
            log.error(
              s"Couldn't create SecureRandom instance using algorithm $name. Falling-back to default one",
              exception
            )
            None
          case Success(value) =>
            Some(value)
        }
      )
      .getOrElse(new SecureRandom())
}
