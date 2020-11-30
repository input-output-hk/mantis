package io.iohk.ethereum.security

import java.security.SecureRandom

import com.typesafe.config.{Config, ConfigFactory}
import io.iohk.ethereum.utils.Logger

import scala.util.{Failure, Success, Try}

trait SecureRandomBuilder extends Logger {

  private lazy val rawMantisConfig: Config = ConfigFactory.load().getConfig("mantis")

  private val secureRandomAlgo: Option[String] =
    if (rawMantisConfig.hasPath("secure-random-algo")) Some(rawMantisConfig.getString("secure-random-algo"))
    else None

  lazy val secureRandom: SecureRandom =
    secureRandomAlgo
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
