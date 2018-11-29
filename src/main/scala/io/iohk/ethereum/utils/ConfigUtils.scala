package io.iohk.ethereum.utils

import java.util.Map.Entry

import akka.http.scaladsl.model.headers.{HttpOrigin, HttpOriginRange}
import com.typesafe.config.{ConfigValue, Config => TypesafeConfig}

import scala.collection.JavaConverters._
import scala.util.Try

object ConfigUtils {

  def parseCorsAllowedOrigins(config: TypesafeConfig, key: String): HttpOriginRange = {
    (Try(parseMultipleOrigins(config.getStringList(key).asScala)) recoverWith {
      case _ => Try(parseSingleOrigin(config.getString(key)))
    }).get
  }

  def parseMultipleOrigins(origins: Seq[String]): HttpOriginRange = HttpOriginRange(origins.map(HttpOrigin(_)): _*)

  def parseSingleOrigin(origin: String): HttpOriginRange = origin match {
    case "*" => HttpOriginRange.*
    case s => HttpOriginRange.Default(HttpOrigin(s) :: Nil)
  }

  def keys(config: TypesafeConfig): Set[String] = config.entrySet().asScala.toSet
    .flatMap((entry: Entry[String, ConfigValue]) => entry.getKey.split('.').headOption)

}
