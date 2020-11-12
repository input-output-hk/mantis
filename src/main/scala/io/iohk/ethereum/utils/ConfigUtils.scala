package io.iohk.ethereum.utils

import akka.http.scaladsl.model.headers.HttpOrigin
import ch.megard.akka.http.cors.scaladsl.model.HttpOriginMatcher
import com.typesafe.config.{ConfigValue, Config => TypesafeConfig}
import java.util.Map.Entry
import scala.collection.JavaConverters._
import scala.util.Try

object ConfigUtils {

  def parseCorsAllowedOrigins(config: TypesafeConfig, key: String): HttpOriginMatcher = {
    (Try(parseMultipleOrigins(config.getStringList(key).asScala)) recoverWith { case _ =>
      Try(parseSingleOrigin(config.getString(key)))
    }).get
  }

  def parseMultipleOrigins(origins: Seq[String]): HttpOriginMatcher = HttpOriginMatcher(origins.map(HttpOrigin(_)): _*)

  def parseSingleOrigin(origin: String): HttpOriginMatcher = origin match {
    case "*" => HttpOriginMatcher.*
    case s => HttpOriginMatcher.Default(HttpOrigin(s) :: Nil)
  }

  def getOptionalValue[V](config: TypesafeConfig, path: String, getter: TypesafeConfig => String => V): Option[V] =
    if (config.hasPath(path)) Some(getter(config)(path))
    else None

  def keys(config: TypesafeConfig): Set[String] = config
    .entrySet()
    .asScala
    .toSet
    .flatMap((entry: Entry[String, ConfigValue]) => entry.getKey.split('.').headOption)

}
