package io.iohk.ethereum.snappy

import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}
import io.iohk.ethereum.snappy.Config.{DualDB, SingleDB, TestMode}

import scala.util.Try

object Config {
  def apply(): Config = new Config(ConfigFactory.load())

  sealed trait TestMode
  case object SingleDB extends TestMode
  case object DualDB extends TestMode
}

class Config(rootConfig: TypesafeConfig) {

  private val snappyConf = rootConfig.getConfig("snappy")

  val mode: TestMode = snappyConf.getString("test-mode").toLowerCase match {
    case "single-db" => SingleDB
    case "dual-db" => DualDB
  }

  val sourceDbPath: String = snappyConf.getString("source-db-path")
  lazy val targetDbPath: String = snappyConf.getString("target-db-path")
  val startBlock: Option[BigInt] = Try(snappyConf.getString("start-block")).toOption.map(BigInt(_))
  val targetBlock: Option[BigInt] = Try(snappyConf.getString("target-block")).toOption.map(BigInt(_))
}
