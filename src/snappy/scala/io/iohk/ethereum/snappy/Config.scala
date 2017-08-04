package io.iohk.ethereum.snappy

import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}

import scala.util.Try

class Config(rootConfig: TypesafeConfig = ConfigFactory.load()) {

  private val snappyConf = rootConfig.getConfig("snappy")

  val sourceDbPath: String = snappyConf.getString("source-db-path")
  val targetDbPath: String = snappyConf.getString("target-db-path")
  val targetBlock: Option[BigInt] = Try(snappyConf.getString("target-block")).toOption.map(BigInt(_))
}
