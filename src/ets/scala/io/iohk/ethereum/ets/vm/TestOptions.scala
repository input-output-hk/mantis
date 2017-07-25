package io.iohk.ethereum.ets.vm

import org.scalatest.ConfigMap

object TestOptions {
  def apply(configMap: ConfigMap): TestOptions = {

    def parseNames(keys: String*): Option[Set[String]] = {
      val arg = keys.foldLeft(None: Option[String]){ _ orElse configMap.get(_).map(_.toString) }
      arg.map(_.toString.split(",").toSet)
    }

    TestOptions(
      parseNames("includeGroups", "ing"),
      parseNames("excludeGroups", "exg"),
      parseNames("includeScenarios", "ins"),
      parseNames("excludeScenarios", "exs")
    )

  }
}

case class TestOptions(
  includedGroups: Option[Set[String]],
  excludedGroups: Option[Set[String]],
  includedScenarios: Option[Set[String]],
  excludedScenarios: Option[Set[String]]
) {

  def isGroupIncluded(name: String): Boolean =
    includedGroups.forall(_.contains(name)) && !excludedGroups.exists(_.contains(name))

  def isScenarioIncluded(name: String): Boolean =
    includedScenarios.forall(_.contains(name)) && !excludedScenarios.exists(_.contains(name))
}
