package io.iohk.ethereum.ets.vm

import io.iohk.ethereum.ets.vm.TestOptions._
import org.scalatest.ConfigMap

object TestOptions {

  trait MatchMode
  case object Plain extends MatchMode
  case object Glob extends MatchMode
  case object Regex extends MatchMode

  def apply(configMap: ConfigMap): TestOptions = {

    def getSetting(keys: String*): Option[String] =
      keys.foldLeft(None: Option[String]){ _ orElse configMap.get(_).map(_.toString) }

    val matchMode = getSetting("match", "m").getOrElse("glob") match {
      case "glob" => Glob
      case "regex" => Regex
      case "plain" => Plain
    }

    TestOptions(
      getSetting("includeGroups", "ing"),
      getSetting("excludeGroups", "exg"),
      getSetting("includeScenarios", "ins"),
      getSetting("excludeScenarios", "exs"),
      matchMode
    )
  }
}

case class TestOptions(
  includedGroups: Option[String],
  excludedGroups: Option[String],
  includedScenarios: Option[String],
  excludedScenarios: Option[String],
  matchMode: MatchMode
) {

  def isGroupIncluded(name: String): Boolean =
    includedGroups.forall(groupMatches(name)) && !excludedGroups.exists(groupMatches(name))

  def isScenarioIncluded(name: String): Boolean =
    includedScenarios.forall(groupMatches(name)) && !excludedScenarios.exists(groupMatches(name))

  private def groupMatches(name: String)(group: String): Boolean = matchMode match {
    case Plain =>
      group.split(",").toSet.contains(name)

    case Glob =>
      val regexes = group.replaceAll("\\*", ".*").split(",").map(_.r)
      regexes.exists(_.findFirstIn(name).contains(name))

    case Regex =>
      group.r.findFirstIn(name).contains(name)
  }
}
